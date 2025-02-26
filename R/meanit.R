#' meanit - a function inspired by SAS PROC MEANS
#' --requires dplyr, rlang, tibble, and tidyr
#'
#' @param df a data frame or tibble
#' @param anvar a required outcome variable to analyze
#' @param ... zero or more variables to cross-classify analysis by
#' @param where expression. an optional where clause - defaults to `NULL`
#' @param missincl logical. If `TRUE` (the default), frequency cells can use `NA`
#' values from one or more cell-defining variables. If `FALSE`, records with `NA`
#' for any variable will be listwise deleted from results.
#' @param digits integerish. number of digits after the decimal for percentages
#' @param center either of "mean" (default) or "median"
#'
#' @return a data frame
#' @export
#'
#' @examples
#' meanit(mtcars,mpg)
#' meanit(mtcars,mpg,cyl,gear)
#' meanit(mtcars,c(mpg,hp),cyl,gear)
#' meanit(mtcars,mpg,cyl,where=hp>100)
#' meanit(mtcars,hp,cyl,center="median")
#' meanit(mtcars,hp,cyl,digits=3)
#'
meanit<-function(df,anvar,...,where=NULL,missincl=TRUE,digits=1,center=c("mean","median")){

  pd<-requireNamespace("dplyr",quietly=TRUE)
  pr<-requireNamespace("rlang",quietly=TRUE)
  pt<-requireNamespace("tibble",quietly=TRUE)
  pi<-requireNamespace("tidyr",quietly=TRUE)

  if (pd==FALSE){

    stop("dplyr package required but not installed")

  } else if (pr==FALSE){

    stop("rlang package required but not installed")

  } else if (pt==FALSE){

    stop("tibble package required but not installed")

  } else if (pi==FALSE){

    stop("tidyr package required but not installed")

  }  else if (!(is.data.frame(df)|tibble::is_tibble(df))) {

    stop("`df` must be a tibble or data frame")

  } else if (missing(anvar)) {

    stop("`anvar` is missing with no default")

  } else if (!rlang::is_logical(missincl)) {

    stop("`missincl` must be TRUE or FALSE")

  } else if (!rlang::is_integerish(digits)) {

    stop("`digits` must be a whole number")

  } else {

    rlang::arg_match(center)

    center<-center[1]

    if (missing(where)) where=rlang::expr(TRUE)

    vars<-rlang::enquos(...)

    if (missincl==F){

      missexp=rlang::expr(!dplyr::if_any(.cols=c(!!!vars),.fn=~is.na(.x)))

    } else missexp=rlang::expr(TRUE)

    if (center=="mean"){

      c.select=rlang::expr(c(ends_with("_mean"),ends_with("_sd")))

    } else if (center=="median"){

      c.select=rlang::expr(c(ends_with("_q25"),ends_with("_median"),ends_with("_q75")))

    }

    df |>
      dplyr::ungroup() |>
      dplyr::filter({{where}} & eval(missexp)) |>
      dplyr::mutate(N=dplyr::n()) |>
      dplyr::group_by(...) |>
      dplyr::summarize(
        .groups="drop"
        ,n=formatC(dplyr::n(),digits=0,format="f",big.mark=',',width=8)
        ,prepct=100*dplyr::n()/mean(N)
        ,pren=dplyr::n()
        ,dplyr::across(
          .cols={{anvar}}
          ,.fns=list(
            nomiss=~formatC(sum(!is.na(.x)),big.mark=',',format="f",digits=0,width=10)
            ,nmiss=~formatC(sum(is.na(.x)),big.mark=',',format="f",digits=0,width=10)
            ,min=~formatC(ifelse(sum(!is.na(.x))>0,min(.x,na.rm=T),NA_real_),big.mark=',',format="f",digits=.env$digits,width=10)
            ,q25=~formatC(ifelse(sum(!is.na(.x))>0,quantile(.x,0.25,na.rm=T),NA_real_),big.mark=',',format="f",digits=.env$digits,width=10)
            ,median=~formatC(ifelse(sum(!is.na(.x))>0,quantile(.x,0.50,na.rm=T),NA_real_),big.mark=',',format="f",digits=.env$digits,width=10)
            ,mean=~formatC(ifelse(sum(!is.na(.x))>0,mean(.x,na.rm=T),NA_real_),big.mark=',',format="f",digits=.env$digits,width=10)
            ,sd=~formatC(ifelse(sum(!is.na(.x))>0,sd(.x,na.rm=T),NA_real_),big.mark=',',format="f",digits=.env$digits,width=10)
            ,q75=~formatC(ifelse(sum(!is.na(.x))>0,quantile(.x,0.75,na.rm=T),NA_real_),big.mark=',',format="f",digits=.env$digits,width=10)
            ,max=~formatC(ifelse(sum(!is.na(.x))>0,max(.x,na.rm=T),NA_real_),big.mark=',',format="f",digits=.env$digits,width=10)
          )
        )
      ) |>
      dplyr::mutate(
        pct=formatC(prepct,format="f",digits=.env$digits,width=10)
        ,cumn=formatC(cumsum(pren),digits=0,format="f",big.mark=',',width=8)
        ,cumpct=formatC(cumsum(prepct),format="f",digits=.env$digits,width=10)
      ) |>
      dplyr::select(...,n,pct,cumn,cumpct,ends_with("_nomiss"),ends_with("_nmiss"),ends_with("_min"),eval(c.select),ends_with("_max")) |>
      tidyr::pivot_longer(
        -c(!!!vars,n,pct,cumn,cumpct)
        ,names_to=c("variable",".value")
        ,names_pattern="^(.+)_(nomiss|nmiss|min|q25|mean|sd|median|q75|max)$"
      ) |>
      dplyr::arrange(variable,...) |>
      dplyr::select(variable,...,everything()) |>
      as.data.frame()

  }
}
