#' A function for SAS PROC FREQ-like frequency tables
#'
#' @param df a data frame or tibble
#' @param ... zero or more variables to cross for frequency cells
#' @param where expression. an optional where clause - defaults to NULL
#' @param missincl logical. If TRUE (the default), frequency cells can use NA
#' values from one or more cell-defining variables. If FALSE, records with NA
#' for any variable will be listwise deleted from results.
#' @param digits integerish. number of digits after the decimal for percentages
#' @param fancy logical. If TRUE (the default) and knitr package installed,
#' formats table using knitr::kable().
#' @param format string. Passed to format parameter of knitr::kable(). Valid
#' options are 'pipe' (default), 'html', 'latex', 'simple', 'rst', 'jira', and
#' 'org'.
#'
#' @importFrom rlang .data .env
#' @return a data frame
#' @export
#'
#' @examples
#' freqy(mtcars)
#' freqy(mtcars,cyl)
#' freqy(mtcars,cyl,gear)
#' freqy(mtcars,cyl,gear,where=hp>100)
#' freqy(mtcars,cyl,gear,digits=3)
#' freqy(mtcars,cyl,gear,fancy=FALSE)
#' freqy(mtcars,cyl,gear,format="html")
#' freqy(mtcars,cyl,gear,format="simple")
#'
freqy<-function(
    df
    ,...
    ,where=NULL
    ,missincl=TRUE
    ,digits=1
    ,fancy=TRUE
    ,format=c(
      "pipe"
      ,"simple"
      ,"html"
      ,"latex"
      ,"rst"
      ,"jira"
      ,"org"
    )
  ){

  pd<-requireNamespace("dplyr",quietly=TRUE)
  pr<-requireNamespace("rlang",quietly=TRUE)
  pt<-requireNamespace("tibble",quietly=TRUE)
  pk<-requireNamespace("knitr",quietly=TRUE)

  if (pd==FALSE){

    stop("dplyr package required but not installed")

  } else if (pr==FALSE){

    stop("rlang package required but not installed")

  } else if (pt==FALSE){

    stop("tibble package required but not installed")

  } else if (!(is.data.frame(df)|tibble::is_tibble(df))) {

    stop("`df` must be a tibble or data frame")

  } else if (!rlang::is_logical(missincl)) {

    stop("`missincl` must be TRUE or FALSE")

  } else if (!rlang::is_integerish(digits)) {

    stop("`digits` must be a whole number")

  } else {

    if (missing(where)) where=rlang::expr(TRUE)

    vars<-rlang::enquos(...)

    if (missincl==F){

      missexp=rlang::expr(!dplyr::if_any(.cols=c(!!!vars),.fn=~is.na(.x)))

    } else missexp=rlang::expr(TRUE)

    table<-df |>
      dplyr::ungroup() |>
      dplyr::filter({{where}} & eval(missexp)) |>
      dplyr::mutate(N=dplyr::n()) |>
      dplyr::group_by(...) |>
      dplyr::summarize(
        .groups="drop"
        ,n=formatC(dplyr::n(),big.mark=',',format="f",digits=0)
        ,prepct=100*dplyr::n()/mean(.data$N)
        ,pren=dplyr::n()
      ) |>
      dplyr::mutate(
        pct=formatC(.data$prepct,format="f",digits=.env$digits)
        ,cumn=formatC(cumsum(.data$pren),big.mark=',',format="f",digits=0)
        ,cumpct=formatC(cumsum(.data$prepct),format="f",digits=.env$digits)
      ) |>
      dplyr::select(-.data$pren,-.data$prepct) |>
      as.data.frame()

    if (fancy & pk) {

      rlang::arg_match(format)

      format<-format[1]

      return(knitr::kable(table,format=format,align='r'))

    } else if (fancy){

      message("knitr package required for fancy tables")

      return(table)

    } else return(table)

  }
}
