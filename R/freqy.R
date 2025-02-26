#' freqy - a function for SAS PROC FREQ-like frequency tables
#' --requires dplyr, rlang, and tibble
#'
#' @param df a data frame or tibble
#' @param ... zero or more variables to cross for frequency cells
#' @param where an optional where clause
#' @param missincl TRUE/FALSE should include records with NA in one or more vars?
#' @param digits integerish number of digits after the decimal for percentages
#'
#' @return a data frame
#' @export
#'
#' @examples
#' freqy(mtcars)
#' freqy(mtcars,cyl)
#' freqy(mtcars,cyl,gear)
#' freqy(mtcars,cyl,gear,where=hp>100)
#' freqy(mtcars,cyl,gear,digits=3)
#'
freqy<-function(df,...,where=NULL,missincl=TRUE,digits=1){

  pd<-require(dplyr,quietly=TRUE,warn.conflicts=FALSE)
  pr<-require(rlang,quietly=TRUE,warn.conflicts=FALSE)
  pt<-require(tibble,quietly=TRUE,warn.conflicts=FALSE)

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

    if (missing(where)) where=expr(TRUE)

    vars<-enquos(...)

    if (missincl==F){

      missexp=expr(!if_any(.cols=c(!!!vars),.fn=~is.na(.x)))

    } else missexp=expr(TRUE)

    df %>%
      dplyr::ungroup() %>%
      dplyr::filter({{where}} & eval(missexp)) %>%
      dplyr::mutate(N=n()) %>%
      dplyr::group_by(...) %>%
      dplyr::summarize(
        .groups="drop"
        ,n=formatC(n(),big.mark=',',format="f",width=8,digits=0)
        ,prepct=100*n()/mean(N)
        ,pren=n()
      ) %>%
      dplyr::mutate(
        pct=formatC(prepct,format="f",digits=.env$digits,width=10)
        ,cumn=formatC(cumsum(pren),big.mark=',',format="f",width=8,digits=0)
        ,cumpct=formatC(cumsum(prepct),format="f",digits=.env$digits,width=10)
      ) %>%
      dplyr::select(-pren,-prepct) %>%
      as.data.frame()

  }
}
