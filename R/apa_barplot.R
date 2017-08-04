#' Barplots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more barplots from a data.frame containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#'
#' @examples
#'
#' apa_barplot(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N")
#' )
#'
#' apa_barplot(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N", "P")
#' )
#'
#' apa_barplot(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N", "P", "K")
#'    , ylim = c(0, 80)
#'    , level = .34
#'    , las = 1
#' )
#'
#' @import grDevices
#' @import graphics
#' @rdname apa_barplot
#' @family plots for factorial designs
#' @export

apa_barplot <- function(data, ...){
  UseMethod("apa_barplot", data)
}

#' @rdname apa_barplot
#' @export

apa_barplot.default <- function(
  data
  , id
  , factors = NULL
  , dv
  , tendency = mean
  , dispersion = conf_int
  , level = 0.95
  , fun_aggregate = mean
  , na.rm = TRUE
  , reference = 0
  , intercept = NULL
  , args_arrows = NULL
  , args_legend = NULL
  , ...
){
  ellipsis <- defaults(
    list(...)
    , set.if.null = list(
      data = data
      , id = id
      , factors = factors
      , dv = dv
      , tendency = substitute(tendency)
      , dispersion = substitute(dispersion)
      , level = level
      , fun_aggregate = substitute(fun_aggregate)
      , na.rm = na.rm
      , reference = reference
      , intercept = intercept
      , args_arrows = args_arrows
      , args_legend = args_legend
      , plot = c("bars", "error_bars")
    )
  )
  do.call("apa_generic_plot", ellipsis)
}


#' @rdname apa_barplot
#' @export

apa_barplot.afex_aov <- function(
  data
  , tendency = mean
  , dispersion = conf_int
  , fun_aggregate = mean
  , ...
){

  ellipsis <- list(...)

  ellipsis <- defaults(
    ellipsis
    , set = list(
      "data" = data
      , "plot" = c("bars", "error_bars")
      , "tendency" = substitute(tendency)
      , "dispersion" = substitute(dispersion)
      , "fun_aggregate" = substitute(fun_aggregate)
    )
  )
  do.call("apa_generic_plot.afex_aov", ellipsis)
}
