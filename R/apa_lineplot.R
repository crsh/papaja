#' Lineplots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more lineplots from a data.frame containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#' @family plots for factorial designs
#' @examples
#' apa_lineplot(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N")
#' )
#'
#' apa_lineplot(
#' data = npk
#'  , id = "block"
#'  , dv = "yield"
#'  , factors = c("N", "P")
#'  , args.legend = list(x = "center")
#'  , jit = 0
#' )
#'
#' apa_lineplot(
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
#' @rdname apa_lineplot
#' @export

apa_lineplot <- function(data, ...){
  UseMethod("apa_lineplot", data)
}






#' @rdname apa_lineplot
#' @export

apa_lineplot.default <- function(
  data
  , id
  , factors = NULL
  , dv
  , tendency = mean
  , dispersion = conf_int
  , level = 0.95
  , fun_aggregate = mean
  , na.rm = TRUE
  , intercept = NULL
  , args_axis = NULL
  , args_points = NULL
  , args_lines = NULL
  , args_arrows = NULL
  , args_legend = NULL
  , ...
){
  ellipsis <- defaults(
    list(...)
    , set.if.null =list(
      data = data
      , id = id
      , factors = factors
      , dv = dv
      , tendency = substitute(tendency)
      , dispersion = substitute(dispersion)
      , level = level
      , fun_aggregate = substitute(fun_aggregate)
      , na.rm = na.rm
      , intercept = intercept
      , args_axis = args_axis
      , args_points = args_points
      , args_lines = args_lines
      , args_arrows = args_arrows
      , args_legend = args_legend
      , plot = c("points", "error_bars", "lines")
    )
  )
  do.call("apa_generic_plot", ellipsis)
}

#' @rdname apa_lineplot
#' @export

apa_lineplot.afex_aov <- function(
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
  do.call("apa_lineplot.default", ellipsis)
}



