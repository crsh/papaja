#' Beeswarm plots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more beeswarm plots from a \code{data.frame} containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#' @inherit apa_factorial_plot
#' @inheritDotParams apa_factorial_plot
#' @family plots for factorial designs
#' @examples
#' apa_beeplot(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N")
#' )
#'
#' apa_beeplot(
#'   data = npk
#'  , id = "block"
#'  , dv = "yield"
#'  , factors = c("N", "P")
#'  , args.legend = list(x = "center")
#' )
#'
#' apa_beeplot(
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
#' @rdname apa_beeplot
#' @export

apa_beeplot <- function(data, ...) {
  UseMethod("apa_beeplot", data)
}


#' @rdname apa_beeplot
#' @export

apa_beeplot.default <- function(
  data
  , id
  , factors = NULL
  , dv
  , tendency = mean
  , dispersion = conf_int
  , level = 0.95
  , fun_aggregate = mean
  , na.rm = TRUE
  , use = "all.obs"
  , intercept = NULL
  , args_x_axis = NULL
  , args_y_axis = NULL
  , args_title = NULL
  , args_points = NULL
  , args_swarm = NULL
  , args_error_bars = NULL
  , args_legend = NULL
  , jit = .3
  , xlab = NULL
  , ylab = NULL
  , main = NULL
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
      , use = use
      , intercept = intercept
      , args_x_axis = args_x_axis
      , args_y_axis = args_y_axis
      , args_points = args_points
      , args_swarm = args_swarm
      , args_error_bars = args_error_bars
      , args_legend = args_legend
      , jit = jit
      , xlab = xlab
      , ylab = ylab
      , main = main
      , plot = c("points", "error_bars", "swarms")
    )
  )
  do.call("apa_factorial_plot", ellipsis)
}


#' @rdname apa_beeplot
#' @export

apa_beeplot.afex_aov <- function(
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
      , "plot" = c("swarms", "error_bars", "points")
      , "tendency" = substitute(tendency)
      , "dispersion" = substitute(dispersion)
      , "fun_aggregate" = substitute(fun_aggregate)
    )
  )
  do.call("apa_factorial_plot.afex_aov", ellipsis)
}
