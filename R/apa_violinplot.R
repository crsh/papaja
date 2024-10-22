#' Violin Plots for Factorial Designs that Conform to APA Guidelines
#'
#' Creates one or more violin plots from a `data.frame` containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#' @inherit apa_factorial_plot
#' @param ... Further arguments passed on to [apa_factorial_plot()].
#'
#' @family plots for factorial designs
#' @examples
#' apa_violinplot(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N")
#' )
#'
#' apa_violinplot(
#'   data = npk
#'   , id = "block"
#'   , dv = "yield"
#'   , factors = c("N", "P")
#'   , args_legend = list(x = "center")
#'   , jit = 0.1
#' )
#'
#'
#' @import grDevices
#' @import graphics
#' @rdname apa_violinplot
#' @export

apa_violinplot <- function(data, ...){
  UseMethod("apa_violinplot", data)
}



#' @rdname apa_violinplot
#' @export

apa_violinplot.default <- function(
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
  , args_lines = NULL
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
      , args_lines = args_lines
      , args_error_bars = args_error_bars
      , args_legend = args_legend
      , jit = jit
      , xlab = xlab
      , ylab = ylab
      , main = main
      , plot = c("points", "violins", "error_bars")
    )
  )
  do.call("apa_factorial_plot", ellipsis)
}

#' @rdname apa_violinplot
#' @export

apa_violinplot.afex_aov <- function(
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
      data = data
      , plot = c("points", "violins", "error_bars")
      , tendency = substitute(tendency)
      , dispersion = substitute(dispersion)
      , fun_aggregate = substitute(fun_aggregate)
    )
  )
  do.call("apa_factorial_plot.afex_aov", ellipsis)
}
