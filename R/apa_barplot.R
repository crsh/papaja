#' Barplots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more barplots from a \code{data.frame} containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#' @inherit apa_factorial_plot
#' @inheritDotParams apa_factorial_plot
#' @family plots for factorial designs
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
  , use = "all.obs"
  , reference = 0
  , intercept = NULL
  , args_x_axis = NULL
  , args_y_axis = NULL
  , args_title = NULL
  , args_rect = NULL
  , args_error_bars = NULL
  , args_legend = NULL
  , xlab = NULL
  , ylab = NULL
  , main = NULL
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
      , use = use
      , reference = reference
      , intercept = intercept
      , args_x_axis = args_x_axis
      , args_y_axis = args_y_axis
      , args_title = args_title
      , args_rect = args_rect
      , args_error_bars = args_error_bars
      , args_legend = args_legend
      , xlab = xlab
      , ylab = ylab
      , main = main
      , jit = .4  # add parameter 'space'
      , plot = c("bars", "error_bars")
    )
  )
  do.call("apa_factorial_plot", ellipsis)
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
  do.call("apa_factorial_plot.afex_aov", ellipsis)
}
