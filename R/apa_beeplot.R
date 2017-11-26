#' Beeswarm plots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more beeswarm plots from a \code{data.frame} containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#' @param data A \code{data.frame} that contains the data or an object of class \code{afex_aov}
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of up to four variable names that is used to stratify the data.
#' @param dv Character. The name of the dependent variable.
#' @param tendency Closure. A function that will be used as measure of central tendency.
#' @param dispersion Closure. A function that will be used to construct error bars (i.e., whiskers). Defaults to
#'    \code{conf_int} for 95\% between-subjects confidence intervals. See details for more options, especially for within-subjects confidence intervals.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to \code{0.95}.
#'    for 95\% confidence intervals. Ignored if \code{dispersion} is not a confidence-interval function. See details.
#' @param fun_aggregate Closure. The function that will be used to aggregate observations within subjects and factors
#'    before calculating descriptive statistics for each cell of the design. Defaults to \code{mean}.
#' @param na.rm Logical. Specifies if missing values are removed. Defaults to \code{TRUE}.
#' @param reference Numeric. A reference point that determines the \emph{y} coordinate of the \emph{x} axis. Useful if there exists a 'nil' value; defaults to\code{0}.
#' @param intercept Numeric. Adds a horizontal line at height \code{intercept} to the plot. Can be either a single value or a matrix. For the matrix
#'    case, multiple lines are drawn, where the dimensions of the matrix determine the number of lines to be drawn.
#' @param jit Numeric. Determines the amount of horizontal displacement. Defaults to \code{0.3}.
#' @param args_x_axis An optional \code{list} that contains further arguments that may be passed to \code{\link{axis}} for customising the \emph{x} axis.
#' @param args_y_axis An optional \code{list} that contains further arguments that may be passed to \code{\link{axis}} for customising the \emph{y} axis.
#' @param args_title  An optional \code{list} that contains further arguments that may be passed to \code{\link{title}}.
#' @param args_points An optional \code{list} that contains further arguments that may be passed to \code{\link{points}}.
#' @param args_swarm An optional \code{list} that contains further arguments to customize the \code{\link{points}} of the beeswarm.
#' @param args_error_bars An optional \code{list} that contains further arguments that may be passed to \code{\link{arrows}}.
#' @param args_legend An optional \code{list} that contains further arguments that may be passed to \code{\link{legend}}
#' @param xlab Character or expression. Label for \emph{x} axis.
#' @param ylab Character or expression. Label for \emph{y} axis.
#' @param main Character or expression. For up to two factors, simply specify the main title. If you stratify the data by more than two factors,
#' either specify a single value that will be added to automatically generated main title, \emph{or} specify an array of multiple titles, one for each plot area.
#' @param ... Further arguments that will be passed to \code{\link{plot.window}}.
#' @details
#'    The measure of dispersion can be either \code{conf_int} for between-subjects confidence intervals, \code{se} for standard errors,
#'    or any other standard function. For within-subjects confidence intervals, specify \code{wsci} or \code{within_subjects_conf_int}.
#'
#'    If between- or within-subjects confidence intervals are requested, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98\% confidence interval, specify
#'    \code{level = 0.98}. \code{level} defaults to 0.95.
#'
#'    For more details on customising plot elements, see \code{\link{apa_factorial_plot}}.
#'
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
  , reference = 0
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
