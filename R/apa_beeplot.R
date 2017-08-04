#' Beeswarm plots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more beeswarm plots from a data.frame containing data from
#' a factorial design and sets APA-friendly defaults.
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
  , intercept = NULL
  , args_axis = NULL
  , args_points = NULL
  , args_swarm = NULL
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
      , args_swarm = args_swarm
      , args_lines = args_lines
      , args_arrows = args_arrows
      , args_legend = args_legend
      , plot = c("points", "error_bars", "swarms")
    )
  )
  do.call("apa_generic_plot", ellipsis)
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

  args <- attributes(data)

  ellipsis <- defaults(
    ellipsis
    , set = list(
      "data" = data$data$long
      , "id" = args$id
      , "dv" = args$dv
      , "factors" = c(args$between, args$within)
      , "tendency" = substitute(tendency)
      , "dispersion" = substitute(dispersion)
      , "fun_aggregate" = substitute(fun_aggregate)
    )
  )
  do.call("apa_beeplot.default", ellipsis)
}
