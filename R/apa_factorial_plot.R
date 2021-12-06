#' Plots for Factorial Designs that Conform to APA Guidelines
#'
#' Create one or more plots by sequentially calling functions from the \pkg{graphics} package.
#' `apa_factorial_plot()` is the workhorse function that is called by
#' [apa_barplot()], [apa_beeplot()], and [apa_lineplot()].
#'
#' @param data A `data.frame` that contains the data, or an object of class `afex_aov`.
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of up to four variable names that is used to stratify the data.
#' @param dv Character. The name of the dependent variable.
#' @param tendency Closure. A function that will be used as measure of central tendency.
#' @param dispersion Closure. A function that will be used to construct error bars (i.e., whiskers). Defaults to
#'    [conf_int()] for 95% between-subjects confidence intervals. See details for more options, especially for within-subjects confidence intervals.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to `0.95`.
#'    for 95% confidence intervals. Ignored if `dispersion` is not a confidence-interval function. See details.
#' @param fun_aggregate Closure. The function that will be used to aggregate observations within subjects and factors
#'    before calculating descriptive statistics for each cell of the design. Defaults to `mean`.
#' @param na.rm Logical. Specifies if missing values are removed. Defaults to `TRUE`.
#' @param use Character. Specifies a method to exclude cases if there are missing values *after* aggregating.
#'    Possible options are `"all.obs"` or `"complete.obs"`.
#' @param reference Numeric. A reference point that determines the *y* coordinate of the *x* axis. Useful if there exists a 'nil' value; defaults to `0`.
#' @param intercept Numeric. Adds a horizontal line at height `intercept` to the plot. Can be either a single value or a matrix. For the matrix
#'    case, multiple lines are drawn, where the dimensions of the matrix determine the number of lines to be drawn.
#' @param plot Character. A vector specifying which elements of the plot should be plotted. Available options are
#'  `c("points", "error_bars", "bars", "swarms", "lines")`.
#' @param jit Numeric. Determines the amount of horizontal displacement. Defaults to `0.3`, defaults to `0.4` if `plot = "bars"`.
#' @param args_x_axis An optional `list` that contains further arguments that may be passed to [axis()] for customizing the *x* axis.
#' @param args_y_axis An optional `list` that contains further arguments that may be passed to [axis()] for customizing the *y* axis.
#' @param args_title  An optional `list` that contains further arguments that may be passed to [title()].
#' @param args_rect An optional `list` that contains further arguments that may be passed to [rect()].
#' @param args_points An optional `list` that contains further arguments that may be passed to [points()].
#' @param args_lines An optional `list` that contains further arguments that may be passed to [lines()].
#' @param args_swarm An optional `list` that contains further arguments to customize the [points()] of the beeswarm.
#' @param args_error_bars An optional `list` that contains further arguments that may be passed to [arrows()].
#' @param args_legend An optional `list` that contains further arguments that may be passed to [legend()]
#' @param xlab Character or expression. Label for *x* axis.
#' @param ylab Character or expression. Label for *y* axis.
#' @param main Character or expression. For up to two factors, simply specify the main title. If you stratify the data by more than two factors,
#' either specify a single value that will be added to automatically generated main title, *or* specify an array of multiple titles, one for each plot area.
#' @return A (nested) list of plot options. *Note that the structure of the return value is about to change in a forthcoming release of papaja.*
#' @inherit formula_processor
#' @inheritDotParams graphics::plot.window
#' @details
#'    The measure of dispersion can be either [conf_int()] for between-subjects confidence intervals, [se()] for standard errors,
#'    or any other standard function. For within-subjects confidence intervals, specify [wsci()] or [within_subjects_conf_int()].
#'
#'    If between- or within-subjects confidence intervals are requested, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98% confidence interval, specify
#'    `level = 0.98`. The default is `level = 0.95` for 95% confidence intervals.
#'
#' ## Customisation of plot elements
#'
#' [apa_factorial_plot()] and its descendants [apa_barplot()], [apa_lineplot()],
#' and [apa_beeplot()] are wrapper functions that sequentially call:
#'
#' - [plot.new()],
#' - [plot.window()],
#' - [axis()] (once for *x* axis, once for *y* axis),
#' - [title()] for axis labels and titles,
#' - [rect()] for bars in bar plots,
#' - [points()] for bee swarms,
#' - [lines()] for lines connecting central tendency points,
#' - [arrows()] for error bars,
#' - [points()] for tendency points,
#' - [legend()] for a legend, and
#' - [lines()] for intercepts.
#'
#' These calls can be customized by setting the respective parameters `args_*** = list(...)`.
#'
#' @family plots for factorial designs
#' @examples
#' apa_factorial_plot(
#'   formula = yield ~ (N + P + K | block)
#'   , data = npk
#'   , plot = c("error_bars", "points", "swarms")
#' )
#'
#' apa_factorial_plot(
#'   data = npk
#'   , id = "block"
#'   , dv = "yield"
#'   , factors = c("N", "P", "K")
#'   , las = 1
#'   , plot = c("error_bars", "points", "swarms")
#'   , ylim = c(0, 100)
#' )
#' @export

apa_factorial_plot <-function(data, ...){
  UseMethod("apa_factorial_plot", data)
}

#' @rdname apa_factorial_plot
#' @method apa_factorial_plot formula
#' @export

apa_factorial_plot.formula <- function(formula, data, ...) {
  formula_processor(formula = formula, data = data, .fun = apa_factorial_plot, ...)
}

#' @rdname apa_factorial_plot
#' @export

apa_factorial_plot.default <- function(
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
  , args_points = NULL
  , args_lines = NULL
  , args_swarm = NULL
  , args_error_bars = NULL
  , args_legend = NULL
  , plot = NULL
  , jit = .3
  , xlab = NULL
  , ylab = NULL
  , main = NULL
  , ...
){
  # Data validation:
  validate(data, check_class = "data.frame", check_NA = FALSE)
  validate(id, check_class="character", check_length = 1)
  if(!is.null(factors)){
    validate(factors, check_class = "character")
    validate(length(factors), check_range = c(1,4))
  }
  validate(tendency, check_class = "function", check_length = 1, check_NA =FALSE)
  validate(dispersion, check_class = "function", check_length = 1, check_NA = FALSE)
  validate(level, check_class = "numeric", check_range = c(0,1))
  validate(fun_aggregate, check_class = "function", check_length = 1, check_NA = FALSE)
  validate(na.rm, check_class = "logical", check_length = 1)
  validate(use, check_class = "character", check_length = 1)

  use <- match.arg(arg = use, choices = c("all.obs", "complete.obs"))

  validate(data, check_class = "data.frame", check_cols = c(id, dv, factors), check_NA = FALSE)
  if(!is.null(intercept)) validate(intercept, check_mode = "numeric", check_NA = FALSE)
  if(!is.null(args_x_axis)) validate(args_x_axis, check_class = "list")
  if(!is.null(args_y_axis)) validate(args_y_axis, check_class = "list")
  if(!is.null(args_title)) validate(args_title, check_class = "list")
  if(!is.null(args_rect)) validate(args_rect, check_class = "list")
  if(!is.null(args_points)) validate(args_points, check_class = "list")
  if(!is.null(args_lines)) validate(args_lines, check_class = "list")
  if(!is.null(args_swarm)) validate(args_swarm, check_class = "list")
  if(!is.null(args_error_bars)) validate(args_error_bars, check_class = "list")
  if(!is.null(args_legend)) validate(args_legend, check_class = "list")
  if(!is.null(plot)) validate(plot, check_class = "character")
  if(!is.null(xlab)) if(!is.expression(xlab)) validate(xlab, check_class = "character")
  if(!is.null(ylab)) if(!is.expression(ylab)) validate(ylab, check_class = "character")
  if(!is.null(main)) if(!is.expression(main)) if(!is.matrix(main)) validate(main, check_class = "character")


  # Set defaults ---------------------------------------------------------------
  ellipsis <- defaults(
    list(...)
    , set = list(
      data = data
       , id = id
       , dv = dv
       , factors = factors
       , intercept = intercept
     )
     , set.if.null = list(
       frame.plot = FALSE
       # pass on arguments from function call
       , tendency = substitute(tendency)
       , dispersion = substitute(dispersion)
       , level = level
       , fun_aggregate = substitute(fun_aggregate)
       , na.rm = na.rm
       , use = use
       , reference = reference
       , intercept = intercept
       , jit = jit
       , xlab = xlab
       , ylab = ylab
       , main = main
     )
  )


  # warning if "beside = FALSE" is specified
  if(is.null(ellipsis$beside) || !(ellipsis$beside)) {
    if(!is.null(ellipsis$beside) && !(ellipsis$beside)) {
      warning("Stacked barplots are not supported. Ignoring parameter 'beside = FALSE'.")
    }
    ellipsis$beside <- TRUE
  }

  # Backward compatibility: args_arrows
  if(!is.null(ellipsis$args_arrows)){
    ellipsis$args_error_bars <- ellipsis$args_arrows
  }
  ellipsis$args_arrows <- NULL

  # Backward compatibility: fun.aggregate (Note that this one is NOT written to ellipsis)
  if(!is.null(ellipsis$fun.aggregate)) {
    fun_aggregate <- ellipsis$fun.aggregate
  }
  ellipsis$fun.aggregate <- NULL

  # Backward compatibility: args_axis
  if(!is.null(ellipsis$args_axis) && is.null(ellipsis$args_x_axis)) {
    ellipsis$args_x_axis <- ellipsis$args_axis
  }
  ellipsis$args_axis <- NULL


  ellipsis$intercept <- intercept

  # jittering of x coordinates
  if(is.null(ellipsis$jit)){
    if("bars" %in% plot){
      ellipsis$jit <- .4
    } else {
      ellipsis$jit <- .3
    }
  }

  args_title$main <- main

  args_x_axis$x <- do.call("apa_plot.data.frame", ellipsis)
  args_y_axis$x <- do.call("x_axis", args_x_axis)
  args_title$x <- do.call("y_axis", args_y_axis)
  x <- do.call("annotation", args_title)

  if(any(plot == "bars")) {
    x <- do.call("bars", c(list(x = x), as.list(args_rect)))
  }
  if(any(plot == "swarms")) {
    x <- do.call("swarms", c(list(x = x), as.list(args_swarm)))
  }
  if(any(plot == "lines")) {
    x <- do.call("lines", c(list(x = x), as.list(args_lines)))
  }
  if(any(plot == "error_bars")) {
    x <- do.call("error_bars", c(list(x = x), as.list(args_error_bars)))
  }
  if(any(plot == "points")) {
    x <- do.call("points", c(list(x = x), as.list(args_points)))
  }

  x <- do.call("legends", c(list(.x = x), as.list(args_legend)))

  for (i in seq_along(x$plots)) {
    x$plots[[i]]$.state <- "modify"
  }

  x
}

#' @rdname apa_factorial_plot
#' @export

apa_factorial_plot.afex_aov <- function(
  data
  , tendency = mean
  , dispersion = conf_int
  , fun_aggregate = mean
  , ...
){

  ellipsis <- list(...)

  args <- attributes(data)

  # Change in upcoming afex release (https://github.com/singmann/afex/commit/80563ee443b9005f176ee5f9fcbd07fd89a642e6#diff-e88193a056961d33007d7390f33df59e)
  between <- if(!is.null(names(args$between))) names(args$between) else args$between
  within <- if(!is.null(names(args$within))) names(args$within) else args$within

  afex_factors <- c(unlist(between), unlist(within))

  # Allow changing plot axes ----
  if(is.null(ellipsis$factors)) {
    factors <- afex_factors
  } else {
    if(setequal(ellipsis$factors, afex_factors)) {
      factors <- ellipsis$factors
    } else {
      stop("The set of factors contained in the `afex_aov` object does not match argument `factors`.")
    }
  }

  # ----
  ellipsis <- defaults(
    ellipsis
    , set = list(
      "data" = data$data$long
      , "id" = args$id
      , "dv" = args$dv
      , "factors" = factors
      , "tendency" = substitute(tendency)
      , "dispersion" = substitute(dispersion)
      , "fun_aggregate" = substitute(fun_aggregate)
    )
  )
  do.call("apa_factorial_plot.default", ellipsis)
}
