#' Barplot for factorial designs that conform to APA guidelines
#'
#' Wrapper function for \code{\link{barplot}} that creates one or more barplots from a data.frame containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#' @param data A \code{data.frame} that contains the data.
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of up to 4 variable names that is used to stratify the data.
#' @param dv Character. The name of the dependent variable.
#' @param tendency Closure. A function that will be used as measure of central tendency.
#' @param dispersion Closure. A function that will be used to construct error bars (i.e., whiskers). Defaults to
#'    \code{conf_int} for 95\% confidence intervals. See details.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to 0.95
#'    for 95\% confidence intervals. Ignored if \code{dispersion} is not \code{conf_int}.
#' @param fun_aggregate Closure. The function that will be used to aggregate observations within subjects and factors
#'    before calculating descriptive statistics for each cell of the design. Defaults to \code{mean}.
#' @param na.rm Logical. Specifies if missing values are removed. Defaults to \code{TRUE}.
#' @param intercept Numeric. Adds a horizontal line to the plot.
#' @param ... Further arguments than can be passed to \code{\link{barplot}} function.
#' @details The measure of dispersion can be either \code{conf_int} for confidence intervals, \code{se} for standard errors,
#'    or any other standard function. If \code{conf_int} is specified, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98\% confindence interval, specify
#'    \code{level = 0.98}. \code{level} defaults to 0.95.
#' @seealso \code{\link{barplot}}
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
#' @export

apa_barplot <- function(
  data
  , id
  , factors
  , dv
  , tendency = mean
  , dispersion = conf_int
  , level = 0.95
  , fun_aggregate = mean
  , na.rm = TRUE
  , intercept = NULL
  , ...
){
  validate(data, check_class = "data.frame", check_NA = FALSE)
  validate(id, check_class="character", check_length = 1)
  validate(factors, check_class = "character")
  validate(length(factors), check_range = c(1,4))
  validate(tendency, check_class = "function", check_length = 1, check_NA =FALSE)
  validate(dispersion, check_class = "function", check_length = 1, check_NA = FALSE)
  validate(level, check_class = "numeric", check_range = c(0,1))
  validate(fun_aggregate, check_class = "function", check_length = 1, check_NA = FALSE)
  validate(na.rm, check_class = "logical", check_length = 1)
  if(!is.null(intercept)) validate(intercept, check_class = "numeric")

  ellipsis <- list(...)

  # Set defaults
  ## Labels
  if(is.null(ellipsis$xlab)){
    ellipsis$xlab <- factors[1]
  }

  if(is.null(ellipsis$ylab)){
    ellipsis$ylab<-paste(dv)
  }

  if(length(ellipsis$args.legend$title) == 0) {
    ellipsis$args.legend$title <- factors[2]
  } else if(ellipsis$args.legend$title == "") {
    ellipsis$args.legend$title <- NULL # Save space
  }

  if(is.null(ellipsis$legend.text)) {
    ellipsis$legend.text <- levels(data[[factors[2]]])
  }

  if(is.null(ellipsis$names.arg)) ellipsis$names.arg <- levels(data[[factors[1]]])

  ## Plot options
  if(is.null(ellipsis$axis.lty)) {
    ellipsis$axis.lty <- 1
  }

  if(is.null(ellipsis$beside) || !(ellipsis$beside)) {
    if(!is.null(ellipsis$beside) && !(ellipsis$beside)) warning("Stacked barplots are not supported. Ignoring parameter 'beside = FALSE'.")
    ellipsis$beside <- TRUE
  }

  use_dplyr <- "dplyr" %in% rownames(installed.packages())

  # Prepare data
  for (i in factors){
    data[[i]]<-as.factor(data[[i]])
  }
  data[[id]]<-as.factor(data[[id]])

  # strip whitespace from factor names
  factors <- gsub(pattern = " ", replacement = "_", factors)
  id <- gsub(pattern = " ", replacement = "_", id)
  colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))


  if(use_dplyr) {
    ## Aggregate subject data
    aggregated <- fast_aggregate(data = data, dv = dv, factors = c(id, factors), fun = fun_aggregate)

    ## Calculate central tendencies
    yy <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency)
  } else {
    ## Aggregate subject data
    aggregated <- aggregate(formula = as.formula(paste0(dv, "~", paste(c(id, factors), collapse = "*"))), data = data, FUN = fun_aggregate)

    ## Calculate central tendencies
    yy <- aggregate(formula = as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = tendency)
  }


  ## Calculate dispersions
  fun_dispersion <- deparse(substitute(dispersion))
  if(fun_dispersion == "conf_int") {
    ee <- aggregate(formula = as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion, level = level)
  } else {
    if(use_dplyr) {
      ee <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = dispersion)
    } else {
      ee <- aggregate(formula = as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion)
    }

  }

  ## Adjust ylim to height of error bars
  if(is.null(ellipsis$ylim)) {
    ellipsis$ylim <- c(min(0, yy[, dv] - ee[, dv]), max(yy[, dv] + ee[, dv]))
  }


  # Plot
  ## One factor
  if(length(factors) == 1){
    if(is.null(ellipsis$col)) {
      ellipsis$col <- "white"
    }
    do.call(function(...) apa.barplot.core(yy = yy, ee = ee, id = id, dv = dv, factors = factors, intercept=intercept, ...), ellipsis)
  }

  ## Two factors
  if(length(factors) >= 2) { # Set default bar colors
    if(is.null(ellipsis$col)) {
      nc <- nlevels(data[[factors[2]]])
      colors <- (nc:1/(nc)) ^ 0.6
      ellipsis$col <- grey(colors)
    }
  }

  if(length(factors) == 2){
    do.call(function(...) apa.barplot.core(yy = yy, ee = ee, id = id, dv = dv, factors = factors,intercept=intercept, ...), ellipsis)
  }

  ## Three factors
  old.mfrow <- par()$mfrow # Save original plot architecture

  if(length(factors) == 3) {
    par(mfrow = c(1, nlevels(data[[factors[3]]])))
    tmp_main <- ellipsis$main
    for (i in levels(data[[factors[3]]])) {
      ellipsis$main <- paste0(tmp_main, c(factors[3],": ",i),collapse="")
      do.call(function(...) apa.barplot.core(yy = yy[yy[[factors[3]]]==i,], ee = ee[ee[[factors[3]]]==i,], id = id, dv = dv, factors = factors,intercept=intercept, ...), ellipsis)
    }
    par(mfrow=old.mfrow)
  }

  ## Four factors
  if(length(factors)==4){
    par(mfrow=c(nlevels(data[[factors[3]]]),nlevels(data[[factors[4]]])))
    tmp_main <- ellipsis$main
    for (i in levels(data[[factors[3]]])){
      for (j in levels(data[[factors[4]]])){
        ellipsis$main<-paste0(c(tmp_main,factors[3],": ",i," & ",factors[4],": ",j),collapse="")
        do.call(function(...) apa.barplot.core(yy = yy[yy[[factors[3]]]==i&yy[[factors[4]]]==j,], ee = ee[ee[[factors[3]]]==i&ee[[factors[4]]]==j,], id = id, dv = dv, factors = factors,intercept=intercept, ...), ellipsis)
      }
    }
    par(mfrow=old.mfrow)
  }
}


apa.barplot.core<-function(yy, ee, id, dv, factors, intercept=NULL, ...) {

  if(length(factors) >= 2) {
    # convert to matrices
    y <- tapply(yy[, dv],list(yy[, factors[2]], yy[, factors[1]]), FUN=as.numeric)
    e <- tapply(ee[, dv],list(ee[, factors[2]], ee[, factors[1]]), FUN=as.numeric)
  } else {
    y <- yy[, dv]
    e <- ee[, dv]
  }

  # barplot()
  barx <- barplot(height = y, ...)

  # error bars
  error.bar(barx, y, e)

  if(!is.null(intercept)){
    segments(x0=colMeans(barx)-barx[1], y0=intercept, x1=colMeans(barx)+barx[1], y1=intercept)
  }
}


# this is a copy from http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#' Between-subjects confidence intervals
#'
#' Returns the deviation that is needed to construct confidence intervals for a vector of observations.
#'
#' @param x Numeric. A vector of observations from your dependent variable.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to 0.95
#'    for 95\% confidence intervals.
#' @param na.rm Logical. Specifies if missing values are removed.
#' @export

conf_int<-function(x, level = 0.95, na.rm = TRUE){
  a <- (1-level)/2
  n <- sum(!is.na(x))
  fac <- -qt(a,df=n-1)
  ee <- (sd(x,na.rm=na.rm)*fac)/sqrt(n)
  return(ee)
}

#' Standard errors
#'
#' Returns the standard error of a vector
#'
#' @param x Nnumeric. A vector of observations from your dependent variable.
#' @param na.rm Logical. Specifies if missing values are removed.
#' @export

se <- function(x, na.rm=TRUE){
  n <- sum(!is.na(x))
  ee <- sd(x, na.rm = na.rm)/sqrt(n)
  return(ee)
}

