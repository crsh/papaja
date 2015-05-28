#' Barplot for factorial designs that conform to APA guidelines
#'
#' Creates one or more barplots from a data.frame containing data from a factorial design.
#'
#' @param data A \code{data.frame} that contains the data.
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of variable names that is used to stratify the data.
#' @param dv Character. The name of the dependent variable.
#' @param tendency Closure. A function that will be used as measure of central tendency.
#' @param dispersion Closure. A function that will be used to construct error bars (i.e., whiskers). Defaults to
#'    \code{conf_int} for 95\% confidence intervals. See details.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to 0.95
#'    for 95\% confidence intervals. Ignored if \code{dispersion} is not \code{conf_int}.
#' @param fun_aggregate Closure. The function that will be used to aggregate observations within subjects and factors
#'    before calculating descriptive statistics for each cell of the design. Defaults to \code{mean}.
#' @param na.rm Logical. Specifies if missing values will be removed. Defaults to \code{TRUE}.
#' @param ylim Numeric. A vector specifying upper and lower limit of the y-axis.
#' @param main Character. The main title for your plot.
#' @param xlab Character. Specifies the label of your x-axis. Defauls to the first element of \code{factors}.
#' @param ylab Character. Specifies the label of your y-axis. Defaults to \code{dv}.
#' @param intercepts Numeric. Add horizontal lines to the plot. Specify one or multiple y-values.
#' @param ... Further arguments than can be passed to the underlying \code{barplot} function.
#' @details The measure of dispersion can be either \code{conf_int} for confidence intervals, \code{se} for standard errors,
#'    or any other standard function. If \code{conf_int} is specified, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98\% confindence interval, specify
#'    \code{level = 0.98}. \code{level} defaults to 0.95.
#' @examples NULL
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
  , ylim = NULL
  , main = NULL
  , xlab = NULL
  , ylab = NULL
  , intercepts = NULL
  , ...
){

  validate(data, check_class = "data.frame", check_NA = FALSE)
  validate(id, check_class="character", check_length = 1)
  validate(factors, check_class = "character", check_length = 1)
  validate(tendency, check_class = "function", check_length = 1, check_NA =FALSE)
  validate(dispersion, check_class = "function", check_length = 1, check_NA = FALSE)
  validate(level, check_class = "numeric", check_range = c(0,1))
  validate(fun_aggregate, check_class = "function", check_length = 1, check_NA = FALSE)
  validate(na.rm, check_class = "logical", check_length = 1)
  if(!is.null(ylim)) validate(ylim, check_class = "numeric", check_length = 2)
  if(!is.null(xlab)) validate(xlab, check_length = 1)
  if(!is.null(ylab)) validate(ylab, check_length = 1)
  if(!is.null(intercepts)) validate(intercepts, check_class = "numeric")

  # save original plot architecture
  old.mfrow<-par()$mfrow

  for (i in 1:length(factors)){
    data[[factors[i]]]<-as.factor(data[[factors[i]]])
  }
  data[[id]]<-as.factor(data[[id]])

  if(nrow(data) == 0) {
    stop("no rows in data")
  }

  if(length(factors) > 4) {
    stop("Too many stratifying factors. Specify not more than four factors.")
  }

  ## aggregate data
  aggregated <- fast_aggregate(data=data, dv=dv, factors=c(id, factors), fun=fun_aggregate, na.rm=na.rm)



  ## calculate central tendency
  #yy <- tapply(data[[dv]], list(data[[factors[2]]], data[[factors[1]]]), FUN=tendency, na.rm=TRUE)
  fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency, na.rm=TRUE)

  ## calculate measure of dispersion
  if ## wenn conf_int
    ee <- tapply(data[[dv]], list(data[[factors[2]]], data[[factors[1]]]), FUN=dispersion, na.rm=TRUE)
    ee <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency, na.rm=TRUE, level = level)
  } else {
   ## ee <- tapply(data[[dv]], list(data[[factors[2]]], data[[factors[1]]]), FUN=conf_int, na.rm=TRUE, level=level)
    ee <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency, na.rm=TRUE)

  ## Calculate central tendencies

  ## Calculate dispersions
  fun_dispersion <- deparse(substitute(dispersion))
  if(fun_dispersion == "conf_int") {

  } else {

  }

  if(is.null(ylim)) { # Make sure that complete error bars are plotted
    # ylim <- c(min(0, aggregated[, dv]), )

  }

  ## one factor
  if(length(factors)==1){
    apa.barplot.one(data=aggregated,id=id,dv=dv,factors=factors, ylim=ylim, main=main, intercepts=intercepts, xlab=xlab, ylab=ylab, ...)
  }

  ## two factors
  if(length(factors)==2){
    apa.barplot.core(data=aggregated,id=id,dv=dv,factors=factors,ylim=ylim,main=main,intercepts=intercepts, xlab=xlab, ylab=ylab, ...)
  }

  ## three factors
  if(length(factors)==3){
    par(mfrow=c(1,nlevels(data[[factors[3]]])))
    for (i in levels(data[[factors[3]]])){
      this.title<-paste(main,c(factors[3],"==",i),collapse="")
      apa.barplot.core(data=aggregated[aggregated[[factors[3]]]==i,],id=id,dv=dv,factors=factors[1:2],ylim=ylim,main=this.title,intercepts=intercepts, xlab=xlab, ylab=ylab, ...)
    }
    par(mfrow=old.mfrow)
  }
  # four factors
  if(length(factors)==4){
    par(mfrow=c(nlevels(data[[factors[3]]]),nlevels(data[[factors[4]]])))
    for (i in levels(data[[factors[3]]])){
      for (j in levels(data[[factors[4]]])){
        this.title<-paste(c(main,factors[3],"==",i,"&",factors[4],"==",j),collapse="")
        apa.barplot.core(data=aggregated[aggregated[[factors[3]]]==i&aggregated[[factors[4]]]==j,],id=id,dv=dv,factors=factors[1:2],ylim=ylim,main=this.title,intercepts=intercepts, xlab=xlab, ylab=ylab, fun_aggregate=fun_aggregate, ...)
      }
    }
    par(mfrow=old.mfrow)
  }
}


apa.barplot.core<-function(data, id, dv, factors, main=NULL, ylim=NULL, intercepts=NULL, xlab=NULL, ylab=NULL, col=NULL, ...){

  if(nrow(data)>0){

    names<-levels(data[[factors[1]]])

    #default greyscale colors
    if(is.null(col)){
      nc<-nlevels(data[[factors[2]]])
      colors<-(nc:1/(nc))^.6
      col<-grey(colors)
    }

    # default label for x-axis
    if(is.null(xlab)){
      xlab <- factors[1]
    }

    # default label for y-axis
    if(is.null(ylab)){
      ylab<-paste(dv)
    }

    barx <- barplot(yy, beside=TRUE, col=col, ylim=ylim, names.arg=names, axis.lty=1, xlab=xlab, legend=levels(data[[factors[2]]]), ylab=ylab, main=main, ...)

    error.bar(barx, yy, ee)

    if(!is.null(intercepts)){
      segments(x0=colMeans(barx)-barx[1], y0=intercepts, x1=colMeans(barx)+barx[1], y1=intercepts)
    }
  }
  if(nrow(data)==0){
    plot(x=c(0,1),ylim=c(0,1),type="l",main=main,ylab=NULL,xlab="No observations")
  }
}

apa.barplot.one<-function(data, id, dv, factors, main=NULL, intercepts=NULL, xlab=NULL, ylab=NULL, ylim=NULL, col="white", ...){

  if(nrow(data)>0){

    names<-levels(data[[factors[1]]])

    # default label for x-axis
    if(is.null(xlab)){
      xlab <- factors[1]
    }

    # default label for y-axis
    if(is.null(ylab)){
      ylab<-paste(dv)
    }

    # plot
    barx <- barplot(yy, beside=TRUE, ylim=ylim, names.arg=names, axis.lty=1, xlab=xlab, ylab=ylab, main=main, col=col, ...)
    error.bar(barx,yy,ee)

    if(!is.null(intercepts)){
      segments(x0=colMeans(barx)-barx[1], y0=intercepts, x1=colMeans(barx)+barx[1], y1=intercepts)
    }
  }
  if(nrow(data)==0){
    plot(x=c(0,1),ylim=c(0,1),type="l",main=main, ylab=paste(c("M +-",dispersion,dv),collapse=" "),xlab="No observations")
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
#' Returns the negative deviation that is needed to construct confidence intervals for a vector of observations.
#' @param x \code{numeric}. A vector of observations from your dependent variable.
#' @param level \code{numeric}. The area of the cdf that will be covered. Defaults to .95 for 95\% confidence intervals.
#' @param na.rm \code{logical}. Shall missing values (i.e., \code{NA}s) be removed?
#' @export

conf_int<-function(x, level = 0.95, na.rm = TRUE){
  a <- (1-level)/2
  n <- sum(!is.na(x))
  fac <- qt(a,df=n-1)
  ee <- (sd(x,na.rm=na.rm)*fac)/sqrt(n)
  return(ee)
}

#' Standard errors
#'
#' Returns the standard error of a vector of observations
#'
#' @param x \code{numeric}. A vector of observations from your dependent variable.
#' @param na.rm \code{logical}. Shall missing values (i.e., \code{NA}s) be removed?
#' @export

se <- function(x, na.rm=TRUE){
  n <- sum(!is.na(x))
  ee <- sd(x, na.rm = na.rm)/sqrt(n)
  return(ee)
}

