#' APA-compatible barplot
#'
#' This function returns barplots.
#' @param data A \code{data.frame} that contains your data
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of variable names that shall be used to stratify data.
#' @param dv Character. The dependent variable.
#' @param tendency Closure. A function that will be used as measure of central tendency.
#' @param dispersion Closure. A function that will be used to constrct error bars (i.e., whiskers). Defaults to \code{conf.int} for 95\% confidence intervals. See details.
#' @param level Numeric. If confidence intervals are printed, this determines the area of the cdf that will be covered. Defaults to .95 for 95\% confidence intervals.
#' @param fun.aggregate Closure. The function that will be used to aggregate data. Defaults to \code{mean}
#' @param na.rm Logical. Specifies if missing values will be removed. Defaults to \code{TRUE}.
#' @param ylim Numeric. A vector specifying upper and lower limit of the y-axis.
#' @param main Character.The main title for your plot.
#' @param ylab Character. Specifies the label of your y-axis. Defaults to \code{dv}.
#' @param intercepts Numeric. Draws horizontal lines into your plot. Specify one or multiple y-values.
#' @details The measure of dispersion can be either \code{conf.int} for confidence intervals, \code{se} for standard errors,
#'    or any other standard function. If \code{conf.int} is specified, you can also specify the area of the cumulative distribution function that will be covered.
#'    For instance, if you want a 98\% confindence interval, specify \code{level=.98}. \code{level} defaults to .95.
#' @examples
#'   NULL
#' @export

apa_barplot<-function(data, id, factors, dv, tendency=mean, dispersion=conf.int, level=.95, fun.aggregate=mean, na.rm=TRUE, ylim=NULL, main=NULL, ylab=NULL, intercepts=NULL, ...){

  # save original plot architecture
  old.mfrow<-par()$mfrow

  for (i in 1:length(factors)){
    data[[factors[i]]]<-as.factor(data[[factors[i]]])
  }
  data[[id]]<-as.factor(data[[id]])

  if(nrow(data)==0){
    stop("no rows in data")
  }

  if(length(factors)<1){
    stop("not enough stratifying factors")
  }
  if(length(factors)>4){
    stop("too many stratifying factors")
  }

  ## aggregate data
  aggregated <- .aggregate(data=data, dv=dv, factors=c(id, factors), fun=fun.aggregate, na.rm=na.rm)# data[,c(id,dv,factors)]#

  ## eindimensional
  if(length(factors)==1){
    apa.barplot.one(data=aggregated,id=id,dv=dv,factors=factors,ylim=ylim,main=main,tendency=tendency,dispersion=dispersion, level=level,intercepts=intercepts,ylab=ylab,fun.aggregate=fun.aggregate, ...)
  }

  ## zweidimensional
  if(length(factors)==2){
    apa.barplot.core(data=aggregated,id=id,dv=dv,factors=factors,ylim=ylim,main=main,tendency=tendency,dispersion=dispersion, level=level,intercepts=intercepts,ylab=ylab,fun.aggregate=fun.aggregate, ...)
  }

  ## dreidimensional
  if(length(factors)==3){
    par(mfrow=c(1,nlevels(data[[factors[3]]])))
    for (i in levels(data[[factors[3]]])){
      this.title<-paste(main,c(factors[3],"==",i),collapse="")
      apa.barplot.core(data=aggregated[aggregated[[factors[3]]]==i,],id=id,dv=dv,factors=factors[1:2],ylim=ylim,main=this.title,tendency=tendency,dispersion=dispersion, level=level,intercepts=intercepts,ylab=ylab, fun.aggregate=fun.aggregate, ...)
    }
    par(mfrow=old.mfrow)
  }
  # vierdimensional
  if(length(factors)==4){
    par(mfrow=c(nlevels(data[[factors[3]]]),nlevels(data[[factors[4]]])))
    for (i in levels(data[[factors[3]]])){
      for (j in levels(data[[factors[4]]])){
        this.title<-paste(c(main,factors[3],"==",i,"&",factors[4],"==",j),collapse="")
        apa.barplot.core(data=aggregated[aggregated[[factors[3]]]==i&aggregated[[factors[4]]]==j,],id=id,dv=dv,factors=factors[1:2],ylim=ylim,main=this.title,tendency=tendency,dispersion=dispersion, level=level,intercepts=intercepts,ylab=ylab, fun.aggregate=fun.aggregate, ...)
      }
    }
    par(mfrow=old.mfrow)
  }
}


apa.barplot.core<-function(data,id,dv,factors,main=NULL,tendency=mean, ylim=NULL, dispersion=conf.int,level=0.95,intercepts=NULL,ylab=NULL, col=NULL,fun.aggregate=match.fun(fun.aggregate), ...){

  if(nrow(data)>0){

    grouped<-grouped_df(data,vars=list(as.name(id),as.name(factors[1]),as.name(factors[2])),drop=TRUE)

    tmp.m<-as.data.frame(summarise_each(grouped,funs(fun.aggregate(.,na.rm=TRUE)),matches(dv)))
    tmp.n<-as.data.frame(summarise_each(grouped,funs(individual.n(.)),matches(dv)))

    #colnames(tmp.m)<-c(id,factors,dv)
    #colnames(tmp.n)<-c(id,factors,dv)

    # the non-dplyr functions to aggregate within core-function
    #tmp.m<-aggregate(formula=formula(paste(c(dv,"~",id,"+",paste(factors,collapse="+")),collapse="")),data=data,FUN=match.fun(fun.aggregate))
    #tmp.n<-aggregate(formula=formula(paste(c(dv,"~",id,"+",paste(factors,collapse="+")),collapse="")),data=data,FUN=individual.n)

    yy <- tapply(tmp.m[[dv]],list(tmp.m[[factors[2]]],tmp.m[[factors[1]]]),tendency,na.rm=TRUE)
    N  <- tapply(tmp.m[[dv]],list(tmp.m[[factors[2]]],tmp.m[[factors[1]]]),individual.n)


    ee <- tapply(tmp.m[[dv]],list(tmp.m[[factors[2]]],tmp.m[[factors[1]]]),dispersion, na.rm=TRUE, level=level)

    names<-levels(data[[factors[1]]])
    legend<-paste(c("white=",levels(data[[factors[2]]])[1],", grey=",levels(data[[factors[2]]])[2]),collapse="")

    #default greyscale colors
    if(is.null(col)){
      nc<-nlevels(data[[factors[2]]])
      colors<-(nc:1/(nc))^.6
      col<-grey(colors)
    }

    if(is.null(ylab)){
      as.character
      ylab<-paste0(dv)
    }


    barx <- barplot(yy, beside=TRUE,col=col, ylim=ylim, names.arg=names, axis.lty=1, xlab=factors[1], legend=levels(data[[factors[2]]]),ylab=ylab, main=main, ...)

    error.bar(barx,yy,ee)


    if(!is.null(intercepts)){
      segments(x0=colMeans(barx)-barx[1], y0=intercepts, x1=colMeans(barx)+barx[1], y1=intercepts)
    }
  }
  if(nrow(data)==0){
    plot(x=c(0,1),y=ylim,type="l",main=main,ylab=NULL,xlab="no obervations")
  }
}

apa.barplot.one<-function(data,id,dv,factors,main=NULL,tendency=mean,dispersion=conf.int, level=.95,intercepts=NULL,ylab=NULL, fun.aggregate=fun.aggregate, ...){

  if(nrow(data)>0){

    tmp.m<-aggregate(formula=formula(paste(c(dv,"~",id,"+",factors),collapse="")),data=data,FUN=match.fun(fun.aggregate))
    tmp.n<-aggregate(formula=formula(paste(c(dv,"~",id,"+",factors),collapse="")),data=data,FUN=individual.n)

    yy <- tapply(tmp.m[[dv]],list(tmp.m[[factors[1]]]),mean)
    N  <- tapply(tmp.m[[dv]],list(tmp.m[[factors[1]]]),individual.n)

    ee <- tapply(tmp.m[[dv]],list(tmp.m[[factors[1]]]),dispersion, na.rm=na.rm, level=level)



    names<-levels(data[[factors[1]]])

    if(is.null(ylab)){
      ylab<-paste(c("M +-",dispersion,dv),collapse=" ")
    }
    barx <- barplot(yy, beside=TRUE,col=col, ylim=ylim, names.arg=names, axis.lty=1, xlab=factors[1],ylab=ylab,main=main,...)
    error.bar(barx,yy,ee)

    if(!is.null(intercepts)){
      segments(x0=colMeans(barx)-barx[1], y0=intercepts, x1=colMeans(barx)+barx[1], y1=intercepts)
    }
  }
  if(nrow(data)==0){
    plot(x=c(0,1),y=ylim,type="l",main=main,,ylab=paste(c("M +-",dispersion,dv),collapse=" "),xlab="no obervations")
  }
}




# this is a copy from http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# returns number of non-missing values
individual.n<-function(x,na.rm){
  y<-sum(!is.na(x))
  return(y)
}

#' Between-subjects confidence intervals
#'
#' returns confidence intervals
#' @param x \code{numeric}. A vector of observations from your dependent variable.
#' @param level \code{numeric}. The area of the cdf that will be covered. Defaults to .95 for 95\% confidence intervals.
#' @param na.rm \code{logical}. Shall missing values (i.e., \code{NA}s) be removed?
#' @export
conf.int<-function(x,level=.95,na.rm=TRUE){
  a<-(1-level)/2
  n<-sum(!is.na(x))
  fac<-qt(a,df=n-1)
  ee<-(sd(x,na.rm=na.rm)*fac)/sqrt(n)
  return(ee)
}

# returns standard errors
se<-function(x, na.rm=TRUE, level){
  n<-sum(!is.na(x))
  ee<-sd(x, na.rm=na.rm)/sqrt(n)
  return(ee)
}

