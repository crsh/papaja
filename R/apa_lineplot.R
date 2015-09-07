#' Lineplot for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more lineplots from a data.frame containing data from
#' a factorial design and sets APA-friendly defaults. It sequentially calls \code{\link{plot}},
#' \code{\link{axis}}, \code{\link{points}}, \code{\link{lines}}, \code{\link{arrows}} and
#' \code{\link{legend}}, that may be further customized.
#'
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
#' @param args_axis An optional \code{list} that contains further arguments that may be passed to \code{\link{axis}}
#' @param args_points An optional \code{list} that contains further arguments that may be passed to \code{\link{points}}
#' @param args_lines An optional \code{list} that contains further arguments that may be passed to \code{\link{lines}}
#' @param args_arrows An optional \code{list} that contains further arguments that may be passed to \code{\link{arrows}}
#' @param args_legend An optional \code{list} that contains further arguments that may be passed to \code{\link{legend}}
#' @param ... Further arguments than can be passed to \code{\link{plot}} function.
#' @details The measure of dispersion can be either \code{conf_int} for confidence intervals, \code{se} for standard errors,
#'    or any other standard function. If \code{conf_int} is specified, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98\% confindence interval, specify
#'    \code{level = 0.98}. \code{level} defaults to 0.95.
#' @seealso \code{\link{barplot}}
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
#' @export

apa_lineplot <- function(
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
  , args_axis = list()
  , args_points = list()
  , args_lines = list()
  , args_arrows = list()
  , args_legend = list()
  , ...
){
  # all the same like barplot:
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
  output <- list()

  # Set defaults
  ellipsis <- defaults(ellipsis,
                       set = list(
                         id = id
                         , dv = dv
                         , factors = factors
                         , intercept = intercept
                       )
                       , set.if.null = list(
                         args.axis = args_axis
                         , args.points = args_points
                         , args.lines = args_lines
                         , args.arrows = args_arrows
                         , args.legend = args_legend
                         , xlab = factors[1]
                         , ylab = as.character(dv)
                         , frame.plot = FALSE
                       ))

  if(length(ellipsis$args.legend$title) == 0) {
    ellipsis$args.legend$title <- factors[2]
  } else if(ellipsis$args.legend$title == "") {
    ellipsis$args.legend$title <- NULL # Save space
  }

  # compatibility: allows aggregation function to be specified via "fun.aggregate"
  if(!is.null(ellipsis$fun.aggregate)) {
    fun_aggregate <- ellipsis$fun.aggregate
  }
  ellipsis$fun.aggregate <- NULL

  # is dplyr available?
  use_dplyr <- "dplyr" %in% rownames(installed.packages())

  # Prepare data
  for (i in factors){
    data[[i]]<-droplevels(as.factor(data[[i]]))
  }
  data[[id]]<-droplevels(as.factor(data[[id]]))

  # strip whitespace from factor names
  factors <- gsub(pattern = " ", replacement = "_", factors)
  id <- gsub(pattern = " ", replacement = "_", id)
  dv <- gsub(pattern = " ", replacement = "_", dv)
  colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))

  if(is.null(ellipsis$jit)){
    ellipsis$jit <- .4
  }

  if(use_dplyr) {
    ## Aggregate subject data
    aggregated <- papaja:::fast_aggregate(data = data, dv = dv, factors = c(id, factors), fun = fun_aggregate)

    ## Calculate central tendencies
    yy <- papaja:::fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency)
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
      ee <- papaja:::fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = dispersion)
    } else {
      ee <- aggregate(formula = as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion)
    }

  }
  ## within-subjects confidence intervals
  if(fun_dispersion == "within_subjects_conf_int" || fun_dispersion == "wsci") {

    # check which factors are between/within
    between <- ellipsis$between
    within <- ellipsis$within

    for (i in 1:length(factors)) {

      if (all(rowSums(table(aggregated[[id]], aggregated[[factors[i]]])!=0)==1)) {
        between <- c(between, factors[i])
      } else {
        within <- c(within, factors[i])
      }
    }

    # split by between factors
    if (is.null(between)) {
      splitted <- list(aggregated)
    } else if(length(between)>1){
      splitted <- split(aggregated, f=as.list(aggregated[, c(between)]), sep = ":")
    } else if (length(between)==1) {
      splitted <- split(aggregated, f=aggregated[, c(between)])
    }

    if(!is.null(within)) {

      Morey_CI <- lapply(X = splitted, FUN = function(x){
        y <- tapply(x[[dv]], as.list(x[, c(id, within)]), FUN = as.numeric) # transform to matrix
        z <- y - array(rowMeans(y, na.rm = TRUE), dim(y)) + mean(y, na.rm=TRUE) # normalise
        CI <- apply(z, MARGIN = (1:(length(within)+1))[-1], FUN = conf_int, level) # calculate CIs for each condition

        # Morey correction
        M <- prod(apply(X = as.matrix(x[, within]), MARGIN = 2, FUN = function(x){nlevels(as.factor(x))}))
        Morey_CI <- CI * M/(M-1)

        # reshape to data.frame
        Morey_CI <- as.data.frame(as.table(Morey_CI))
        if(length(within)==1){
          colnames(Morey_CI)[colnames(Morey_CI)=="Var1"] <- within
        }
        colnames(Morey_CI)[colnames(Morey_CI)=="Freq"] <- dv
        # return
        Morey_CI
      })

    if(is.null(between)) {
      ee <- data.frame(unlist(Morey_CI, recursive=FALSE))
    } else {
      names <- strsplit(names(Morey_CI), split = ":")
      for (i in 1:length(Morey_CI)) {
        for ( j in 1:length(between)){
        Morey_CI[[i]][[between[j]]] <- names[[i]][j]
        }
      }
    }
    ee <- papaja:::fast_aggregate(data = dplyr::bind_rows(Morey_CI), factors = factors, dv = dv, fun =mean)
    output$Morey_CI <- Morey_CI
    } else {
      stop("No within-subjects factors specified.")
    }
  }

  output$yy <- yy
  output$ee <- ee

  ## Adjust ylim to height of error bars
  if(is.null(ellipsis$ylim)) {
    ellipsis$ylim <- c(min(0, yy[, dv] - ee[, dv]), max(yy[, dv] + ee[, dv]))
  }

  ## One factor
  if(length(factors) < 3){
#     if(is.null(ellipsis$lty)){
#       ellipsis$lty <- "solid"
#     }

    ellipsis <- defaults(
      ellipsis
      , set = list(
        yy = yy
        , ee = ee
      )
      , set.if.null = list(

      ))

    # par(mfrow=par("mfrow"))
    do.call("apa.lineplot.core", ellipsis)
  }

  ## Three factors
  old.mfrow <- par("mfrow") # Save original plot architecture

  if(length(factors) == 3) {
    par(mfrow = c(1, nlevels(data[[factors[3]]])))
    tmp_main <- ellipsis$main

    # by default, only plot legend in topright plot:
    tmp_plot <- 1:nlevels(data[[factors[3]]])==nlevels(data[[factors[3]]])
    names(tmp_plot) <- levels(data[[factors[3]]])

    ellipsis$args.legend <- defaults(ellipsis$args.legend
                                     , set = list(

                                     )
                                     , set.if.null = list(
                                       plot = tmp_plot
                                     )
                           )

    if(is.null(ellipsis$args.legend$plot)) {
      ellipsis$args.legend$plot <- 1:nlevels(data[[factors[3]]])==nlevels(data[[factors[3]]])
    }

    if(length(ellipsis$args.legend$plot)!=nlevels(data[[factors[3]]])) {
      rec <- length(ellipsis$args.legend$plot) / nlevels(data[[factors[3]]])
      ellipsis$args.legend$plot <- rep(ellipsis$args.legend$plot, round(rec+1))
    }

    names(ellipsis$args.legend$plot) <- levels(data[[factors[3]]])

    for (i in levels(yy[[factors[3]]])) {

      ellipsis.i <- defaults(ellipsis, set = list(
        main = paste0(tmp_main, c(factors[3],": ",i),collapse="")
        , yy = yy[yy[[factors[3]]]==i, ]
        , ee = ee[ee[[factors[3]]]==i, ]
      ), set.if.null = list(

      ))

      # by default, only draw legend in very right plot
      ellipsis.i$args.legend <- defaults(ellipsis.i$args.legend, set = list(plot = ellipsis$args.legend$plot[i]))

      # suppresses ylab
      if(i!=levels(yy[[factors[3]]])[1]){
        ellipsis.i$ylab <- ""
      }

      do.call("apa.lineplot.core", ellipsis.i)
    }
    par(mfrow=old.mfrow)
  }

  ## Four factors
  if(length(factors)==4){
    par(mfrow=c(nlevels(data[[factors[3]]]),nlevels(data[[factors[4]]])))
    tmp_main <- ellipsis$main

    legend.plot <- array(FALSE, dim=c(nlevels(data[[factors[3]]]), nlevels(data[[factors[4]]])))
    legend.plot[1,nlevels(data[[factors[4]]])] <- TRUE

    ellipsis$args.legend <- defaults(ellipsis$args.legend
                                     , set = list(

                                     )
                                     , set.if.null = list(
                                       plot = legend.plot
                                     )
    )
    rownames(ellipsis$args.legend$plot) <- levels(data[[factors[3]]])
    colnames(ellipsis$args.legend$plot) <- levels(data[[factors[4]]])



    for (i in levels(yy[[factors[3]]])){
      for (j in levels(yy[[factors[4]]])) {
        ellipsis.i <- defaults(ellipsis, set = list(
          main = paste0(c(tmp_main,factors[3],": ",i," & ",factors[4],": ",j),collapse="")
          , yy = yy[yy[[factors[3]]]==i&yy[[factors[4]]]==j,]
          , ee = ee[ee[[factors[3]]]==i&ee[[factors[4]]]==j,]
        ), set.if.null = list(
          # nothing
        ))

        # by default, only draw legend in topright plot
        ellipsis.i$args.legend <- defaults(ellipsis.i$args.legend, set = list(plot = ellipsis$args.legend$plot[i, j]))

        # suppresses ylab
        if(j!=levels(yy[[factors[4]]])[1]){
          ellipsis.i$ylab <- ""
        }
        do.call("apa.lineplot.core", ellipsis.i)
      }
    }
    par(mfrow=old.mfrow)
  }
  invisible(output)
}


apa.lineplot.core<-function(yy, ee, id, dv, factors, intercept=NULL, ...) {

  ellipsis <- list(...)

  # Plot
  # plot.new()

  # jittering of x coordinates
  jit <- ellipsis$jit


  factors <- gsub(factors, pattern = " ", replacement = "_")
  id <- gsub(id, pattern = " ", replacement = "_")
  dv <- gsub(dv, pattern = " ", replacement = "_")

  # move to apa_lineplot???
  if(length(factors) > 1){
    yy$x <- as.integer(yy[[factors[1]]]) + (as.integer(yy[[factors[2]]])-.5)/(nlevels(yy[[factors[2]]]))*(jit)-.5*jit
    l2 <- levels(yy[[factors[2]]])
    onedim <- FALSE
  } else {
    yy$x <- as.integer(yy[[factors[1]]])
    l2 <- 1
    factors[2] <- "f2"
    yy[["f2"]] <- 1
    ee[["f2"]] <- 1
    onedim <- TRUE
  }




  # save parameters for multiple plot functions
  args.legend <- ellipsis$args.legend
  args.points <- ellipsis$args.points
  args.lines <- ellipsis$args.lines
  args.axis <- ellipsis$args.axis
  args.arrows <- ellipsis$args.arrows

  # basic plot


  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      xlim = c(min(yy$x), max(yy$x))
    )
    , set = list(
      xaxt = "n"
      , x = 1
      , type = "n"
      , jit = NULL
      , args.legend = NULL
      , args.points = NULL
      , args.lines = NULL
      , args.axis = NULL
      , args.arrows = NULL
    )
  )

  do.call("plot.default", ellipsis)

  # prepare defaults for x axis
  args.axis <- defaults(args.axis
    , set = list(
      side = 1
    )
    , set.if.null = list(
      at = 1:nlevels(yy[[factors[1]]])
      , labels = levels(yy[[factors[1]]])
    )
  )


  # only draw axis if axis type is not specified or not specified as "n"
  if(is.null(args.axis$xaxt)||args.axis$xaxt!="n") {
    do.call("axis", args.axis)
  }

  # convert to matrices
  x <- tapply(yy[, "x"],list(yy[[factors[1]]], yy[[factors[2]]]), as.numeric)
  y <- tapply(yy[, dv],list(yy[[factors[1]]], yy[[factors[2]]]), as.numeric)
  e <- tapply(ee[, dv],list(ee[[factors[1]]], ee[[factors[2]]]), as.numeric)


  # prepare and draw points
  args.points <- defaults(args.points
    , set = list(
      x = x
      , y = y
    )
    , set.if.null = list(
      pch = c(21:25,1:20)
      , col = rep("black", length(l2))
    )
  )

  do.call("points", args.points)

  # prepare and draw lines
  args.lines <- defaults(args.lines
     , set = list(
       x = x
       , y = y
     )
     , set.if.null = list(
       lty = 1:6
       , col = rep("black", length(l2))
     )
  )

  do.call("lines", args.lines)

  # prepare and draw arrows (i.e., error bars)
  args.arrows <- defaults(args.arrows
                          , set = list(
                            x0 = x
                            , x1 = x
                            , y0 = y-e
                            , y1 = y+e
                          )
                          , set.if.null = list(
                              angle = 90, code = 3, length = .1
                            )
                          )


  do.call("arrows", args.arrows)

  # prepare and draw legend
  if(onedim==FALSE) {

    args.legend <- defaults(args.legend
        , set.if.null = list(
          x = "topright"
          , legend = levels(yy[[factors[2]]])
          , pch = args.points$pch[1:nlevels(yy[[factors[2]]])]
          , lty = args.lines$lty
          , bty = "n"
    ))

    do.call("legend", args.legend)
  }

  # draw intercept

  if(!is.null(intercept)){
    lines(x=xlim, y=rep(intercept,2))
  }
  return(list(ellipsis, args.axis, args.points, args.lines, args.legend))
}


#' Set defaults
#'
#' A helper function that is intended for internal use. A list \code{ellipsis} may be manipulated by overwriting (via \code{set}) or adding (via \code{set.if.null}) list elements.
#'
#' @param ellipsis A \code{list}, usually a list that comes from an ellipsis
#' @param set A named  \code{list} of parameters that are intended to be set.
#' @param set.if.null A named \code{list} of parameters that are intended to be set if and only if the parameter is not already in \code{ellipsis}.

defaults <- function(ellipsis, set = NULL, set.if.null = NULL) {

  ellipsis <- as.list(ellipsis)

  for (i in names(set)) {
    ellipsis[[i]] <- set[[i]]
  }
  for (i in names(set.if.null)) {
    if(is.null(ellipsis[[i]])) ellipsis[[i]] <- set.if.null[[i]]
  }
  return(ellipsis)
}

#' @method lines matrix

lines.matrix <- function(x, y, type = "l", ...) {

  args <- list(...)
  args$type = type

  for (i in 1:ncol(x)){
    args.i <- lapply(X = args, FUN = sel, i)
    args.i$x <- x[, i]
    args.i$y <- y[, i]
    do.call("lines", args.i)
  }
}


#' @method points matrix

points.matrix <- function(x, y, type = "p", ...) {

  args <- list(...)
  args$type = type

  for (i in 1:ncol(x)){
    args.i <- lapply(X = args, FUN = sel, i)
    args.i$x <- x[, i]
    args.i$y <- y[, i]
    do.call("points", args.i)
  }
}

#' @method arrows matrix

arrows.matrix <- function(x0, x1, y0, y1, ...) {

  args <- list(...)

  for (i in 1:ncol(x)){
    args.i <- lapply(X = args, FUN = sel, i)
    args.i$x0 <- x0[, i]
    args.i$x1 <- x1[, i]
    args.i$y0 <- y0[, i]
    args.i$y1 <- y1[, i]
    do.call("arrows", args.i)
  }
}

# helper function
sel <- function(x, i){
  if(length(x)>=i) x <- x[i]
  return(x)
}

#' @export
within_subjects_conf_int <- function(...) return(100)

#' @export
wsci <- within_subjects_conf_int
