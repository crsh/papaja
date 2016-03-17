#' Beeswarm plots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more beeswarm plots from a data.frame containing data from
#' a factorial design and sets APA-friendly defaults.
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
#' @param intercept Numeric. Adds a horizontal line to the plot. Can be either a single value or a matrix. For the matrix
#'    case, multiple lines are drawn, where the dimensions of the matrix determine the number of lines to be drawn.
#' @param args_axis An optional \code{list} that contains further arguments that may be passed to \code{\link{axis}}
#' @param args_points An optional \code{list} that contains further arguments that may be passed to \code{\link{points}}
#' @param args_arrows An optional \code{list} that contains further arguments that may be passed to \code{\link{arrows}}
#' @param args_legend An optional \code{list} that contains further arguments that may be passed to \code{\link{legend}}
#' @param ... Further arguments than can be passed to \code{\link{plot}} function.
#' @details The measure of dispersion can be either \code{conf_int} for confidence intervals, \code{se} for standard errors,
#'    or any other standard function. If \code{conf_int} is specified, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98\% confindence interval, specify
#'    \code{level = 0.98}. \code{level} defaults to 0.95.
#' @seealso \code{\link{barplot}}
#' @examples
#' apa_beeplot(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N")
#' )
#'
#' apa_beeplot(
#' data = npk
#'  , id = "block"
#'  , dv = "yield"
#'  , factors = c("N", "P")
#'  , args.legend = list(x = "center")
#'  #, jit = 0
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
#' @familiy apa_beeplot
#'
#' @rdname apa_beeplot
#' @export

apa_beeplot.default <- function(
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
  if(!is.null(intercept)) validate(intercept, check_mode = "numeric")

  ellipsis <- list(...)
  output <- list()

  # Set defaults
  ellipsis <- defaults(ellipsis,
                       set = list(
                         id = id
                         , dv = dv
                         , factors = factors
                         , intercept = intercept
                         , reference = NULL
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
  for (i in c(id, factors)){
    data[[i]]<-droplevels(as.factor(data[[i]]))
  }

  # strip whitespace from factor names
  factors <- gsub(pattern = " ", replacement = "_", factors)
  id <- gsub(pattern = " ", replacement = "_", id)
  dv <- gsub(pattern = " ", replacement = "_", dv)
  colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))

  # remove extraneous columns from dataset
  data <- data[, c(id, factors, dv)]

  if(is.null(ellipsis$jit)){
    ellipsis$jit <- .3
  }

  ## Aggregate subject data
  if(use_dplyr) {
    aggregated <- papaja:::fast_aggregate(data = data, dv = dv, factors = c(id, factors), fun = fun_aggregate)
  } else {
    aggregated <- aggregate(formula = as.formula(paste0(dv, "~", paste(c(id, factors), collapse = "*"))), data = data, FUN = fun_aggregate)
  }

  ## Calculate central tendencies
  if(use_dplyr) {
    yy <- papaja:::fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency)
  } else {
    yy <- aggregate(formula = as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = tendency)
  }

  ## Calculate dispersions
  fun_dispersion <- deparse(substitute(dispersion))
  if(fun_dispersion == "within_subjects_conf_int" || fun_dispersion == "wsci") {
    ee <- wsci(data = aggregated, id = id, factors = factors, level = level, method = "Morey", dv = dv)
  } else {

    if(fun_dispersion == "conf_int") {
      ee <- aggregate(formula = as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion, level = level)
    } else {
      if(use_dplyr) {
        ee <- papaja:::fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = dispersion)
      } else {
        ee <- aggregate(formula = as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion)
      }
    }
  }

  colnames(yy)[which(colnames(yy)==dv)] <- "tendency"
  colnames(ee)[which(colnames(ee)==dv)] <- "dispersion"

  y.values <- merge(yy, ee, by = factors)

  output$y <- y.values

  output$data <- aggregated


  ## Adjust ylim to height of error bars
  if(is.null(ellipsis$ylim)) {
    ellipsis$ylim <- c(
      min(
        0
        , y.values[, "tendency"] - y.values[, "dispersion"]
        , aggregated[, dv]
      )
      , max(
        y.values[, "tendency"] + y.values[, "dispersion"]
        , aggregated[, dv]
      )
    )
  }

  ## One or two factors
  if(length(factors) < 3){
    #     if(is.null(ellipsis$lty)){
    #       ellipsis$lty <- "solid"
    #     }

    ellipsis <- defaults(
      ellipsis
      , set = list(
        y.values = y.values
        , aggregated = aggregated
      )
      , set.if.null = list(

      ))

    # par(mfrow=par("mfrow"))
    do.call("apa.beeplot.core", ellipsis)
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

    for (i in levels(y.values[[factors[3]]])) {

      ellipsis.i <- defaults(ellipsis, set = list(
        main = paste0(tmp_main, c(factors[3],": ",i),collapse="")
        , y.values = y.values[y.values[[factors[3]]]==i, ]
        , aggregated = aggregated[aggregated[[factors[3]]]==i, ]
      ), set.if.null = list(

      ))

      # by default, only draw legend in very right plot
      ellipsis.i$args.legend <- defaults(ellipsis.i$args.legend, set = list(plot = ellipsis$args.legend$plot[i]))

      # suppresses ylab
      if(i!=levels(y.values[[factors[3]]])[1]){
        ellipsis.i$ylab <- ""
      }

      do.call("apa.beeplot.core", ellipsis.i)
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



    for (i in levels(y.values[[factors[3]]])){
      for (j in levels(y.values[[factors[4]]])) {
        ellipsis.i <- defaults(ellipsis, set = list(
          main = paste0(c(tmp_main,factors[3],": ",i," & ",factors[4],": ",j),collapse="")
          , y.values = y.values[y.values[[factors[3]]]==i&y.values[[factors[4]]]==j,]
          , aggregated = aggregated[aggregated[[factors[3]]]==i&aggregated[[factors[4]]]==j,]
        ), set.if.null = list(
          # nothing
        ))

        # by default, only draw legend in topright plot
        ellipsis.i$args.legend <- defaults(ellipsis.i$args.legend, set = list(plot = ellipsis$args.legend$plot[i, j]))

        # suppresses ylab
        if(j!=levels(y.values[[factors[4]]])[1]){
          ellipsis.i$ylab <- ""
        }
        do.call("apa.beeplot.core", ellipsis.i)
      }
    }
    par(mfrow=old.mfrow)
  }
  invisible(output)
}


apa.beeplot.core<-function(aggregated, y.values, id, dv, factors, intercept=NULL, ...) {

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
    l2 <- levels(y.values[[factors[2]]])
    onedim <- FALSE
  } else {
    l2 <- 1
    factors[2] <- "f2"
    y.values[["f2"]] <- as.factor(1)
    aggregated$f2 <- as.factor(1)
    onedim <- TRUE
  }

  space <-1-jit

  y.values$x <- as.integer(y.values[[factors[1]]]) - .5
  aggregated$x <- as.integer(aggregated[[factors[1]]]) - .5

  if(onedim==FALSE){
    y.values$x <- y.values$x - .5  + space/2 + (1-space)/(nlevels(y.values[[factors[[2]]]])-1) * (as.integer(y.values[[factors[2]]])-1)
    aggregated$x <- aggregated$x - .5 + space/2 + (1-space)/(nlevels(aggregated[[factors[[2]]]])-1) * (as.integer(aggregated[[factors[2]]])-1)
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
      xlim = c(0, max(as.integer(y.values[[factors[1]]])))
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

  if(length(factors)==1) {
    for (i in levels(aggregated[[factors[1]]])){
      coord <- beeswarm::swarmx(x = aggregated[aggregated[[factors[1]]]==i, "x"]
                                , y = aggregated[aggregated[[factors[1]]]==i, dv]
                                , cex = .6
                )
      aggregated[aggregated[[factors[1]]]==i, "swarmx"] <- coord[["x"]]
      aggregated[aggregated[[factors[1]]]==i, "swarmy"] <- coord[["y"]]
    }
  }
  if(length(factors)>1) {
    for (i in levels(aggregated[[factors[1]]])) {
      for (j in levels(aggregated[[factors[2]]])) {
        coord <- beeswarm::swarmx(x = aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "x"]
                                  , y = aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, dv]
                                  , cex = .6
                  )
        aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "swarmx"] <-coord[["x"]]
        aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "swarmy"] <- coord[["y"]]
      }
    }
  }




  # prepare defaults for x axis
  args.axis <- defaults(args.axis
                        , set = list(
                          side = 1
                        )
                        , set.if.null = list(
                          at = 1:nlevels(y.values[[factors[1]]])-.5
                          , labels = levels(y.values[[factors[1]]])
                        )
  )


  # only draw axis if axis type is not specified or not specified as "n"
  if(is.null(args.axis$xaxt)||args.axis$xaxt!="n") {
    do.call("axis", args.axis)
  }

  # convert to matrices
  x <- tapply(y.values[, "x"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)
  y <- tapply(y.values[, "tendency"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)
  e <- tapply(y.values[, "dispersion"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)

  agg.x <- tapply(aggregated[, "swarmx"], list(aggregated[[factors[1]]], aggregated[[factors[2]]]), as.numeric)
  agg.y <- tapply(aggregated[, "swarmy"], list(aggregated[[factors[1]]], aggregated[[factors[2]]]), as.numeric)

  nc <- nlevels(aggregated[[factors[2]]])

  bg.colors <- grey((nc:1/(nc)) ^ 0.6, alpha = .4)
  if(!is.null(args.points$bg)){
    tmp <- col2rgb(args.points$bg, alpha = TRUE)
    tmp <- matrix(tmp, ncol = ncol(agg.x), nrow = 4)
    for (j in 1:ncol(agg.x)){
      bg.colors[j] <- rgb(r = tmp[1, j], g = tmp[2, j], b = tmp[3, j], alpha = tmp[4, j] * .4, maxColorValue = 255)
    }
  }

  for (i in 1:nrow(agg.x)) {
    for (j in 1:ncol(agg.x)) {
      points(x=agg.x[i,j][[1]], y = agg.y[i,j][[1]], pch = c(21:25,1:20)[j], bg = bg.colors[j], col = rgb(r = 0, g = 0, b = 0, alpha = .4), cex = .6)
    }
  }


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


  # prepare and draw points
  args.points <- defaults(args.points
                          , set = list(
                            x = x
                            , y = y
                          )
                          , set.if.null = list(
                            pch = c(21:25,1:20)
                            , col = rep("black", length(l2))
                            , bg = rep("black", length(l2))
                            , cex = rep(1.0, length(l2))
                          )
  )

  do.call("points.matrix", args.points)

#   # prepare and draw lines
#   args.lines <- defaults(args.lines
#                          , set = list(
#                            x = x
#                            , y = y
#                          )
#                          , set.if.null = list(
#                            lty = 1:6
#                            , col = rep("black", length(l2))
#                          )
#   )

  # do.call("lines", args.lines)



  # prepare and draw legend
  if(onedim==FALSE) {

    args.legend <- defaults(args.legend
                            , set.if.null = list(
                              x = "topright"
                              , legend = levels(y.values[[factors[2]]])
                              , pch = args.points$pch[1:nlevels(y.values[[factors[2]]])]
                              , lty = args.lines$lty
                              , bty = "n"
                              , pt.bg = args.points$bg
                              , pt.cex = args.points$cex
                            ))

    do.call("legend", args.legend)
  }

  # draw intercept

  if(!is.null(intercept)){
    if(is.matrix(intercept)) {
      diff <- (ellipsis$xlim[2] - ellipsis$xlim[1])/(ncol(intercept)-1)
      x.vector <- seq(ellipsis$xlim[1], ellipsis$xlim[2], diff)
      for(i in 1:nrow(intercept)) {
        for (j in 1:ncol(intercept)) {
          lines(x = c(x.vector[j]-(diff/2), x.vector[j]+(diff/2)), y = rep(intercept[i,j], 2))
          # print(list(x = c(x.vector[j]-(diff/2), x.vector[j]+(diff/2)), y = rep(intercept[i,j], 2)))
        }
      }
    } else {
      lines(x = ellipsis$xlim, y = rep(intercept,2))
    }
  }
  return(list(ellipsis, args.axis, args.points, args.lines, args.legend))
}



#' @rdname apa_beeplot
#' @export

apa_beeplot <- function(x, ...){
  UseMethod("apa_beeplot")
}


#' @rdname apa_beeplot
#' @method apa_beeplot afex_aov
#' @export

apa_beeplot.afex_aov <- function(x, ...){

  ellipsis <- list(...)

  args <- attributes(x)

  ellipsis <- defaults(
    ellipsis
    , set = list(
      "data" = x$data$long
      , "id" = args$id
      , "dv" = args$dv
      , "factors" = c(args$between, args$within)
    )
  )
  do.call("apa_beeplot.default", ellipsis)
}
