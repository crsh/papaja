#' Generic plots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more generic plots segfjoiswefjplaesfvafaf
#'
#'
#' @param data A \code{data.frame} that contains the data or an object of class \code{afex_aov}
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
#' @param args_lines An optional \code{list} that contains further arguments that may be passed to \code{\link{lines}}. With \code{list(type = "l")} you can add lines to your plot.
#' @param args_swarm An optional \code{list} that contains further arguments to customize the \code{\link{points}} of the beeswarm.
#' @param args_arrows An optional \code{list} that contains further arguments that may be passed to \code{\link{arrows}}
#' @param args_legend An optional \code{list} that contains further arguments that may be passed to \code{\link{legend}}
#' @param ... Further arguments than can be passed to \code{\link{plot}} function.
#' @details The measure of dispersion can be either \code{conf_int} for confidence intervals, \code{se} for standard errors,
#'    or any other standard function. If \code{conf_int} is specified, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98\% confidence interval, specify
#'    \code{level = 0.98}. \code{level} defaults to 0.95.
#' @seealso \code{\link{barplot}}
#'
#' @export


apa_plot_skeleton <-function(data, ...){
  UseMethod("apa_plot_skeleton", data)
}

#' @export

apa_plot_skeleton.default <- function(
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
  , args_swarm = list()
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
  validate(data, check_class = "data.frame", check_cols = c(id, dv, factors), check_NA = FALSE)
  if(!is.null(intercept)) validate(intercept, check_mode = "numeric")

  # remove extraneous columns from dataset
  data <- data[, c(id, factors, dv)]

  # Add missing variable labels
  data <- default_label(data)

  # temporarily save variable_labels
  pretty_labels <- variable_label(data)

  # Handling of factors:
  # a) convert to factor
  for (i in factors){
    data[[i]] <- as.factor(data[[i]])
  }

  # b) drop factor levels
  data <- droplevels(data)

  # write variable labels back to data.frame
  variable_label(data) <- pretty_labels

  ellipsis <- list(...)
  output <- list()

  # Set defaults
  ellipsis <- defaults(
    ellipsis
    , set = list(
       id = id
       , dv = dv
       , factors = factors
       , intercept = intercept
       , reference = NULL
     )
     , set.if.null = list(
       args.axis = args_axis
       , args.points = args_points
       , args.swarm = args_swarm
       , args.lines = args_lines
       , args.arrows = args_arrows
       , args.legend = args_legend
       , xlab = papaja:::combine_plotmath(list(variable_label(data[[factors[1]]]), ""))
       , ylab = papaja:::combine_plotmath(list(variable_label(data[[dv]]), ""))
       , frame.plot = FALSE
     )
  )

  if(length(ellipsis$args.legend$title) == 0) {
    ellipsis$args.legend$title <- factors[2]
  } else if(!is.expression(ellipsis$args.legend$title) && ellipsis$args.legend$title == "") {
    ellipsis$args.legend$title <- NULL # Save space
  }

  # compatibility: allows aggregation function to be specified via "fun.aggregate"
  if(!is.null(ellipsis$fun.aggregate)) {
    fun_aggregate <- ellipsis$fun.aggregate
  }
  ellipsis$fun.aggregate <- NULL

  # further compatibility stuff: ignore arguments from other plot functions
  ellipsis$reference <- NULL

  # is dplyr available?
  use_dplyr <- package_available("dplyr")

  # strip whitespace from factor names
  factors <- gsub(pattern = " ", replacement = "_", factors)
  id <- gsub(pattern = " ", replacement = "_", id)
  dv <- gsub(pattern = " ", replacement = "_", dv)
  colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))


  ## Aggregate subject data
  if(use_dplyr) {
    aggregated <- fast_aggregate(data = data, dv = dv, factors = c(id, factors), fun = fun_aggregate)
  } else {
    aggregated <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(c(id, factors), collapse = "*"))), data = data, FUN = fun_aggregate)
  }

  ## Calculate central tendencies
  if(use_dplyr) {
    yy <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency)
  } else {
    yy <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = tendency)
  }

  ## Calculate dispersions
  fun_dispersion <- deparse(substitute(dispersion))
  if(fun_dispersion == "within_subjects_conf_int" || fun_dispersion == "wsci") {
    ee <- wsci(data = aggregated, id = id, factors = factors, level = level, method = "Morey", dv = dv)
  } else {

    if(fun_dispersion == "conf_int") {
      ee <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion, level = level)
    } else {
      if(use_dplyr) {
        ee <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = dispersion)
      } else {
        ee <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion)
      }
    }
  }

  colnames(yy)[which(colnames(yy)==dv)] <- "tendency"
  colnames(ee)[which(colnames(ee)==dv)] <- "dispersion"

  y.values <- merge(yy, ee, by = factors)

  y.values$lower_limit <- apply(X = y.values[, c("tendency", "dispersion")], MARGIN = 1, FUN = function(x){sum(x[1], -x[2], na.rm = TRUE)})
  y.values$upper_limit <- apply(X = y.values[, c("tendency", "dispersion")], MARGIN = 1, FUN = sum, na.rm = TRUE)

  output$y <- y.values

  output$data <- aggregated

  output$args <- list()


  ## Adjust ylim to height of error bars and ensure that all points of the swarm are plotted
  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      ylim = c(
        min(
          0
          , y.values[, "lower_limit"]
          , aggregated[, dv]
          , na.rm = TRUE
        )
        , max(
          y.values[, "upper_limit"]
          , aggregated[, dv]
          , na.rm = TRUE
        )
      )
    )
  )


  ## zero to two factors
  if(length(factors) < 3){

    ellipsis <- defaults(
      ellipsis
      , set = list(
        y.values = y.values
        , aggregated = aggregated
      )
      , set.if.null = list(

      ))

    output$args <- do.call("apa_plot_skeleton_single", ellipsis)
  }

  old.mfrow <- par("mfrow") # Save original plot architecture
  ## Three factors


  if(length(factors) == 3) {
    par(mfrow = c(1, nlevels(data[[factors[3]]])))
    tmp_main <- ellipsis$main

    # by default, only plot legend in topright plot:
    tmp_plot <- 1:nlevels(data[[factors[3]]])==nlevels(data[[factors[3]]])
    names(tmp_plot) <- levels(data[[factors[3]]])

    ellipsis$args.legend <- defaults(
      ellipsis$args.legend
      , set = list(
        # nothing
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
        y.values = y.values[y.values[[factors[3]]]==i, ]
        , aggregated = aggregated[aggregated[[factors[3]]]==i, ]
      ), set.if.null = list(
        main = papaja:::combine_plotmath(list(tmp_main, variable_label(data[[factors[3]]]), ": ", i))
      ))

      # by default, only draw legend in very right plot
      ellipsis.i$args.legend <- defaults(ellipsis.i$args.legend, set = list(plot = ellipsis$args.legend$plot[i]))

      # suppresses ylab
      if(i!=levels(y.values[[factors[3]]])[1]){
        ellipsis.i$ylab <- ""
      }

      output$args[[paste0("plot", i)]] <- do.call("apa_plot_skeleton_single", ellipsis.i)
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
          y.values = y.values[y.values[[factors[3]]]==i&y.values[[factors[4]]]==j,]
          , aggregated = aggregated[aggregated[[factors[3]]]==i&aggregated[[factors[4]]]==j,]
        ), set.if.null = list(
          main = papaja:::combine_plotmath(list(tmp_main, variable_label(data[[factors[3]]]), ": ", i, " & ", variable_label(data[[factors[4]]]), ": ", j))
        ))

        # by default, only draw legend in topright plot
        ellipsis.i$args.legend <- defaults(ellipsis.i$args.legend, set = list(plot = ellipsis$args.legend$plot[i, j]))

        # suppresses ylab
        if(j!=levels(y.values[[factors[4]]])[1]){
          ellipsis.i$ylab <- ""
        }
        output$args[[paste0("plot", i, j)]] <- do.call("apa_plot_skeleton_single", ellipsis.i)
      }
    }
    par(mfrow=old.mfrow)
  }
  invisible(output)
}


#' @export

apa_plot_skeleton_single <- function(aggregated, y.values, id, dv, factors, intercept = NULL, ...) {

  ellipsis <- list(...)

  # jittering of x coordinates
  if(is.null(ellipsis$jit)){
    ellipsis$jit <- .3
  }

  factors <- gsub(factors, pattern = " ", replacement = "_")
  id <- gsub(id, pattern = " ", replacement = "_")
  dv <- gsub(dv, pattern = " ", replacement = "_")


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

  space <-1 - ellipsis$jit

  y.values$x <- as.integer(y.values[[factors[1]]]) - .5
  aggregated$x <- as.integer(aggregated[[factors[1]]]) - .5

  if(onedim==FALSE){
    y.values$x <- y.values$x - .5  + space/2 + (1-space)/(nlevels(y.values[[factors[[2]]]])-1) * (as.integer(y.values[[factors[2]]])-1)
    aggregated$x <- aggregated$x - .5 + space/2 + (1-space)/(nlevels(aggregated[[factors[[2]]]])-1) * (as.integer(aggregated[[factors[2]]])-1)
  }

  # save parameters for multiple plot functions
  args.legend <- ellipsis$args.legend
  args.points <- ellipsis$args.points
  args.swarm <- ellipsis$args.swarm
  args.lines <- ellipsis$args.lines
  args.axis <- ellipsis$args.axis
  args.arrows <- ellipsis$args.arrows
  tmp_plot <- ellipsis$plot

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
      , args.swarm = NULL
      , plot = NULL
    )
  )

  do.call("plot.default", ellipsis)
  ellipsis$plot <- tmp_plot

  args.swarm <- defaults(
    args.swarm
    , set.if.null = list(
      cex = .5
      , alpha = .3
      , priority = c("ascending", "descending", "density", "random", "none")
    )
  )

  for (i in levels(aggregated[[factors[1]]])) {
    for (j in levels(aggregated[[factors[2]]])) {
      coord <- beeswarm::swarmx(x = aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "x"]
                                , y = aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, dv]
                                , cex = args.swarm$cex
                                , priority = args.swarm$priority
                )
      aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "swarmx"] <- coord[["x"]]
      aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "swarmy"] <- coord[["y"]]
    }
  }

  args.swarm$priority <- NULL


  # prepare x axis
  args.axis <- defaults(
    args.axis
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

  ## default colors for tendency points (which are inherited by swarm points)
  nc <- nlevels(aggregated[[factors[2]]])-1
  if(nc==0) nc <- 1
  bg.colors <- grey((0:nc/(nc)) ^ 0.6)

  # prepare (tendency) points
  args.points <- defaults(
    args.points
    , set = list(
      x = x
      , y = y
    )
    , set.if.null = list(
      pch = c(21:25,1:20)
      , col = rep("black", length(l2))
      , bg = bg.colors
      , cex = rep(1.0, length(l2))
    )
  )


  if("swarm" %in% ellipsis$plot){
    args.swarm <- defaults(
      args.swarm
      , set = list(
        # nothing yet
      )
      , set.if.null = list(
        x = agg.x
        , y = agg.y
        , col = brighten(args.points$col, factor = .9)
        , bg = brighten(args.points$bg, factor = .9)
        , pch = args.points$pch
      )
    )

    args.swarm$alpha <- NULL

    do.call("points.matrix", args.swarm)
  }


  # prepare and draw lines
  if("lines" %in% ellipsis$plot){
    args.lines <- defaults(
      args.lines
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
  }

  # prepare and draw error bars
  if("errorbar" %in% ellipsis$plot){
    args.arrows <- defaults(
      args.arrows
      , set = list(
        x0 = t(x)
        , x1 = t(x)
        , y0 = t(y-e)
        , y1 = t(y+e)
      )
      , set.if.null = list(
        angle = 90, code = 3, length = .1
      )
    )

    # draw arrows (measure of dispersion)
    do.call("arrows", args.arrows)
  }

  if("points" %in% ellipsis$plot){
    # draw points (central tendency)
    do.call("points.matrix", args.points)
  }

  # prepare and draw legend
  if(onedim==FALSE) { # only draw legend if a second factor is present

    args.legend <- defaults(
      args.legend
      , set.if.null = list(
        x = "topright"
        , legend = levels(y.values[[factors[2]]])
        , pch = args.points$pch[1:nlevels(y.values[[factors[2]]])]
        , lty = args.lines$lty
        , bty = "n"
        , pt.bg = args.points$bg
        , col = args.points$col
        , pt.cex = args.points$cex
      )
    )

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
  invisible(list(
    "plot" = ellipsis
    , "axis" = args.axis
    , "points" = args.points
    , "swarm" = args.swarm
    , "legend" = args.legend))
}


#' @rdname apa_plot_skeleton
#' @export

apa_plot_skeleton.afex_aov <- function(data, ...){

  ellipsis <- list(...)

  args <- attributes(data)

  ellipsis <- defaults(
    ellipsis
    , set = list(
      "data" = data$data$long
      , "id" = args$id
      , "dv" = args$dv
      , "factors" = c(args$between, args$within)
    )
  )
  do.call("apa_plot_skeleton.default", ellipsis)
}
