#' Plots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more plots. \code{apa_generic_plot} is the workhorse function that is called
#' by \code{\link{apa_barplot}}, \code{\link{apa_beeplot}}, and \code{\link{apa_lineplot}}.
#'
#' If required by parameter \code{plot}, it sequentially calls
#' \code{\link{plot.new()}},
#' \code{\link{plot.window}},
#' \code{\link{axis}},
#'
#'
#' @param data A \code{data.frame} that contains the data or an object of class \code{afex_aov}
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of up to 4 variable names that is used to stratify the data.
#' @param dv Character. The name of the dependent variable.
#' @param tendency Closure. A function that will be used as measure of central tendency.
#' @param dispersion Closure. A function that will be used to construct error bars (i.e., whiskers). Defaults to
#'    \code{conf_int} for 95\% between-subjects confidence intervals. See details.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to 0.95
#'    for 95\% confidence intervals. Ignored if \code{dispersion} is not a confidence-interval function. See details.
#' @param fun_aggregate Closure. The function that will be used to aggregate observations within subjects and factors
#'    before calculating descriptive statistics for each cell of the design. Defaults to \code{mean}.
#' @param na.rm Logical. Specifies if missing values are removed. Defaults to \code{TRUE}.
#' @param reference Numeric. A reference point that determines the y-coordinate of the x-axis.
#' @param intercept Numeric. Adds a horizontal line to the plot. Can be either a single value or a matrix. For the matrix
#'    case, multiple lines are drawn, where the dimensions of the matrix determine the number of lines to be drawn.
#' @param plot Character. A vector specifying which elements of the plot should be plotted. Available options are
#'  \code{c("points", "error_bars", "bars", "swarms")}
#' @param jit Numeric.
#' @param args_x_axis An optional \code{list} that contains further arguments that may be passed to \code{\link{axis}}.
#' @param args_y_axis An optional \code{list} that contains further arguments that may be passed to \code{\link{axis}}.
#' @param args_points An optional \code{list} that contains further arguments that may be passed to \code{\link{points}}.
#' @param args_lines An optional \code{list} that contains further arguments that may be passed to \code{\link{lines}}.
#' @param args_swarm An optional \code{list} that contains further arguments to customize the \code{\link{points}} of the beeswarm.
#' @param args_error_bars An optional \code{list} that contains further arguments that may be passed to \code{\link{arrows}}.
#' @param args_legend An optional \code{list} that contains further arguments that may be passed to \code{\link{legend}}
#' @param ... Further arguments that will be passed to \code{\link{plot.window}}.
#' @details The measure of dispersion can be either \code{conf_int} for between-subjects confidence intervals, \code{se} for standard errors,
#'    or any other standard function.
#'    For within-subjects confidence intervals, specify \code{wsci} or \code{within_subjects_conf_int}
#'    If between- or within-subjects confidence intervals are requested, you can also specify the area of the cumulative
#'    distribution function that will be covered. For instance, if you want a 98\% confidence interval, specify
#'    \code{level = 0.98}. \code{level} defaults to 0.95.
#' @family plots for factorial designs
#' @examples
#' apa_generic_plot(
#'   data = npk
#'   , id = "block"
#'   , dv = "yield"
#'   , factors = c("N", "P", "K")
#'   , las = 1
#'   , plot = c("error_bars", "points", "swarms")
#' )
#'
#' @export


apa_generic_plot <-function(data, ...){
  UseMethod("apa_generic_plot", data)
}

#' @rdname apa_generic_plot
#' @export

apa_generic_plot.default <- function(
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
  , args_points = NULL
  , args_swarm = NULL
  , args_lines = NULL
  , args_error_bars = NULL
  , args_legend = NULL
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
  validate(data, check_class = "data.frame", check_cols = c(id, dv, factors), check_NA = FALSE)
  if(!is.null(intercept)) validate(intercept, check_mode = "numeric", check_NA = FALSE)

  # remove extraneous columns from dataset
  data <- data[, c(id, factors, dv)]

  # Add missing variable labels
  data <- default_label(data)

  # temporarily save variable_labels
  pretty_labels <- variable_label(data)

  # strip whitespace from factor names
  factors <- gsub(pattern = " ", replacement = "_", factors)
  id <- gsub(pattern = " ", replacement = "_", id)
  dv <- gsub(pattern = " ", replacement = "_", dv)
  colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))

  # Handling of factors:
  # a) convert to factor
  for (i in c(id, factors)){
    data[[i]] <- as.factor(data[[i]])
  }

  # b) drop factor levels
  data <- droplevels(data)

  # write variable labels back to data.frame
  variable_label(data) <- pretty_labels

  ellipsis <- list(...)
  output <- list()

  # If no factors were specified, use an arbitrary one
  if(is.null(factors)||length(factors)==0){
    factors <- "arbitrary_factor_name"
    data[[factors]] <- 1
    data[[factors]] <- as.factor(data[[factors]])
    ellipsis$args_x_axis<- list(tick = FALSE, labels = "")
  }

  # Set defaults
  ellipsis <- defaults(
    ellipsis
    , set = list(
       id = id
       , dv = dv
       , factors = factors
       , intercept = intercept
     )
     , set.if.null = list(
       args_x_axis = args_x_axis
       , args_y_axis = args_y_axis
       , args_points = args_points
       , args_swarm = args_swarm
       , args_lines = args_lines
       , args_error_bars = args_error_bars
       , args_legend = args_legend
       , xlab = if(!is.null(factors)){combine_plotmath(list(variable_label(data[[factors[1]]]), ""))}
       , ylab = combine_plotmath(list(variable_label(data[[dv]]), ""))
       , frame.plot = FALSE
       , reference = reference
     )
  )

  # Only use a legend title if more than one factor is specified, allow suppressing the legend title
  if(length(factors)>1){
    if(length(ellipsis$args_legend$title) == 0) {
      ellipsis$args_legend$title <- variable_label(data[[factors[2]]])
    } else if(!is.expression(ellipsis$args_legend$title) && ellipsis$args_legend$title == "") {
      ellipsis$args_legend$title <- NULL # Save space
    }
  }

  # warning if "beside = FALSE" is specified
  if(is.null(ellipsis$beside) || !(ellipsis$beside)) {
    if(!is.null(ellipsis$beside) && !(ellipsis$beside)) warning("Stacked barplots are not supported. Ignoring parameter 'beside = FALSE'.")
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
  if(!is.null(ellipsis$args_axis)) {
    ellipsis$args_x_axis <- ellipsis$args_axis
  }
  ellipsis$args_axis <- NULL


  # Bar colors
  if(is.null(ellipsis$col)) {
    if(length(factors) < 2){
      ellipsis$col <- "white"
    } else {
      nc <- nlevels(data[[factors[2]]])
      colors <- (nc:1/(nc)) ^ 0.6
      ellipsis$col <- grey(colors)
    }
  }

  ellipsis$intercept <- intercept

  # is dplyr available?
  use_dplyr <- package_available("dplyr")

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

    output$args <- do.call("apa_generic_plot_single", ellipsis)
  }

  old.mfrow <- par("mfrow") # Save original plot architecture
  ## Three factors


  if(length(factors) == 3) {
    par(mfrow = c(1, nlevels(data[[factors[3]]])))
    tmp_main <- ellipsis$main

    # by default, only plot legend in topright plot:
    tmp_plot <- 1:nlevels(data[[factors[3]]])==nlevels(data[[factors[3]]])
    names(tmp_plot) <- levels(data[[factors[3]]])

    ellipsis$args_legend <- defaults(
      ellipsis$args_legend
      , set = list(
        # nothing
      )
      , set.if.null = list(
        plot = tmp_plot
      )
    )

    if(is.null(ellipsis$args_legend$plot)) {
      ellipsis$args_legend$plot <- 1:nlevels(data[[factors[3]]])==nlevels(data[[factors[3]]])
    }

    if(length(ellipsis$args_legend$plot)!=nlevels(data[[factors[3]]])) {
      rec <- length(ellipsis$args_legend$plot) / nlevels(data[[factors[3]]])
      ellipsis$args_legend$plot <- rep(ellipsis$args_legend$plot, round(rec+1))
    }

    names(ellipsis$args_legend$plot) <- levels(data[[factors[3]]])

    if(!is.null(ellipsis$main)){
      names(ellipsis$main) <- levels(y.values[[factors[3]]])
    }

    for (i in levels(y.values[[factors[3]]])) {

      ellipsis.i <- defaults(ellipsis, set = list(
        y.values = y.values[y.values[[factors[3]]]==i, ]
        , aggregated = aggregated[aggregated[[factors[3]]]==i, ]
      ), set.if.null = list(
        main = papaja:::combine_plotmath(list(tmp_main, variable_label(data[[factors[3]]]), ": ", i))
      ))
      if(!is.null(ellipsis$main)){
        ellipsis.i$main <- ellipsis$main[i]
      }

      # by default, only draw legend in very right plot
      ellipsis.i$args_legend <- defaults(ellipsis.i$args_legend, set = list(plot = ellipsis$args_legend$plot[i]))

      # suppresses ylab
      if(i!=levels(y.values[[factors[3]]])[1]){
        ellipsis.i$ylab <- ""
      }

      output$args[[paste0("plot", i)]] <- do.call("apa_generic_plot_single", ellipsis.i)
    }
    par(mfrow=old.mfrow)
  }

  ## Four factors
  if(length(factors)==4){
    par(mfrow=c(nlevels(data[[factors[3]]]),nlevels(data[[factors[4]]])))
    tmp_main <- ellipsis$main

    legend.plot <- array(FALSE, dim=c(nlevels(data[[factors[3]]]), nlevels(data[[factors[4]]])))
    legend.plot[1,nlevels(data[[factors[4]]])] <- TRUE

    ellipsis$args_legend <- defaults(ellipsis$args_legend
                                     , set = list(

                                     )
                                     , set.if.null = list(
                                       plot = legend.plot
                                     )
    )
    rownames(ellipsis$args_legend$plot) <- levels(data[[factors[3]]])
    colnames(ellipsis$args_legend$plot) <- levels(data[[factors[4]]])

    if(!is.null(ellipsis$main)){
      rownames(ellipsis$main) <- levels(y.values[[factors[3]]])
      colnames(ellipsis$main) <- levels(y.values[[factors[4]]])
    }


    for (i in levels(y.values[[factors[3]]])){
      for (j in levels(y.values[[factors[4]]])) {
        ellipsis.i <- defaults(ellipsis, set = list(
          y.values = y.values[y.values[[factors[3]]]==i&y.values[[factors[4]]]==j,]
          , aggregated = aggregated[aggregated[[factors[3]]]==i&aggregated[[factors[4]]]==j,]
        ), set.if.null = list(
          main = papaja:::combine_plotmath(list(tmp_main, variable_label(data[[factors[3]]]), ": ", i, " & ", variable_label(data[[factors[4]]]), ": ", j))
        ))
        if(!is.null(ellipsis$main)){
          ellipsis.i$main <- ellipsis$main[i, j]
        }
        # by default, only draw legend in topright plot
        ellipsis.i$args_legend <- defaults(ellipsis.i$args_legend, set = list(plot = ellipsis$args_legend$plot[i, j]))

        # suppresses ylab
        if(j!=levels(y.values[[factors[4]]])[1]){
          ellipsis.i$ylab <- ""
        }
        output$args[[paste0("plot", i, j)]] <- do.call("apa_generic_plot_single", ellipsis.i)
      }
    }
    par(mfrow=old.mfrow)
  }
  invisible(output)
}


#' @export

apa_generic_plot_single <- function(aggregated, y.values, id, dv, factors, intercept = NULL, ...) {

  ellipsis <- list(...)

  # jittering of x coordinates
  if(is.null(ellipsis$jit)){
    ellipsis$jit <- .3
    if("bars" %in% ellipsis$plot){
      ellipsis$jit <- .4
    }
  }


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

  space <- (1 - ellipsis$jit)


  y.values$x <- as.integer(y.values[[factors[1]]]) - .5
  aggregated$x <- as.integer(aggregated[[factors[1]]]) - .5

  if(onedim==FALSE){
    y.values$x <- y.values$x - .5  + space/2 + (1-space)/(nlevels(y.values[[factors[[2]]]])-1) * (as.integer(y.values[[factors[2]]])-1)
    aggregated$x <- aggregated$x - .5 + space/2 + (1-space)/(nlevels(aggregated[[factors[[2]]]])-1) * (as.integer(aggregated[[factors[2]]])-1)
  }

  # save parameters for multiple plot functions
  args_legend <- ellipsis$args_legend
  args_points <- ellipsis$args_points
  args_swarm <- ellipsis$args_swarm
  args_lines <- ellipsis$args_lines
  args_x_axis <- ellipsis$args_x_axis
  args_y_axis <- ellipsis$args_y_axis
  args_error_bars <- ellipsis$args_error_bars
  args_title <- ellipsis$args_title

  # move all arguments that are white-listed
  args_plot_window <- list()
  whitelist <- c("xlim", "ylim", "log", "asp", "xaxs", "yaxs", "len")
  for(i in whitelist)
    args_plot_window[[i]] <- ellipsis[[i]]

  # new plot area
  plot.new()

  # plot.window
  args_plot_window <- defaults(
    args_plot_window
    , set.if.null = list(
      xlim = c(0, max(as.integer(y.values[[factors[1]]])))
      , ylim = ellipsis$ylim
    )
    , set = list(
    )
  )

  do.call("plot.window", args_plot_window)

  # prepare defaults for x-axis
  args_x_axis <- defaults(
    args_x_axis
    , set = list(
      side = 1
    )
    , set.if.null = list(
      at = 1:nlevels(y.values[[factors[1]]]) - .5
      , labels = levels(y.values[[factors[1]]])
      , tick = TRUE # ifelse(ellipsis$ylim[1]==0, FALSE, TRUE)
    )
  )


  # Some modifications are in order if a barplot starts at reference point
  # However, save `lwd`
  tmp_lwd <- ifelse(is.null(args_x_axis$lwd), 1, args_x_axis$lwd)

  if("bars" %in% ellipsis$plot && ellipsis$ylim[1]==ellipsis$reference){

    args_x_axis <- defaults(
      args_x_axis
      , set = list(
        lwd = 0
      )
      , set.if.null = list(
        lwd.tick = 1
        , pos = ellipsis$ylim[1]
      )
    )
  }


  # only draw axis if axis type is not specified or not specified as "n"
  if(is.null(args_x_axis$xaxt)||args_x_axis$xaxt!="n") {
    do.call("axis", args_x_axis)
  }

  # prepare defaults for y-axis
  args_y_axis <- defaults(
    args_y_axis
    , set = list(
      side = 2
    )
    , set.if.null = list(
      labels = TRUE
      , las = ellipsis$las
    )
  )

  do.call("axis", args_y_axis)


  if("bars" %in% ellipsis$plot){

    abline(h = ellipsis$reference, lwd = tmp_lwd)
    space <- .2

    x0 <- as.integer(y.values[[factors[1]]]) - 1 + space/2 + (1-space)/nlevels(y.values[[factors[[2]]]]) * (as.integer(y.values[[factors[2]]])-1)
    x1 <- as.integer(y.values[[factors[1]]]) - 1 + space/2 + (1-space)/nlevels(y.values[[factors[[2]]]]) * (as.integer(y.values[[factors[2]]]))

    y.values$x <- (x0 + x1)/2
    l2 <- levels(y.values[[factors[2]]])

    y.values[["col"]] <- ellipsis$col[as.integer(y.values[[factors[2]]])]

    args_rect <- defaults(
      list()
      , set.if.null = list(

        xleft = x0
        , xright = x1
        , ytop = y.values[["tendency"]]
        , ybottom = ifelse(
          ellipsis$ylim[1] < ellipsis$ylim[2] # Is ylab increasing?
          , ifelse(ellipsis$ylim[1] >= ellipsis$reference, ellipsis$ylim[1], ellipsis$reference) # for increasing ylab
          , ifelse(ellipsis$ylim[1] <= ellipsis$reference, ellipsis$ylim[1], ellipsis$reference) # for decreasing ylab
        )
      )
      , set = list(
        col = y.values[["col"]]
        , xpd = FALSE
      )
    )
    do.call("rect", args_rect)
  }

  # prepare defaults for title and labels
  args_title <- defaults(
    args_title
    , set = list(

    )
    , set.if.null = list(
      main = ellipsis$main
      ,  xlab = ellipsis$xlab
      , ylab = ellipsis$ylab
    )
  )

  do.call("title", args_title)

  if("swarms" %in% ellipsis$plot){
    args_swarm <- defaults(
      args_swarm
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
                                  , cex = args_swarm$cex
                                  , priority = args_swarm$priority
                  )
        aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "swarmx"] <- coord[["x"]]
        aggregated[aggregated[[factors[1]]]==i&aggregated[[factors[2]]]==j, "swarmy"] <- coord[["y"]]
      }
    }

    args_swarm$priority <- NULL
  }


  # prepare x axis
  args_x_axis <- defaults(
    args_x_axis
    , set = list(
      side = 1
    )
    , set.if.null = list(
      at = 1:nlevels(y.values[[factors[1]]])-.5
      , labels = levels(y.values[[factors[1]]])
    )
  )

  # convert to matrices
  x <- tapply(y.values[, "x"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)
  y <- tapply(y.values[, "tendency"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)
  e <- tapply(y.values[, "dispersion"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)

  if("swarms" %in% ellipsis$plot){
    agg.x <- tapply(aggregated[, "swarmx"], list(aggregated[[factors[1]]], aggregated[[factors[2]]]), as.numeric)
    agg.y <- tapply(aggregated[, "swarmy"], list(aggregated[[factors[1]]], aggregated[[factors[2]]]), as.numeric)
  }

  ## default colors for tendency points (which are inherited by swarm points)
  nc <- nlevels(aggregated[[factors[2]]])-1
  if(nc==0) nc <- 1
  bg.colors <- grey((0:nc/(nc)) ^ 0.6)

  # prepare (tendency) points
  args_points <- defaults(
    args_points
    , set = list(
      x = x
      , y = y
    )
    , set.if.null = list(
      pch = c(21:25, 1:20)
      , col = rep("black", length(l2))
      , bg = bg.colors
      , cex = rep(1.0, length(l2))
    )
  )


  if("swarms" %in% ellipsis$plot){
    args_swarm <- defaults(
      args_swarm
      , set = list(
        # nothing yet
      )
      , set.if.null = list(
        x = agg.x
        , y = agg.y
        , col = brighten(args_points$col, factor = .9)
        , bg = brighten(args_points$bg, factor = .9)
        , pch = args_points$pch
      )
    )

    args_swarm$alpha <- NULL

    do.call("points.matrix", args_swarm)
  }


  # prepare and draw (central tendency) lines
  if("lines" %in% ellipsis$plot){
    args_lines <- defaults(
      args_lines
      , set = list(
        x = x
        , y = y
      )
      , set.if.null = list(
        lty = 1:6
        , col = rep("black", length(l2))
      )
    )

    do.call("lines", args_lines)
  }

  # prepare and draw error bars
  if("error_bars" %in% ellipsis$plot){
    args_error_bars <- defaults(
      args_error_bars
      , set = list(
        x0 = t(x)
        , x1 = t(x)
        , y0 = t(y-e)
        , y1 = t(y+e)
      )
      , set.if.null = list(
        angle = 90
        , code = 3
        , length = .06
      )
    )

    do.call("arrows", args_error_bars)
  }

  if("points" %in% ellipsis$plot){
    # draw points (central tendency)
    do.call("points.matrix", args_points)
  }

  # prepare and draw legend
  if(onedim==FALSE) { # only draw legend if a second factor is present

    args_legend <- defaults(
      args_legend
      , set.if.null = list(
        x = "topright"
        , legend = levels(y.values[[factors[2]]])
        , pch = args_points$pch[1:nlevels(y.values[[factors[2]]])]
        , lty = args_lines$lty
        , bty = "n"
        , pt.bg = args_points$bg
        , col = args_points$col
        , pt.cex = args_points$cex
      )
    )
    if("bars" %in% ellipsis$plot){
      args_legend <- defaults(
        args_legend
        , set = list(
          pch = NULL
          , lty = NULL
        )
        , set.if.null = list(
          fill = ellipsis$col
        )
      )
    }
    do.call("legend", args_legend)
  }


  # Draw intercept
  if(!is.null(intercept)){
    if(is.matrix(intercept)) {
      diff <- (args_plot_window$xlim[2] - args_plot_window$xlim[1])/(ncol(intercept)-1)
      x.vector <- seq(args_plot_window$xlim[1], args_plot_window$xlim[2], diff)
      for(i in 1:nrow(intercept)) {
        for (j in 1:ncol(intercept)) {
          lines(x = c(x.vector[j]-(diff/2), x.vector[j]+(diff/2)), y = rep(intercept[i, j], 2))
        }
      }
    } else {
      n_lines <- length(intercept)
      x_coordinates <- seq(args_plot_window$xlim[1], args_plot_window$xlim[2], diff(args_plot_window$xlim)/n_lines)
      for (i in 1:n_lines){
        y_coordinates <- rep(intercept[i], 2)
        lines(x = x_coordinates[(0:1) + i], y = y_coordinates)
      }
    }
  }

  # Invisibly return arguments that were used for plotting
  invisible(
    list(
      args_x_axis = args_x_axis
      , args_y_axis = args_y_axis
      , args_points = args_points
      , args_swarm = args_swarm
      , args_lines = args_lines
      , args_error_bars = args_error_bars
      , args_legend = args_legend
      , args_plot_window =args_plot_window
    )
  )

}


#' @rdname apa_generic_plot
#' @export

apa_generic_plot.afex_aov <- function(
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
  do.call("apa_generic_plot.default", ellipsis)
}
