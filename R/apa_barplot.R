#' Barplots for factorial designs that conform to APA guidelines
#'
#' Wrapper function that creates one or more barplots from a data.frame containing data from
#' a factorial design and sets APA-friendly defaults.
#'
#'
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
#'
#' @import grDevices
#' @import graphics
#' @rdname apa_barplot
#' @family plots for factorial designs
#' @export

apa_barplot <- function(data, ...){
  UseMethod("apa_barplot", data)
}

#' @rdname apa_barplot
#' @export

apa_barplot.default <- function(
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
  , args_arrows = list()
  , args_legend = list()
  , ...
){
  ellipsis <- defaults(
    list(...)
    , set.if.null = list(
      data = data
      , id = id
      , factors = factors
      , dv = dv
      , tendency = substitute(tendency)
      , dispersion = substitute(dispersion)
      , level = level
      , fun_aggregate = substitute(fun_aggregate)
      , na.rm = na.rm
      , reference = reference
      , intercept = intercept
      , args_arrows = args_arrows
      , args_legend = args_legend
      , plot = c("bars", "error_bars")
    )
  )
  do.call("apa_generic_plot", ellipsis)
}

#' @rdname apa_barplot
#' @export

apa_barplot.afex_aov <- function(
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
      , "plot" = c("bars", "error_bars")
      , "tendency" = substitute(tendency)
      , "dispersion" = substitute(dispersion)
      , "fun_aggregate" = substitute(fun_aggregate)
    )
  )
  do.call("apa_generic_plot.afex_aov", ellipsis)
}


#   validate(data, check_class = "data.frame", check_NA = FALSE)
#   validate(id, check_class="character", check_length = 1)
#   validate(factors, check_class = "character")
#   validate(length(factors), check_range = c(1,4))
#   validate(tendency, check_class = "function", check_length = 1, check_NA =FALSE)
#   validate(dispersion, check_class = "function", check_length = 1, check_NA = FALSE)
#   validate(level, check_class = "numeric", check_range = c(0,1))
#   validate(fun_aggregate, check_class = "function", check_length = 1, check_NA = FALSE)
#   validate(na.rm, check_class = "logical", check_length = 1)
#   validate(data, check_class = "data.frame", check_cols = c(id, dv, factors), check_NA = FALSE)
#   if(!is.null(intercept)) validate(intercept, check_class = "numeric")
#
#   # remove extraneous columns from dataset
#   data <- data[, c(id, factors, dv)]
#
#   # Add missing variable labels
#   data <- default_label(data)
#
#   # temporarily save variable_labels
#   pretty_labels <- variable_label(data)
#
#   # Handling of factors:
#   # a) convert to factor
#   for (i in factors){
#     data[[i]] <- as.factor(data[[i]])
#   }
#
#   # b) drop factor levels
#   data <- droplevels(data)
#
#   # write variable labels back to data.frame
#   variable_label(data) <- pretty_labels
#
#   ellipsis <- list(...)
#   output <- list()
#
#   # compatibility: allows aggregation function to be specified via "fun.aggregate"
#   if(!is.null(ellipsis$fun.aggregate)) {
#     fun_aggregate <- ellipsis$fun.aggregate
#   }
#   ellipsis$fun.aggregate <- NULL
#
#   # strip whitespace from factor names
#   factors <- gsub(pattern = " ", replacement = "_", factors)
#   id <- gsub(pattern = " ", replacement = "_", id)
#   dv <- gsub(pattern = " ", replacement = "_", dv)
#   colnames(data) <- gsub(pattern = " ", replacement = "_", colnames(data))
#
#
#
#   # is dplyr available?
#   use_dplyr <- package_available("dplyr")
#
#   if(use_dplyr) {
#     ## Aggregate subject data
#     aggregated <- fast_aggregate(data = data, dv = dv, factors = c(id, factors), fun = fun_aggregate)
#
#     ## Calculate central tendencies
#     yy <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = tendency)
#   } else {
#     ## Aggregate subject data
#     aggregated <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(c(id, factors), collapse = "*"))), data = data, FUN = fun_aggregate)
#
#     ## Calculate central tendencies
#     yy <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = tendency)
#   }
#
#
#   ## Calculate dispersions
#   fun_dispersion <- deparse(substitute(dispersion))
#   if(fun_dispersion == "within_subjects_conf_int" || fun_dispersion == "wsci") {
#     ee <- wsci(data = aggregated, id = id, factors = factors, level = level, method = "Morey", dv = dv)
#   } else {
#     if(fun_dispersion == "conf_int") {
#       ee <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion, level = level)
#     } else {
#       if(use_dplyr) {
#         ee <- fast_aggregate(data = aggregated, factors = factors, dv = dv, fun = dispersion)
#       } else {
#         ee <- stats::aggregate(formula = stats::as.formula(paste0(dv, "~", paste(factors, collapse = "*"))), data = aggregated, FUN = dispersion)
#       }
#     }
#   }
#
#   colnames(yy)[which(colnames(yy)==dv)] <- "tendency"
#   colnames(ee)[which(colnames(ee)==dv)] <- "dispersion"
#
#   y.values <- merge(yy, ee, by = factors)
#
#   y.values$lower_limit <- apply(X = y.values[, c("tendency", "dispersion")], MARGIN = 1, FUN = function(x){sum(x[1], -x[2], na.rm = TRUE)})
#   y.values$upper_limit <- apply(X = y.values[, c("tendency", "dispersion")], MARGIN = 1, FUN = sum, na.rm = TRUE)
#
#   output$y <- y.values
#
#
#   # Set defaults
#   ellipsis <- defaults(ellipsis,
#                        set = list(
#                          id = id
#                          , dv = dv
#                          , factors = factors
#                          , legend.text = FALSE
#                        )
#                        , set.if.null = list(
#                          xlab = variable_label(data[[factors[1]]])
#                          , ylab = variable_label(data[[dv]])
#                          #, bty = "n"
#                          , names.arg = levels(data[[factors[1]]])
#                          , axis.lty = 1
#                          , ylim = c(min(reference, y.values[, "lower_limit"], na.rm = TRUE), max(reference, y.values[, "upper_limit"], na.rm = TRUE))
#                          , args.arrows = args_arrows
#                          , args.legend = args_legend
#                        ))
#
#
#
#   # defaults for legend, only necessary if more than one factor is supplied
#   if(length(factors) > 1) {
#     ellipsis$args.legend <- defaults(
#       ellipsis$args.legend
#       , set = list(
#         # nothing
#       )
#       , set.if.null = list(
#         title = variable_label(data[[factors[2]]])
#       ))
#   }
#
#   # allows use to suppress legend title via specifying title = ""
#   if(!is.null(ellipsis$args.legend$title)&&!is.expression(ellipsis$args.legend$title) && ellipsis$args.legend$title == "") {
#     ellipsis$args.legend$title <- NULL # Save space
#   }
#
#   # warning if "beside = FALSE" is specified
#   if(is.null(ellipsis$beside) || !(ellipsis$beside)) {
#     if(!is.null(ellipsis$beside) && !(ellipsis$beside)) warning("Stacked barplots are not supported. Ignoring parameter 'beside = FALSE'.")
#     ellipsis$beside <- TRUE
#   }
#
#
#   # colors
#   if(is.null(ellipsis$col)) {
#     if(length(factors) == 1){
#       ellipsis$col <- "white"
#     } else {
#       nc <- nlevels(data[[factors[2]]])
#       colors <- (nc:1/(nc)) ^ 0.6
#       ellipsis$col <- grey(colors)
#     }
#   }
#
#   ellipsis$intercept <- intercept
#   ellipsis$reference <- reference
#
#   # Plot
#   ## One or two factors
#   if(length(factors) < 3){
#
#     ellipsis <- defaults(
#       ellipsis
#       , set = list(
#         y.values = y.values
#       )
#     )
#
#     do.call("apa.barplot.core", ellipsis)
#   }
#
#   ## Three factors
#   old.mfrow <- par()$mfrow # Save original plot architecture
#
#   if(length(factors) == 3) {
#     par(mfrow = c(1, nlevels(data[[factors[3]]])))
#     tmp_main <- ellipsis$main
#     tmp_plot <- 1:nlevels(data[[factors[3]]])==nlevels(data[[factors[3]]])
#     names(tmp_plot) <- levels(data[[factors[3]]])
#
#     ellipsis$args.legend <- defaults(ellipsis$args.legend
#                                      , set = list(
#
#                                      )
#                                      , set.if.null = list(
#                                        plot = tmp_plot
#                                      )
#     )
#
#     if(length(ellipsis$args.legend$plot)!=nlevels(data[[factors[3]]])) {
#       rec <- length(ellipsis$args.legend$plot) / nlevels(data[[factors[3]]])
#       ellipsis$args.legend$plot <- rep(ellipsis$args.legend$plot, round(rec+1))
#     }
#
#     names(ellipsis$args.legend$plot) <- levels(data[[factors[3]]])
#
#     for (i in levels(data[[factors[3]]])) {
#       ellipsis.i <-defaults(
#         ellipsis
#         , set = list(
#           main = papaja:::combine_plotmath(list(tmp_main, variable_label(data[[factors[3]]]), ": ", i))
#           , y.values = y.values[y.values[[factors[3]]]==i,]
#         )
#       )
#
#       # by default, only draw legend in very right plot
#       ellipsis.i$args.legend <- defaults(ellipsis.i$args.legend, set = list(plot = ellipsis$args.legend$plot[i]))
#
#       do.call("apa.barplot.core", ellipsis.i)
#     }
#     par(mfrow=old.mfrow)
#   }
#
#   ## Four factors
#   if(length(factors)==4){
#     par(mfrow=c(nlevels(data[[factors[3]]]),nlevels(data[[factors[4]]])))
#     tmp_main <- ellipsis$main
#     legend.plot <- array(FALSE, dim=c(nlevels(data[[factors[3]]]), nlevels(data[[factors[4]]])))
#     legend.plot[1,nlevels(data[[factors[4]]])] <- TRUE
#
#     ellipsis$args.legend <- defaults(ellipsis$args.legend
#                                      , set = list(
#                                      )
#                                      , set.if.null = list(
#                                        plot = legend.plot
#                                      )
#     )
#     rownames(ellipsis$args.legend$plot) <- levels(data[[factors[3]]])
#     colnames(ellipsis$args.legend$plot) <- levels(data[[factors[4]]])
#
#     for (i in levels(data[[factors[3]]])){
#       for (j in levels(data[[factors[4]]])){
#
#         ellipsis.ij <- defaults(
#           ellipsis
#           , set = list(
#             main = papaja:::combine_plotmath(list(tmp_main, variable_label(data[[factors[3]]]), ": ", i, " & ", variable_label(data[[factors[4]]]), ": ", j))
#             , y.values = y.values[y.values[[factors[3]]]==i&y.values[[factors[4]]]==j,]
#           )
#         )
#
#         # by default, only draw legend in topright plot
#         ellipsis.ij$args.legend <- defaults(ellipsis.ij$args.legend, set = list(plot = ellipsis$args.legend$plot[i, j]))
#
#         do.call("apa.barplot.core", ellipsis.ij)
#       }
#     }
#     par(mfrow=old.mfrow)
#   }
#   invisible(output)
# }
#
#
# apa.barplot.core<-function(y.values, id, dv, factors, ...) {
#
#   if(length(factors) > 1) {
#     # convert to matrices
#     y <- tapply(y.values[, "tendency"],list(y.values[, factors[2]], y.values[, factors[1]]), FUN=as.numeric)
#     e <- tapply(y.values[, "dispersion"],list(y.values[, factors[2]], y.values[, factors[1]]), FUN=as.numeric)
#     onedim <- FALSE
#   } else {
#     factors[2] <- "f2"
#     y.values[["f2"]] <- as.factor(1)
#     y <- y.values[, "tendency"]
#     e <- y.values[, "dispersion"]
#     onedim <- TRUE
#   }
#
#   space <- .2
#
#   x0 <- as.integer(y.values[[factors[1]]]) - 1 + space/2 + (1-space)/nlevels(y.values[[factors[[2]]]]) * (as.integer(y.values[[factors[2]]])-1)
#   x1 <- as.integer(y.values[[factors[1]]]) - 1 + space/2 + (1-space)/nlevels(y.values[[factors[[2]]]]) * (as.integer(y.values[[factors[2]]]))
#   xf1 <- (x0 + x1)/2
#   l2 <- levels(y.values[[factors[2]]])
#
#   ellipsis <- list(...)
#
#   # save parameters for multiple plot functions
#   args.legend <- ellipsis$args.legend
#   args.plot.window <- ellipsis$args.plot.window
#   args.points <- ellipsis$args.points
#   args.lines <- ellipsis$args.lines
#   args.axis <- ellipsis$args.axis
#   args.arrows <- ellipsis$args.arrows
#   args.y.axis <- list()
#   args.title <- list()
#
#   # basic plot
#   plot.new()
#
#   # plot.window
#   args.plot.window <- defaults(
#     args.plot.window
#     , set.if.null = list(
#       xlim = c(0, max(as.integer(y.values[[factors[1]]])))
#       , ylim = ellipsis$ylim
#     )
#     , set = list(
#     )
#   )
#
#   do.call("plot.window", args.plot.window)
#
#   # prepare defaults for x axis
#   args.axis <- defaults(args.axis
#                         , set = list(
#                           side = 1
#                         )
#                         , set.if.null = list(
#                           at = 1:nlevels(y.values[[factors[1]]]) - .5
#                           , labels = levels(y.values[[factors[1]]])
#                           , tick = TRUE # ifelse(ellipsis$ylim[1]==0, FALSE, TRUE)
#                           , lwd = ifelse(ellipsis$ylim[1]==ellipsis$reference, 0, 1)
#                           , lwd.tick = 1
#                           , pos = ifelse(ellipsis$ylim[1]==ellipsis$reference, ellipsis$ylim[1], ellipsis$ylim[1] - (ellipsis$ylim[2] - ellipsis$ylim[1]) * .02)
#                         )
#   )
#
#
#   # only draw axis if axis type is not specified or not specified as "n"
#   if(is.null(args.axis$xaxt)||args.axis$xaxt!="n") {
#     do.call("axis", args.axis)
#   }
#
#   # prepare defaults for y axis
#   args.y.axis <- defaults(
#     args.y.axis
#     , set = list(
#       side = 2
#     )
#     , set.if.null = list(
#       labels = TRUE
#       , las = ellipsis$las
#     )
#   )
#
#   do.call("axis", args.y.axis)
#
#   abline(h = ellipsis$reference)
#
#   # prepare defaults for title and labels
#   args.title <- defaults(
#     args.title
#     , set = list(
#
#     )
#     , set.if.null = list(
#       main = ellipsis$main
#       ,  xlab = ellipsis$xlab
#       , ylab = ellipsis$ylab
#     )
#   )
#
#   do.call("title", args.title)
#
#   y.values[["col"]] <- ellipsis$col[as.integer(y.values[[factors[2]]])]
#
#   args.rect <- defaults(
#     list()
#     , set.if.null = list(
#
#       xleft = x0
#       , xright = x1
#       , ytop = y.values[["tendency"]]
#       , ybottom = ifelse(
#           ellipsis$ylim[1] < ellipsis$ylim[2] # increasing ylab
#           , ifelse(ellipsis$ylim[1] > ellipsis$reference, ellipsis$ylim[1], ellipsis$reference)
#           , ifelse(ellipsis$ylim[1] < ellipsis$reference, ellipsis$ylim[1], ellipsis$reference)
#       )
#     )
#     , set = list(
#       col = y.values[["col"]]
#       , xpd = FALSE
#     )
#   )
#
#   do.call("rect", args.rect)
#
#   # convert to matrices
#   x <- tapply(xf1 ,list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)
#   y <- tapply(y.values[, "tendency"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)
#   e <- tapply(y.values[, "dispersion"],list(y.values[[factors[1]]], y.values[[factors[2]]]), as.numeric)
#
#   # prepare and draw arrows (i.e., error bars)
#   args.arrows <- defaults(args.arrows
#                           , set = list(
#                             x0 = t(x)
#                             , x1 = t(x)
#                             , y0 = t(y-e)
#                             , y1 = t(y+e)
#                           )
#                           , set.if.null = list(
#                             angle = 90
#                             , code = 3
#                             , length = (1-space)/nlevels(y.values[[factors[[2]]]]) * .125
#                             #, length = ifelse(prod(dim(as.matrix(barx))) < 8, .1, 1/max(barx))
#                           )
#   )
#
#   # print(y.values)
#   do.call("arrows", args.arrows)
#
#   # prepare and draw legend
#   if(onedim==FALSE) {
#
#     args.legend <- defaults(args.legend
#                             , set.if.null = list(
#                               x = "topright"
#                               , legend = levels(y.values[[factors[2]]])
#                               , fill = ellipsis$col
#                               , bty = "n"
#                             ))
#
#     do.call("legend", args.legend)
#   }
#
#
#    if(!is.null(ellipsis$intercept)){
#      space <- 0
#      x0 <- as.integer(y.values[[factors[1]]]) - 1 + space/2 + (1-space)/nlevels(y.values[[factors[[2]]]]) * (as.integer(y.values[[factors[2]]])-1)
#      x1 <- as.integer(y.values[[factors[1]]]) - 1 + space/2 + (1-space)/nlevels(y.values[[factors[[2]]]]) * (as.integer(y.values[[factors[2]]]))
#      segments(x0=x0, y0=ellipsis$intercept, x1=x1, y1=ellipsis$intercept)
#    }
# }
