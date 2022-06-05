
# Helper functions -------------------------------------------------------------


#' @keywords internal

extract <- function(x, i) {
  vapply(X = x, FUN = `[[`, i, FUN.VALUE = numeric(1L))
}



#' @keywords internal

modify_or_add <- function(.x, creator, ...) {

  if(identical(.x$.state, "modify")) {
    old_args <- inherit_args(.x, creator)
    idx <- attr(old_args, "idx_creator")
    ellipsis <- defaults(
      old_args
      , set = list(...)
    )
    if(length(idx) == 0L) {
      ellipsis <- list(...)
      idx <- length(.x$visuals) + 1L
    }
  } else {
    ellipsis <- list(...)
    idx <- length(.x$visuals) + 1L
  }
  list(idx = idx, ellipsis = ellipsis)
}



#' @keywords internal

inherit_args <- function(x, creator = "points") {

  idx_creator <- which(vapply(X = x$visuals, FUN = `[[`, ".creator", FUN.VALUE = character(1L)) == creator)

  if(length(idx_creator)) {
    structure(
      x$visuals[[idx_creator[[1L]]]]$args
      , idx_creator = idx_creator[[1L]]
    )
  } else {
    structure(
      list()
      , idx_creator = integer(0L)
    )
  }
}


#' Bars
#'
#' Add
#'
#' @export

bars <- function(x, ...) {
  UseMethod("bars", x)
}


#' @export

bars.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(x$plots, FUN = bars, ...)
  x
}


#' @export

bars.apa_plot <- function(x, ...) {

  factors <- x$input$factors

  if(length(factors) > 1L) {
    numeric_fct2 <- as.numeric(x$y[[ factors[[2L]] ]])
    levels_fct2 <- levels(x$y[[ factors[[2L]] ]])
    nlevels_fct2 <- length(levels_fct2)
    space <- 1 - x$input$jit / (nlevels_fct2 - 1L) * nlevels_fct2

    nc <- nlevels_fct2
    colors <- (nc:1/(nc)) ^ 0.6
    col <- grey(colors)
  } else {
    numeric_fct2 <- rep(1, nrow(x$y))
    nlevels_fct2 <- 1L
    space <- 1 - x$input$jit
    col <- "white"
  }
  if(length(factors) > 0L) {
    numeric_fct1 <- as.numeric(x$y[[ factors[[1L]] ]])
  } else {
    numeric_fct1 <- rep(1, nrow(x$y))
  }

  x0 <- numeric_fct1 - 1 + space/2 + (1-space)/nlevels_fct2 * (numeric_fct2 - 1)
  x1 <- numeric_fct1 - 1 + space/2 + (1-space)/nlevels_fct2 * (numeric_fct2)


  x$visuals[[length(x$visuals) + 1]] <- list(
    .f = "rect"
    , .creator = "bars"
    , args = defaults(
      list(...)
      , set.if.null = list(
        xpd = FALSE
        , xleft = x0
        , xright = x1
        , ytop = extract(x$y[[ x$input$dv ]], 2L)
        , ybottom = if(x$visuals$plot.window$args$ylim[1L] < x$visuals$plot.window$args$ylim[2L]) {  # Is ylab increasing?
          ifelse(x$visuals$plot.window$args$ylim[1] >= x$input$reference, x$visuals$plot.window$args$ylim[1], x$input$reference) # for increasing ylab
        } else {
          ifelse(x$visuals$plot.window$args$ylim[1] <= x$input$reference, x$visuals$plot.window$args$ylim[1], x$input$reference) # for decreasing ylab
        }
        , col = col
      )
    )
  )
  x$visuals[[length(x$visuals) + 1L]] <- list(
    .f = "abline"
    , .creator = "bars"
    , args = list(
      h = x$input$reference
    )
  )
  x
}





#' @export

points.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(x$plots, FUN = points, ...)
  x
}


#' @export

points.apa_plot <- function(x, ...) {

  tmp <- modify_or_add(x, "points", ...)
  ellipsis <- tmp$ellipsis

  factors <- x$input$factors

  ## default colours for tendency points (which are inherited by swarm points)
  if(length(factors) < 2L) {
    n_colors <- 1L
  } else {
    n_colors <- nlevels(x$y[[factors[[2L]]]])
  }
  bg_colors <- grey(seq(from = 0, to = 1, length.out = n_colors) ^ .6)

  x$visuals[[tmp$idx]] <- list(
    .f = "points"
    , .creator = "points"
    , args = defaults(
      ellipsis
      , set.if.null = list(
        x = x$x
        , y = extract(x$y[[x$input$dv]], 2L)
        , pch = c(21:25, 1:20)[seq_len(n_colors)]
        , col = "black"
        , bg = bg_colors
        , lwd = par("lwd")
      )
    )
  )
  x
}






#' @export

swarms <- function(x, ...) {
  UseMethod("swarms", x)
}



#' @export

swarms.apa_plot_list <- function(x, ...) {

  if(!requireNamespace("beeswarm", quietly = TRUE)) {
    stop("Please install the 'beeswarm' package to plot bee swarms.", call. = FALSE)
  }

  x$plots <- lapply(X = x$plots, FUN = swarms, ...)
  x
}



#' @export

swarms.apa_plot <- function(x, ...) {

  tmp <- modify_or_add(x, "swarms", ...)

  x$visuals[[tmp$idx]] <- list(
    .f = ".swarms"
    , .creator = "swarms"
    , args = tmp$ellipsis
  )
  x
}



#' @keywords internal

.swarms <- function(...) {

  x <- evalq(x, parent.frame(1L))
  args_points <- defaults(
    inherit_args(x, creator = "points")
    , set.if.null = list(
      cex = par("cex")
    )
  )

  ellipsis <- defaults(
    list(...)
    , set.if.null = list(
      cex = args_points$cex * .5
      , col = brighten(args_points$col, factor = .9)
      , bg = brighten(args_points$bg, factor = .9)
      , pch = args_points$pch
      , lwd = args_points$lwd * .5
    )
  )

  f <- cbind(x$y_agg[, x$input$factors[-c(3:4)], drop = FALSE], 1L)

  if(length(x$input$factors) > 0L) {
    nlevels_fct1 <- nlevels(x$y[[ x$input$factors[[1L]] ]])
  } else {
    nlevels_fct1 <- 1L
  }

  split_x <- split(
    x$x_agg
    , f = f
  )
  split_y <- split(
    x$y_agg
    , f = f
  )

  split_swarm <- mapply(
    .x = split_x
    , .y = split_y
    , cex = rep(ellipsis$cex, each = nlevels_fct1)
    , FUN = function(.x, .y, cex, priority, compact) {
      z <- beeswarm::swarmx(
        x = .x
        , y = unlabel(.y[[x$input$dv]])
        , cex = cex
        , priority = priority
        , compact = compact
      )
      rownames(z) <- rownames(.y)
      z
    }
    , MoreArgs = list(
      priority = "density"
      , compact = TRUE
    )
    , SIMPLIFY = FALSE
  )
  # x$y_agg
  swarm_coords <- unsplit(split_swarm, f)

  # fix indices of inherited parameters
  if(length(x$input$factors) > 1L) {
    idx_fct2 <- as.integer(x$y_agg[[x$input$factors[[2L]]]])
  } else {
    idx_fct2 <- rep(1L, nrow(x$y_agg))
  }

  inherited_pars <- c("cex", "pch", "col", "bg")
  ellipsis[inherited_pars] <- lapply(
    X = ellipsis[inherited_pars]
    , FUN = function(x) {
      rep(x, length.out = max(idx_fct2))[idx_fct2]
    }
  )

  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      x = swarm_coords$x
      , y = swarm_coords$y
    )
  )
  do.call("points", ellipsis)
}



#' @export

lines.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(x$plots, lines, ...)
  x
}


#' @export

lines.apa_plot <- function(x, ...) {

  factors <- x$input$factors
  dv      <- x$input$dv


  tmp <- modify_or_add(x, "lines", ...)


  if(length(factors) > 1L) {
    f <- x$y[[factors[[2L]]]]
  } else {
    f <- factor(rep(1L, nrow(x$y)))
  }

  ellipsis <- defaults(
    list(...)
    , set.if.null = list(
      col = "black" # will be recycled by Map()
      , lty = seq_len(nlevels(f))
    )
  )

  ellipsis <- lapply(
    X = ellipsis
    , FUN = rep
    , length.out = nlevels(f)
  )

  ellipsis$x <- split(x$x, f = f)
  ellipsis$y <- split(extract(x$y[[dv]], 2L), f = f)
  ellipsis$f <- lines

  x$visuals[[tmp$idx]] <- list(
    .f = "Map"
    , .creator = "lines"
    , args = ellipsis
  )

  x
}



#' @export
error_bars <- function(x, ...) {
  UseMethod("error_bars", x)
}

#' @export
error_bars.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(x$plots, error_bars, ...)
  x
}

#' @export
error_bars.apa_plot <- function(
  x
  , length = .04
  , angle = 90
  , code = 3
  , ...
) {

  tmp <- modify_or_add(x, "error_bars", ...)

  x$visuals[[tmp$idx]] <- list(
    .f = "arrows"
    , .creator = "error_bars"
    , args = defaults(
      tmp$ellipsis
      , set.if.null = list(
        x0 = x$x
        , y0 = extract(x$y[[x$input$dv]], 1L)
        , y1 = extract(x$y[[x$input$dv]], 3L)
        , length = .04
        , angle = 90
        , code = 3
      )
    )
  )
  x
}


#' @export
x_axis <- function(x, ...) {
  UseMethod("x_axis", x)
}

#' @export
x_axis.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(x$plots, FUN = x_axis, ...)
  x
}


#' @export

x_axis.apa_plot <- function(x, ...) {

  tmp <- modify_or_add(x, "x_axis", ...)

  x$visuals[[tmp$idx]] <- list(
    .f = ".x_axis"
    , .creator = "x_axis"
    , args = tmp$ellipsis
  )
  x
}

.x_axis <- function(...) {
  x <- evalq(x, parent.frame(1L))
  args_bars <- inherit_args(x, creator = "bars")

  factors <- x$input$factors

  if(length(factors) > 0L) {
    at     <- seq_len(nlevels(x$y[[factors[[1L]]]])) - .5
    labels <- levels(x$y[[factors[1L]]])
    tick   <- TRUE
  } else {
    at     <- 1
    labels <- character(1L)
    tick   <- FALSE
  }

  args_axis <- defaults(
    list(...)
    , set.if.null = list(
      side = 1L
      , at = at
      , labels = labels
      , tick = tick
    )
  )

  ylim1 <- x$visuals$plot.window$args$ylim[1]
  if(length(args_bars) && x$input$reference == ylim1) {
    args_axis <- defaults(
      args_axis
      , set = list(
        lwd = 0
      )
      , set.if.null = list(
        lwd.tick = 1
        , pos = ylim1
      )
    )
  }


  do.call("axis", args_axis)

}


#' @export
y_axis <- function(x, ...) {
  UseMethod("y_axis", x)
}

#' @export
y_axis.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(x$plots, y_axis, ...)
  x
}

#' @export

y_axis.apa_plot <- function(x, ...) {

  tmp <- modify_or_add(x, "y_axis", ...)


  x$visuals[[tmp$idx]] <- list(
    .f = "axis"
    , .creator = "y_axis"
    , args = defaults(
      tmp$ellipsis
      , set.if.null = list(
        side = 2L
        , labels = TRUE
      )
    )
  )
  x
}



#' @export
annotation <- function(x, ...) {
  UseMethod("annotation", x)
}

#' @export
annotation.apa_plot_list <- function(x, main = NULL, ...) {

  z <- x$plots[[1]]
  factors <- z$input$factors

  if(length(factors) > 0L) {
    xlab <- variable_label(z$y[[factors[[1L]]]])
  } else {
    xlab <- ""
  }

  if(length(factors) > 2L) {
    nlevels_fct3 <- nlevels(z$y[[ factors[[3L]] ]])
    inbetween_labels <- character(nlevels_fct3 - 1L)
  } else {
    inbetween_labels <- character(0L)
  }

  ellipsis <- defaults(
    list(...)
    , set.if.null = list(
      xlab = xlab
      , ylab =  c(
        variable_label(z$y[[z$input$dv]][[1L]])
        , inbetween_labels
      )
    )
  )

  if(is.null(main) && length(factors) > 2L) {
    #   , main = combine_plotmath(list(tmp_main, variable_label(data[[factors[3]]]), ": ", i, " & ", variable_label(data[[factors[4]]]), ": ", j))

    if(length(factors) == 3L) {
      main <- Map(
        f = list
        , list(variable_label(z$y[[factors[[3L]]]]))
        , ": "
        , levels(z$y[[factors[[3L]]]])
      )
    } else {
      main <- Map(
        f = list
        , list(variable_label(z$y[[ factors[[3L]] ]]))
        , ": "
        , levels(z$y[[ factors[[3L]] ]])
        , " & "
        , list(variable_label(z$y[[ factors[[4L]] ]]))
        , ": "
        , rep(levels(z$y[[ factors[[4L]] ]]), each = nlevels(z$y[[factors[[3L]]]]))
      )
    }
    main <- lapply(main, combine_plotmath)
  }


  # same-length arguments
  ellipsis <- lapply(
    X = ellipsis
    , FUN = rep
    , length.out = length(x$plots)
  )
  ellipsis$x = x$plots
  ellipsis$main <- main

  # length-one argument to Map()
  ellipsis$f = annotation

  x$plots <- do.call(
    what = "Map"
    , args = ellipsis
    , quote = TRUE
  )
  x
}

#' @export
annotation.apa_plot <- function(x, ...) {

  tmp <- modify_or_add(x, "annotation", ...)

  x$visuals[[tmp$idx]] <- list(
    .f = "title"
    , .creator = "annotation"
    , args = tmp$ellipsis
  )
  x
}

#' @export
legends <- function(.x, ...) {
  UseMethod("legends", .x)
}

#' @export
legends.apa_plot_list <- function(.x, plot = NULL, title = NULL, ...) {

  z <- .x$plots[[1L]]
  factors <- z$input$factors

  if(is.null(plot)) {
    if(length(factors) < 2L) {
      plot <- FALSE
    } else if(length(factors) == 2L) {
      plot <- TRUE
    } else if(length(factors) > 2L) {
      plot <- seq_along(.x$plots) == nlevels(z$y[[factors[[3L]]]])
    }
  } else {
    plot <- rep(plot, length.out = length(.x$plots))
  }

  if(identical(title, character(1L))) {
    # suppress legend title
    title <- NULL
  } else if(is.null(title) && length(factors) > 1L) {
    title <- variable_label(z$y[[factors[[2L]]]])
  }

  .x$plots <- mapply(
    .x = .x$plots
    , plot = plot
    , FUN = legends
    , MoreArgs = list(title = title, ...)
    , SIMPLIFY = FALSE
  )
  .x
}

#' @export
legends.apa_plot <- function(.x, ...) {

  tmp <- modify_or_add(.x, "legends", ...)

  .x$visuals[[tmp$idx]] <- list(
    .f = ".legends"
    , .creator = "legends"
    , args = tmp$ellipsis
  )
  .x
}



#' @keywords internal

.legends <- function(.x, ...) {

  x <- evalq(x, parent.frame(1L))

  args_points <- defaults(
    inherit_args(x, creator = "points")
    , set.if.null = list(
      cex = par("cex")
    )
  )
  args_lines <- defaults(
    inherit_args(x, creator = "lines")
  )

  args_bars <- defaults(
    inherit_args(x, creator = "bars")
  )

  if(length(x$input$factors) > 1L) {
    levels_fct2 <- levels(x$y[[x$input$factors[[2L]]]])
    nlevels_fct2 <- length(levels_fct2)
  } else {
    levels_fct2 <- ""
    nlevels_fct2 <- 1
  }

  # 'col' could be inherited from either points or lines
  # line col should have precedence over point colour, because points will most frequently be coloured via 'bg'
  if( !is.null(args_lines$col) ) {
    col <- rep(args_lines$col, length.out = nlevels_fct2)
  } else {
    if( !is.null(args_points$col) ) {
      col <- rep(args_points$col, length.out = nlevels_fct2)
    } else {
      col <- NULL
    }
  }


  args_legend <- defaults(
    list(...)
    , set.if.null = list(
      x = "topright"
      , legend = levels_fct2
      , pch    = if(is.null(args_points$pch)) NULL else rep(args_points$pch, length.out = nlevels_fct2)
      , bty    = "n"
      , pt.bg  = if(is.null(args_points$bg))  NULL else rep(args_points$bg , length.out = nlevels_fct2)
      , pt.cex = if(is.null(args_points$cex)) NULL else rep(args_points$cex, length.out = nlevels_fct2)
      , pt.lwd = if(is.null(args_points$lwd)) NULL else rep(args_points$lwd, length.out = nlevels_fct2)
      , col    = col
      , lty    = if(is.null(args_lines$lty))  NULL else rep(args_lines$lty , length.out = nlevels_fct2)
      , lwd    = if(is.null(args_lines$lwd))  NULL else rep(args_lines$lwd , length.out = nlevels_fct2)
      # Bar plot stuff
      , fill   = if(is.null(args_bars$col)) NULL else rep(args_bars$col, length.out = nlevels_fct2)
      , border = if(is.null(args_bars$border)) "black" else rep(args_bars$border, length.out = nlevels_fct2)
    )
  )
  do.call("legend", args_legend)
}


#' @export

intercept <- function(x, ...) {
  UseMethod("intercept")
}

#' @export

intercept.apa_plot_list <- function(x, y, ...) {
  x$plots <- lapply(X = x$plots, FUN = intercept, y = y, ...)
  x
}

#' @export
intercept.apa_plot <- function(x, y, ...) {
  tmp <- modify_or_add(x, "intercept", ...)

  xlim <- x$visuals$plot.window$args$xlim

  x_coords <- seq(xlim[1L], xlim[2L], length.out = length(y) + 1L)
  x_coords <- Map(
    x_coords[seq_along(y)]
    , x_coords[seq_along(y) + 1L]
    , f = c
  )


  tmp$ellipsis <- defaults(
    tmp$ellipsis
    , set.if.null = list(
      x = x_coords
      , y = y
    )
  )

  x$visuals[[tmp$idx]] <- list(
    .f = ".intercept"
    , .creator = "intercept"
    , args = tmp$ellipsis
  )
  x
}

#' @keywords internal

.intercept <- function(x, y, ...) {
  Map(
    f = lines
    , x = lapply(X = x, FUN = rep, length.out = 2L)
    , y = lapply(X = y, FUN = rep, length.out = 2L)
    , ...
  )
}


#' @export

print.apa_plot <- function(x, ...) {

  # create print_env for delayed processing
  print_env <- new.env()
  print_env$x <- x

  y <- mapply(
    FUN = do.call
    , what = lapply(x$visuals, `[[`, ".f")
    , args = lapply(x$visuals, `[[`, "args")
    , MoreArgs = list(
      envir = print_env
      , quote = TRUE
    )
  )
  invisible(x)
}


#' @export

print.apa_plot_list <- function(x, ...) {

  z <- x$plots[[1L]]

  factors <- z$input$factors

  if(length(factors) > 2L) {
    old_mfrow <- par("mfrow")
    on.exit(par(mfrow = old_mfrow))

    nlevels_fct3 <- nlevels(z$y[[factors[[3L]]]])

    if(!is.null(x$input$mfrow)) {
      new_mfrow <- x$input$mfrow
      print(x$input$mfrow)
    } else {
      if(length(factors) == 3L) {
        new_mfrow <- c(1L, nlevels_fct3)
      } else {
        nlevels_fct4 <- nlevels(z$y[[factors[[4L]]]])
        new_mfrow = c(nlevels_fct4, nlevels_fct3)
      }
    }
    par(mfrow = new_mfrow)
  }
  y <- lapply(X = x$plots, FUN = print, ...)
  invisible(x)
}


