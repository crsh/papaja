#' Brighten up a colour
#'
#' We use this internal utility function to brighten up a specified colour (e.g., the swarm colour in \code{\link{apa_beeplot}}).
#'
#' @param col Colour to be brightened up. Can be anything that is digestible by \code{\link{col2rgb}}.
#' @param factor Numeric The rate with which the color channel should be boosted. Must be in the range between 0 and 1.
#' @return A character vector as returned by \code{\link{rgb}}.
#' @keywords internal

brighten <- function(col, factor){
  validate(factor, check_range = c(0, 1))

  old_color <- col2rgb(col, alpha = TRUE)
  new_color <- rgb(
    red = (old_color["red", ]) + (255 - old_color["red", ])^factor
    , green = (old_color["green", ]) + (255 - old_color["green", ])^factor
    , blue = (old_color["blue", ]) + (255 - old_color["blue", ])^factor
    , maxColorValue = 255
  )
  return(new_color)
}

#' Matrix method for lines
#'
#' Internal function for convenient plotting of multiple lines.
#'
#' @keywords internal

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

#' Matrix method for points
#'
#' Internal function for convenient plotting of multiple points.
#'
#' @keywords internal

points.matrix <- function(x, y, type = "p", ...) {

  args <- list(...)
  args$type = type

  for (i in 1:ncol(x)){
    args.i <- lapply(X = args, FUN = sel, i)
    args.i$x <- unlist(x[, i])
    args.i$y <- unlist(y[, i])
    do.call("points", args.i)
  }
}

#  Matrix method for arrows
#
# Internal function for convenient plotting of multiple arrows.
#
# @keywords internal

# arrows.matrix <- function(x0, x1, y0, y1, ...) {
#
#   args <- list(...)
#
#   for (i in 1:ncol(x0)){
#     args.i <- lapply(X = args, FUN = sel, i)
#     args.i$x0 <- x0[, i]
#     args.i$x1 <- x1[, i]
#     args.i$y0 <- y0[, i]
#     args.i$y1 <- y1[, i]
#     do.call("arrows", args.i)
#   }
# }
