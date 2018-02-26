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

#' Combine to expression
#'
#' We use this internal function to generate expressions that can be used for plotting. Accepts a list of elements that are coerced,
#' currently supperted elements are \code{character}, \code{expression}, and \code{character} that contain \code{latex} elements.
#'
#' @param x A \code{list} that contains all elements that are intended to be coerced into one expression.
#' @return An expression
#' @keywords internal

combine_plotmath <- function(x){

  x <- lapply(X  = x, FUN = tex_conv)
  y <- as.expression(substitute(paste(a, b), list(a = x[[1]], b = x[[2]])))

  if(length(x)>2){
    for (i in 3:length(x)){
      y <- as.expression(substitute(paste(a, b), list(a = y[[1]], b = x[[i]])))
    }
  }
  return(y)
}


#' @keywords internal

tex_conv <- function(x, latex2exp = package_available("latex2exp")){
  if(!is.null(x)){
    if(!is.expression(x)){
      if(latex2exp){
        latex2exp::TeX(x, output = "expression")[[1]]
      } else {
        as.expression(x)[[1]]
      }
    } else{
      x[[1]]
    }
  } else {
    as.expression("")[[1]]
  }
}
