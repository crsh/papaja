#' Brighten up a Color
#'
#' Brighten up a specified color (e.g., the swarm color in [apa_beeplot()]).
#' *This function is not exported.*
#'
#' @param col Colour to be brightened up. Can be anything that is digestible by [col2rgb()].
#' @param factor Numeric. The rate with which the color channel should be boosted. Must be in the range between 0 and 1.
#' @return A character vector as returned by [rgb()].
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

#' Matrix Method for lines()
#'
#' Internal function for convenient plotting of multiple lines.
#' *This function is not exported.*
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

#' Matrix Method for points()
#'
#' Internal function for convenient plotting of multiple points.
#' *This function is not exported.*
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



#' Combine to Expression
#'
#' We use this internal function to generate expressions that can be used for plotting. Accepts a list of elements that are coerced,
#' currently supported elements are `character`, `expression`, and `character` that contain \code{latex} elements.
#' *This function is not exported.*
#'
#' @param x A `list` that contains all elements that are intended to be coerced into one expression.
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

  if( length(x) ){
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


#' Formula Interface for Plot Functions
#'
#' This is an internal function that ensures consistent processing of formulae
#' in plotting functions.
#'
#' @param formula A two-sided formula specifying the dependent variable on the left
#'   of a `~` operator, and factors on the right. A vertical bar (`|`) can be used to
#'   specify an additional subject identifier, which must be provided if an additional
#'   aggregation step is necessary. See the examples section for details.
#' @param data A data frame (or list) from which the variables in `formula` should be taken.
#' @keywords internal

formula_processor <- function(formula, data, .fun, ...) {
  if(length(formula) < 3L) stop("Please provide a formula with both left- and right-hand side variables.", call. = FALSE)

  lhs <- all.vars(formula[[2L]])
  rhs <- all.vars(formula[[3L]])

  deparsed_terms <- lapply(formula[[3L]], deparse)
  bar_term <- grep(deparsed_terms, pattern = "|", fixed = TRUE)

  if(length(bar_term) != 1L) stop("Please provide one and only one id variable.", call. = FALSE)
  all_rhs <- all.vars(formula[[3L]])

  split_bar_term <- strsplit(deparsed_terms[[bar_term]], split = "|", fixed = TRUE)[[1L]][[2L]]
  id_var <- gsub(split_bar_term, pattern = ")| ", replacement = "")

  .fun(
    data = data
    , dv = lhs
    , id = id_var
    , factors = setdiff(all_rhs, id_var)
    , ...
  )
}
