#' Assign and extract variable labels
#'
#' Functions used to assign and extract variable labels of a \code{vector} \emph{or}
#' the columns (i.e., vectors) of a \code{data.frame}.
#'
#' @param x Either a vector or a \code{data.frame} containing the variables you want to label.
#' @param value The variable label(s) to be assigned. Can be anything, e.g. character, expression, etc. If applied to a single vector,
#' this should be a length-one argument. If applied to a \code{data.frame}, \code{value} is required to be a \emph{named} vector or
#' a \emph{named} list. Check the examples for details.
#'
#' @return
#'         If applied to a \code{data.frame}, \code{variable_label} returns a named list containing all variable labels.
#'         If applied to a \code{vector}, a length-one \code{character}.
#'
#' @examples
#' variable_label(npk) <- c(N = "Nitrogen", P = "Phosphate", K = "Potassium")
#' variable_label(npk)
#' variable_label(npk) <- NULL
#' variable_label(npk)
#' @rdname variable_label
#' @export

variable_label <- function(x) {
  UseMethod("variable_label", x)
}

#' @rdname variable_label
#' @export

variable_label.default <-function(x) {
  attr(x, "label")
}


#' @rdname variable_label
#' @export

variable_label.data.frame <-function(x) {
  mapply(x = x, FUN = variable_label, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}



#' @rdname variable_label
#' @export

"variable_label<-" <- function(x, value) {
  UseMethod("variable_label<-")
}

#' @rdname variable_label
#' @export

"variable_label<-.default" <- function(x, value) {
  assign_label.default(x, value)
}

#' @rdname variable_label
#' @export
"variable_label<-.data.frame" <- function(x, value) {
  assign_label.data.frame(x, value)
}

#' Assign and extract variable units of measurement
#'
#' Functions used to assign and extract variable units (of measurement) of a \code{vector} \emph{or}
#' the columns (i.e., vectors) of a \code{data.frame}.
#'
#' @param x Either a vector or a \code{data.frame} containing the variables you want to assign a unit of measurement.
#' @param value A vector with the variable unit(s) to be assigned. Can be anything, e.g. character, expression, etc. If applied to a single vector,
#' this should be a length-one argument. If applied to a \code{data.frame}, \code{value} is required to be a \emph{named} vector or
#' a \emph{named} list. Check the examples for details.
#'
#' @return
#'         If applied to a \code{data.frame}, \code{variable_label} returns a named list containing all variable units.
#'         If applied to a \code{vector}, a length-one \code{character}.
#'
#' @examples
#' variable_unit(npk$yield) <- "pounds/plot"
#' variable_unit(npk)
#' variable_unit(npk) <- c(yield = "ppp")
#' variable_unit(npk)
#' variable_unit(npk) <- NULL
#' @rdname variable_unit
#' @export

variable_unit <- function(x) {
  UseMethod("variable_unit", x)
}

#' @rdname variable_unit
#' @export

variable_unit.default <-function(x) {
  attr(x, "unit")
}


#' @rdname variable_unit
#' @export

variable_unit.data.frame <-function(x) {
  mapply(x = x, FUN = variable_unit, SIMPLIFY = FALSE)
}



#' @rdname variable_unit
#' @export

"variable_unit<-" <- function(x, value) {
  UseMethod("variable_unit<-")
}

#' @rdname variable_unit
#' @export

"variable_unit<-.default" <- function(x, value) {
  assign_label.default(x, value, which = "unit")
}

#' @rdname variable_unit
#' @export
"variable_unit<-.data.frame" <- function(x, value) {
  assign_label.data.frame(x, value, which = "unit")
}

#' Assign a variable label
#'
#' These internal functions are used by \code{\link{variable_label}}.
#'
#' @param x The object that is intended to get some variable labels. Typically a \code{data.frame} or a \code{vector}.
#' @param value Character, expression. The label that is intended to be assigned to the variables. If \code{x} is a \code{data.frame},
#'                        the number of labels must match the number of columns.
#' @param which Character. The supposed name of the attribute to be set, defaults to \code{label}.
#' @return
#' A \code{data.frame} or a \code{vector}, now with variable labels. The (S3-)class of this new object is \code{labelled}.
#' @rdname assign_label
#' @keywords internal
#' @export

assign_label <- function(x, value, which) {
  if(missing(value)) stop("You did not provide a value to assign as a variable label.")
  UseMethod("assign_label", x)
}

#' @rdname assign_label
#' @keywords internal
#' @export

assign_label.default <- function(x, value, which = "label") {

  # validate(value, check_length = 1)
  attr(x, which = which) <- value

  if(!("labelled" %in% class(x))) {
    class(x) <- c("labelled", class(x))
  }

  if(is.null(attr(x, which = "label"))&&is.null(attr(x, which = "unit"))&&"labelled" %in% class(x)){
    class(x) <- setdiff(class(x), "labelled")
  }

  return(x)
}

#' @rdname assign_label
#' @keywords internal
#' @export

assign_label.data.frame <- function(x, value, which = "label"){

  if(is.null(value)){
    x <- as.data.frame(
      mapply(
        FUN = assign_label
        , x = x
        , value = vector("list", length = ncol(x))
        , which = which
        , USE.NAMES = TRUE
        , SIMPLIFY = FALSE
      )
      , stringsAsFactors = FALSE
    )
  } else {

    if(is.null(names(value))){
      stop("Parameter 'value' is required to be a named vector or list.")
    }

    if(!all(names(value) %in% colnames(x))){
      stop("Some requested columns could not be found in data.frame.")
    }

    col_order <- match(names(value), colnames(x))
    modified_x <- as.data.frame(x[, col_order])

    d <- mapply(FUN = assign_label, x = modified_x, value = value, which = which, USE.NAMES = TRUE, SIMPLIFY = FALSE)
    d <- as.data.frame(d, col.names = names(modified_x), stringsAsFactors = FALSE)
    x[, col_order] <- d
  }
  x
}





#' @export

"[.labelled"<- function(x, ...) {
  original_attributes <- attributes(x)
  x <- NextMethod("[")
  attributes(x) <- original_attributes
  return(x)
}



#' Provide a method for factor
#'
#' It would be nice to export this as a method
#' This one is necessary for the S3 methods of droplevels, relevel, reorder, as.data.frame
#'
#' @keywords internal


factor.labelled <-function(x, ...){

  original_labels <- variable_label(x)
  x <- factor(x, ...)
  variable_label(x) <- original_labels

  if(!("labelled" %in% class(x))) {
    class(x) <- c("labelled", class(x))
  }

  return(x)
}


#' Drop unused levels of factor
#'
#' This one is intended to become the S3 method for labelled factors. Unfortunately, this only works reliably for R >= 3.4.0.
#' Therefore, we postponed exporting this function.
#'
#' @keywords internal

droplevels.labelled <- function(x, exclude = if (anyNA(levels(x))) NULL else NA, ...){
  original_labels <- variable_label(x)
  y <- factor.labelled(x, exclude = exclude, ...)
  variable_label(y) <- original_labels
  y
}


#' @importFrom stats relevel
#' @keywords internal

relevel.labelled <- function(x, ...){

  original_label <- variable_label(x)
  x <- NextMethod(x, ...)
  variable_label(x) <- original_label
  if(!("labelled" %in% class(x))) {
    class(x) <- c("labelled", class(x))
  }
  return(x)
}


#' @importFrom stats reorder
#' @keywords internal

reorder.labelled <- function(x, ...){
  original_label <- variable_label(x)
  x <- NextMethod(x, ...)
  variable_label(x) <- original_label
  if(!("labelled" %in% class(x))) {
    class(x) <- c("labelled", class(x))
  }
  return(x)
}

#' @noRd
#' @keywords internal
#' @export

as.data.frame.labelled <- as.data.frame.vector


#' Set default variable labels from column names
#'
#' We use this function internally to provide default variable for all columns in a data.frame from column names.
#' @param x A \code{data.frame}
#' @return Returns a \code{data.frame} with labelled columns. Labels are preserved (if already specified), otherwise generated from column names.
#' @keywords internal

default_label <- function(x){
  UseMethod("default_label", x)
}

#' @rdname default_label
#' @method default_label data.frame
#' @keywords internal

default_label.data.frame <- function(x){
  columns <- sapply(X = variable_label(x), FUN = is.null, simplify = TRUE)

  if(any(columns)){
    value <- colnames(x)[columns]
    names(value) <- colnames(x)[columns]
    variable_label(x[, columns]) <- value
  }
  x
}


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


#' @importFrom latex2exp TeX
#' @keywords internal

tex_conv <- function(x){
  if(!is.null(x)){
    if(!is.expression(x)){
      if(package_available("latex2exp")){
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




