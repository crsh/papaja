#' Variable labels
#'
#' Functions used to assign and extract variable labels of a \code{vector} \emph{or}
#' the columns (i.e., vectors) of a \code{data.frame}.
#'
#' @param x Either a vector or a \code{data.frame} containing the variables you want to label.
#' @param value A vector with the variable label(s) to be assigned. Can be anything, e.g. character, expression, etc.
#'
#' @return
#'         If applied to a \code{data.frame}, \code{variable_label} returns a named vector containing all variable labels.
#'         If applied to a \code{vector}, a length-one \code{character}.
#'
#' @examples
#' variable_label(npk) <- c("Block number", "Nitrogen", "Phosphate", "Potassium", "Yield")
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
  mapply(x = x, FUN = variable_label, SIMPLIFY = TRUE)
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



#' Assign a variable label
#'
#' These internal functions are used by \code{\link{variable_label}}.
#'
#' @param x The object that is intended to get some variable labels. Typically a \code{data.frame} or a \code{vector}.
#' @param value Character, expression. The label that is intended to be assigned to the variables. If \code{x} is a \code{data.frame},
#'                        the number of labels must match the number of columns.
#' @return
#' A \code{data.frame} or a \code{vector}, now with variable labels. The (S3-)class of this new object is \code{labelled}.
#' @rdname assign_label
#' @keywords internal
#' @export

assign_label <- function(x, value) {
  if(missing(value)) stop("You did not provide a value to assign as a variable label.")
  UseMethod("assign_label", x)
}

#' @rdname assign_label
#' @keywords internal
#' @export

assign_label.default <- function(x, value) {

  # validate(value, check_length = 1)
  attr(x, which = "label") <- value

  if(!("labelled" %in% class(x))) {
    class(x) <- c("labelled", class(x))
  }

  return(x)
}

#' @rdname assign_label
#' @keywords internal
#' @export

assign_label.data.frame <- function(x, value){
  # sanity checks
  if(length(value)!=ncol(x)){
    stop(
      paste0(
        "Your data.frame has "
        , ncol(x)
        , " columns, but you provided "
        , length(value)
        , ifelse(length(value)==1, " variable label.", " variable labels.")
        , "\nThe number of labels needs to equal the number of columns."
      )
    )
  }

  d <- mapply(FUN = assign_label, x = x, value = value, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  as.data.frame(d, col.names = names(x), stringsAsFactors = FALSE)
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
#' @keywords internal

default_label.data.frame <- function(x){
  columns <- sapply(X = variable_label(x), FUN = is.null, simplify = TRUE)
  # print(columns)
  if(any(columns)){
    variable_label(x[, columns]) <- colnames(x)[columns]
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




