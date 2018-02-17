#' Assign or Extract Variable Labels
#'
#' Assign or extract variable labels of a \code{vector} \emph{or}
#' the columns (i.e., vectors) of a \code{data.frame}.
#'
#' @param object Either a vector or a \code{data.frame}.
#' @param value Character. The variable label(s) to be assigned. If \code{variable_label} is applied to a single vector,
#' this should be a length-one argument. If applied to a \code{data.frame}, \code{value} is required to be a \emph{named} vector.
#' Check the examples for details.
#' @rdname variable_label
#' @export

"variable_label" <- function(object, ...){
  UseMethod("variable_label")
}



#' @rdname variable_label
#' @export



variable_label.default <- function(object, ...){
  attr(object, "label")
}

#' @rdname variable_label
#' @export

variable_label.data.frame <- function(object, ...){
  mapply(FUN = variable_label, object, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}



# ------------------------------------------------------------------------------
# Assignment methods

#' @rdname variable_label
#' @export

"variable_label<-" <- function(object, value, ...){
  UseMethod("variable_label<-")
}

#' @rdname variable_label
#' @export

"variable_label<-.default" <- function(object, value, ...){
  assign_label.default(object, value)
}



#' @rdname variable_label
#' @export

"variable_label<-.data.frame" <- function(object, value, ...){
  assign_label.data.frame(object, value)
}


# ------------------------------------------------------------------------------
# Workhorse functions

#' @keywords internal

assign_label <- function(object, value, ...){
  UseMethod("assign_label")
}


#' @keywords internal

assign_label.default <- function(object, value){
  structure(
    object
    , label = value
    , class = c("labelled", setdiff(class(object), "labelled"))
  )
}



#' @keywords internal

assign_label.data.frame <- function(object, value, ...){
  # R allows data frames to have duplicate column names.
  # The following code is optimized to work even in this horrible case.
  # This is especially important for default_label and apa_table (e.g., in
  # a frequency table, you frequently have repeating column names):
  columns_to_annotate <- colnames(object) %in% names(value)
  if(is.null(names(value))){
    stop("Assigned label is required to be a named character vector.")
  }

  if(!all(names(value) %in% colnames(object))){
    stop("Some requested columns could not be found in data.frame.")
  }
  # do not coerce to vector if only one column is changed:
  modified_object <- object[, columns_to_annotate, drop = FALSE]
  colnames(modified_object) <- colnames(object)[columns_to_annotate]
  ordered_value <- value[colnames(modified_object)]

  d <- mapply(
    FUN = papaja:::assign_label.default
    , modified_object
    , value = ordered_value
    , USE.NAMES = TRUE
    , SIMPLIFY = FALSE
  )
  d <- as.data.frame(
    d
    , col.names = names(modified_object)
    , stringsAsFactors = FALSE
    , check.names = FALSE
  )
  object[, columns_to_annotate] <- d

  object
}


#' @title Set default variable labels from column names
#' @description We use this function internally to provide default variable for all columns in a data.frame from column names.
#' @param x A \code{data.frame}
#' @return Returns a \code{data.frame} with labelled columns. Labels are preserved (if already specified), otherwise generated from column names.
#' @rdname default_label
#' @keywords internal

setGeneric(
  "default_label"
  , def = function(object){
    standardGeneric("default_label")
  }
)



#' @rdname default_label
#' @keywords internal

setMethod(
  "default_label"
  , signature = "data.frame"
  , definition = function(object){

    as.data.frame.list(
      x = mapply(
        FUN = function(x, value){
          if(is.null(variable_label(x))){
            variable_label(x) <- value
          }
          x
        }
        , x = object
        , value = colnames(object)
        , USE.NAMES = TRUE
        , SIMPLIFY = FALSE
      )
      , check.names = FALSE
      , stringsAsFactors = FALSE
    )
  }
)



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

