#' @include annotated_data.R
NULL



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
#' @keywords internal

setGeneric(
  name = "variable_label"
  , def = function(object){
    standardGeneric("variable_label")
  }
)



#' @rdname variable_label
#' @keywords internal

setGeneric(
  name = "variable_label<-"
  , def = function(object, value){
    standardGeneric("variable_label<-")
  }
)



#' @rdname variable_label
#' @export

setMethod(
  "variable_label"
  , signature = "annotated_vector"
  , definition = function(object){
    object@annotation@label
  }
)



#' Extract label of an Atomic (i.e., non-annotated) vector
#'
#' This is only intended as a dummy method. However, it could be used to obtain compatibility with S3-type labels (like in Hmisc package).
#'
#' @keywords internal

setMethod(
  "variable_label"
  , signature = "vector"
  , definition = function(object){
    NULL
  }
)



setOldClass("labelled")

#' Extract label from a 'labelled' vector
#'
#' This method is for minimum compatibility with the \pkg{Hmisc} package. Allows to extract a variable label that was set by \code{Hmisc::label}.
#' @param object A vector of (S3-) class \code{labelled} (e.g., from \pkg{Hmisc} package) and attribute \code{label} set.
#'
#' @export

setMethod(
  "variable_label"
  , signature = "labelled"
  , definition = function(object){
    return(attr(object, "label"))
  }
)



#' @rdname variable_label
#' @export

setMethod(
  "variable_label"
  , signature = "data.frame"
  , definition = function(object){
    mapply(FUN = variable_label, object, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }
)



#' @rdname variable_label
#' @export

setMethod(
  "variable_label<-"
  , signature = c(object = "annotated_vector", value = "character")
  , definition = function(object, value){
    object@annotation@label <- value
    validObject(object@annotation)
    object@label <- value
    validObject(object)
    object
  }
)



#' @rdname variable_label
#' @export

setMethod(
  "variable_label<-"
  , signature = c(object = "vector", value = "character")
  , definition = function(object, value){

    new(
      paste0("annotated_", class(object))
      , .Data = object
      , annotation = new("vector_annotation", label = value)
    )
  }
)



#' @rdname variable_label
#' @export

setMethod(
  "variable_label<-"
  , signature = c(object = "factor", value = "character")
  , definition = function(object, value){

    new(
      "annotated_factor"
      , .Data = object
      , label = value
      , annotation = new("vector_annotation", label = value)
      , levels = levels(object)
      , .S3Class = "factor"
    )
  }
)



#' @rdname variable_label
#' @export

setMethod(
  "variable_label<-"
  , signature = c(object = "annotated_factor", value = "character")
  , definition = function(object, value){

    new(
      "annotated_factor"
      , .Data = object@.Data
      , label = value
      , annotation = new(
        "vector_annotation"
        , label = value
        , unit = object@annotation@unit
      )
      , levels = object@levels
      , .S3Class = "factor"
    )
  }
)



#' @rdname variable_label
#' @export

setMethod(
  "variable_label<-"
  , signature = c("data.frame", value = "character")
  , definition = function(object, value){

    if(is.null(names(value))){
      stop("Assigned label is required to be a named character vector.")
    }

    if(!all(names(value) %in% colnames(object))){
      stop("Some requested columns could not be found in data.frame.")
    }

    col_order <- match(names(value), colnames(object))
    modified_object <- as.data.frame(object[, col_order], stringsAsFactors = FALSE)
    modified_object <- annotate(modified_object)
    d <- mapply(FUN = assign_annotation, modified_object, name = rep("label", length(col_order)), value = value, USE.NAMES = TRUE, SIMPLIFY = FALSE)
    d <- as.data.frame(d, col.names = names(modified_object), stringsAsFactors = FALSE)
    object[, col_order] <- d

    object
  }
)



#' @keywords internal

setGeneric(
  name = "assign_annotation"
  , def = function(object, value, name){
    standardGeneric("assign_annotation")
  }
)



#' @keywords internal

setMethod(
  "assign_annotation"
  , signature = c(object = "annotated_vector", value = "character", name = "character")
  , definition = function(object, value, name){
    slot(object@annotation, name = name) <- value
    object@label <- object@annotation@label
    object
  }
)

#' #' @keywords internal
#'
#' setMethod(
#'   "assign_annotation"
#'   , signature = c(object = "vector", value = "character", name = "character")
#'   , definition = function(object, value, name){
#'     object <- new(
#'       paste0("annotated_", class(object))
#'       , annotation <- new("vector_annotation")
#'     )
#'     object@label <- object@annotation@label
#'     object# return
#'   }
#' )



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
    columns <- sapply(X = variable_label(object), FUN = is.null, simplify = TRUE)

    if(any(columns)){
      value <- colnames(object)[columns]
      names(value) <- value
      variable_label(object[, columns]) <- value
    }
    object
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



#' @importFrom latex2exp TeX
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

