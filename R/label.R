#' @title Generic functions for 'label'
#' @description
#'  What it does, more specifically...
#' @param object blablabla
#' @param value bla
#' @rdname label
#' @export

setGeneric(
  name = "label"
  , def = function(object){
    standardGeneric("label")
  }
)

#' label dummy methods
#'
#' only a dummy method
#'
#' @keywords internal

setGeneric(
  name = "label<-"
  , def = function(object, value){
    standardGeneric("label<-")
  }
)


#' @rdname label
#' @export

setMethod(
  "label"
  , signature = "annotated_vector"
  , definition = function(object){
    object@label
  }
)

#' Extract label of an atomic (i.e., non-annotated) vector
#'
#' This is only intended as a dummy method. However, it could be used to obtain compatibility with S3-type labels (like in Hmisc package).
#'
#' @keywords internal

setMethod(
  "label"
  , signature = "vector"
  , definition = function(object){
    return(NULL)
  }
)

#' @rdname label
#' @export

setMethod(
  "label"
  , signature = "data.frame"
  , definition = function(object){
    mapply(FUN = label, object, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }
)


#' @rdname label
#' @export

setMethod(
  "label<-"
  , signature = c(object = "annotated_vector", value = "character")
  , definition = function(object, value){
    object@annotation@label <- value
    validObject(object@annotation)
    object@label <- value
    validObject(object)
    object
  }
)

#' @rdname label
#' @export

setMethod(
  "label<-"
  , signature = c(object = "vector", value = "character")
  , definition = function(object, value){

    new(
      paste0("annotated_", class(object))
      , .Data = object
      , label = value
      , annotation = new("vector_annotation", label = value)
    )
  }
)

#' @rdname label
#' @export

setMethod(
  "label<-"
  , signature = c(object = "factor", value = "character")
  , definition = function(object, value){

    new(
      paste0("annotated_", class(object))
      , .Data = object
      , label = value
      , annotation = new("vector_annotation", label = value)
      , levels = levels(object)
      , .S3Class = "factor"
    )
  }
)

#' @rdname label
#' @export

setMethod(
  "label<-"
  , signature = c("data.frame", value = "character")
  , definition = function(object, value){

    if(is.null(names(value))){
      stop("Parameter 'value' is required to be a named vector or list.")
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

# setMethod(
#   "assign_annotation"
#   , signature = c(object = "vector", value = "character", name = "character")
#   , definition = function(object, value, name){
#     object <- new(paste0())
#     slot(object@annotation, name = name) <- value
#     object@label <- object@annotation@label
#     object
#   }
# )


setGeneric(
  "default_label"
  , def = function(object){
    standardGeneric("default_label")
  }
)

#' @title Set default variable labels from column names
#' @description We use this function internally to provide default variable for all columns in a data.frame from column names.
#' @param x A \code{data.frame}
#' @return Returns a \code{data.frame} with labelled columns. Labels are preserved (if already specified), otherwise generated from column names.
#' @rdname default_label
#' @keywords internal

setMethod(
  "default_label"
  , signature = "data.frame"
  , definition = function(object){
    columns <- sapply(X = label(object), FUN = is.null, simplify = TRUE)

    if(any(columns)){
      value <- colnames(object)[columns]
      names(value) <- value
      label(object[, columns]) <- value
    }
    object
  })


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

