#' Assign or Extract Variable Labels
#'
#' Assign or extract variable labels of a \code{vector} \emph{or}
#' the columns (i.e., vectors) of a \code{data.frame}.
#'
#' @param x Either a vector or a \code{data.frame}.
#' @param value Character. The variable label(s) to be assigned. If \code{variable_label} is applied to a single vector,
#' this should be a length-one argument. If applied to a \code{data.frame}, \code{value} is required to be a \emph{named} vector.
#' Check the examples for details.
#' @param ... Further arguments that can be passes to methods.
#' @rdname variable_label
#' @export

"variable_label" <- function(x, ...){
  UseMethod("variable_label")
}

#' @rdname variable_label
#' @export

variable_label.default <- function(x, ...){
  attr(x, "label")
}

#' @rdname variable_label
#' @export

variable_label.data.frame <- function(x, ...){
  mapply(FUN = variable_label, x, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}



# ------------------------------------------------------------------------------
# Replacement methods

#' @rdname variable_label
#' @export

`variable_label<-` <- function(x, value){
  UseMethod("variable_label<-")
}

#' @rdname variable_label
#' @export

`variable_label<-.default` <- function(x, value){
  assign_label.default(x, value)
}



#' @rdname variable_label
#' @export

`variable_label<-.data.frame` <- function(x, value){
  assign_label.data.frame(x, value)
}


# ------------------------------------------------------------------------------
# Workhorse functions

#' @keywords internal

assign_label <- function(x, value, ...){
  UseMethod("assign_label")
}


#' @keywords internal

assign_label.default <- function(x, value){
  structure(
    x
    , label = value
    , class = c("papaja_labelled", setdiff(class(x), "papaja_labelled"))
  )
}



#' @keywords internal

assign_label.data.frame <- function(x, value, ...){
  # R allows data frames to have duplicate column names.
  # The following code is optimized to work even in this horrible case.
  # This is especially important for default_label and apa_table (e.g., in
  # a frequency table, you frequently have repeating column names):
  columns_to_annotate <- colnames(x) %in% names(value)
  if(is.null(names(value))){
    stop("Assigned label is required to be a named character vector.")
  }

  if(!all(names(value) %in% colnames(x))){
    stop("Some requested columns could not be found in data.frame.")
  }
  # do not coerce to vector if only one column is changed:
  modified_object <- x[, columns_to_annotate, drop = FALSE]
  colnames(modified_object) <- colnames(x)[columns_to_annotate]
  ordered_value <- value[colnames(modified_object)]

  d <- mapply(
    FUN = assign_label.default
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
  x[, columns_to_annotate] <- d

  x
}


# ------------------------------------------------------------------------------
# alias generics

#' @rdname variable_label
#' @export

"variable_labels" <- variable_label

#' @rdname variable_label
#' @export

`variable_labels<-` <- `variable_label<-`




#' @title Set default variable labels from column names
#' @description We use this function internally to provide default variable for all columns in a data.frame from column names.
#' @param x A \code{data.frame}
#' @return Returns a \code{data.frame} with labelled columns. Labels are preserved (if already specified), otherwise generated from column names.
#' @rdname default_label
#' @keywords internal

default_label <- function(x, ...) {
  UseMethod("default_label", x)
}

default_label.default <- function(x, ...) no_method(x)

# setGeneric(
#   "default_label"
#   , def = function(object){
#     standardGeneric("default_label")
#   }
# )


#' @rdname default_label
#' @keywords internal

default_label.data.frame <- function(x) {

  x_out <- as.data.frame.list(
    x = mapply(
      FUN = function(y, value) {
        if(is.null(variable_label(y))) {
          variable_label(y) <- value
        }
        y
      }
      , y = x
      , value = colnames(x)
      , USE.NAMES = TRUE
      , SIMPLIFY = FALSE
    )
    , check.names = FALSE
    , stringsAsFactors = FALSE
    , col.names = attr(x, "names")
    , row.names = attr(x, "row.names") # `rownames(x)` would coerce to character
  )
  attributes(x_out) <- attributes(x)
  x_out
}

# setMethod(
#   "default_label"
#   , signature = "data.frame"
#   , definition = function(object){
#
#     as.data.frame.list(
#       x = mapply(
#         FUN = function(x, value){
#           if(is.null(variable_label(x))){
#             variable_label(x) <- value
#           }
#           x
#         }
#         , x = object
#         , value = colnames(object)
#         , USE.NAMES = TRUE
#         , SIMPLIFY = FALSE
#       )
#       , check.names = FALSE
#       , stringsAsFactors = FALSE
#     )
#   }
# )


# ------------------------------------------------------------------------------
# Some S3 methods for class papaja_labelled, aimed at making variable labels a bit
# more stable

#' @export

`[.papaja_labelled` <- function(x, ..., drop = FALSE) {
  y <- NextMethod("[")
  variable_label(y) <- variable_label(x)
  y
}

#' @export

`[[.papaja_labelled` <- function(x, ..., exact = TRUE) {
  y <- NextMethod("[[")
  variable_label(y) <- variable_label(x)
  y
}


#' @export

print.papaja_labelled <- function(x, ...) {
  unit_defined <- !is.null(attr(x, "unit"))

  cat(
    "Variable label     : ", attr(x, "label")
    , if(unit_defined) {"\nUnit of measurement: "}
    , if(unit_defined) {attr(x, "unit")}
    , "\n"
    , sep = ""
  )
  variable_label(x) <- NULL
  NextMethod("print")
}


#' @export

droplevels.papaja_labelled <- function(x, exclude = if(anyNA(levels(x))) NULL else NA, ...){
  original_label <- variable_label(x)
  x <- NextMethod("droplevels", x, exclude = exclude, ...)
  variable_label(x) <- original_label
  x
}

#' @export

rep.papaja_labelled <- function(x, ...){
  y <- NextMethod()
  variable_label(y) <- variable_label(x)
  y
}


#' Reorder Levels of Labelled Factor
#'
#' The levels of a factor are re-ordered so that the level specified by ref is
#' first and the others are moved down. This is a copy from \code{\link[stats]{relevel}}
#' in the \pkg{stats} package, but preserves the \code{label} attribute and class \code{papaja_labelled}.
#' @importFrom stats relevel
#' @inheritParams stats::relevel
#' @export

relevel.papaja_labelled <- function(x, ref, ...){
  y <- NextMethod()
  variable_label(y) <- variable_label(x)
  y
}

