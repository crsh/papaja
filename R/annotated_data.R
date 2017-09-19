#' @import methods
NULL # to import the complete `methods` package, document NULL.

#' Annotations for Vectors
#'
#' This S4 class is used to represent annotations (i.e., meta-data) of a vector;
#' it is used as the meta-data container of the
#' \code{\link{annotated_vector-class}}.
#' It currently contains slots for a variable label and the unit of measurement,
#' but is intended to be incrementally extended.
#'
#' @slot label Length-one character. A label for the vector.
#' @slot unit  Length-one character. The unit of measurement.
#'
#' @keywords internal

setClass(
  "vector_annotation"
  , slots = c(
    label = "character"
    , unit = "character"
  )
)


# ------------------------------------------------------------------------------
# Validity method for `vector_annotation`-class.
#
# Hadley did not anticipate someone wanting to document a validity method.

setValidity(
  Class = "vector_annotation"
  , method = function(object){

    # `return_value` is intended to keep the results of all checks:
    return_value <- rep(NA, length(slotNames(object)))
    names(return_value) <- slotNames(object)

    # For each slot, check if `value` has length <=1
    for (i in slotNames(object)){
      length_slot <- length(slot(object, i))
      return_value[i] <- if(length_slot < 2){
        NA
      } else {
        paste0(
          "Slot \""
          , i
          , "\" has length "
          , length_slot
          , ", but needs to be of length 0 or 1."
        )
      }
    }
    # Return problems:
    return_value[!is.na(return_value)]
  }
)

# ------------------------------------------------------------------------------
# Classes for unnamed vectors.
#
# It would be great to have a graphic representation of the class structure.
# However, an R package implementing this possibility seems to be broken,
# currently.


#' S4 Class Union for Annotated Vectors
#'
#' This is a collection of classes that extend basic data types (currently
#' numeric, integer, character, logical vectors, and factors) with an additional
#' vector_annotation-object.
#'
#' @slot .Data  Depending on the data type that the class extends, the original
#' values.
#' @slot names  A character vector of value names, if supplied.
#' @slot label  Length-one character. This one must always be a copy of slot
#' "label" in slot "annotation".
#' \emph{Don't you ever manipulate this one by hand.}
#' It is only included for nice printing in RStudio.
#' @slot annotation A \code{\link{vector_annotation-class}} object,
#' containing all meta-data of the vector.
#' @details
#'   We currently support six different classes of annotated vectors:
#'   \code{annotated_numeric}, \code{annotated_integer},
#'   \code{annotated_character}, \code{annotated_logical},
#'   \code{annotated_factor}.
#'
#'   These can be extended to named vectors (via \code{\link{names}(x)}).
#'   The corresponding classes are
#'   \code{annotated_named_numeric}, \code{annotated_named_integer},
#'   \code{annotated_named_character}, \code{annotated_named_logical},
#'   \code{annotated_named_factor}.
#'
#'   \emph{Ordered factors, raw and complex vectors are not supported, yet.}
#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_numeric"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "numeric"
)

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_integer"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "integer"
)

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_character"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "character"
)

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_logical"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "logical"
)

# ------------------------------------------------------------------------------
# Factor class needs some more brain:
setOldClass("factor")

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_factor"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "factor"
)

# #' @rdname annotated_vector-class
# #' @keywords internal

# setClass(
#   "annotated_ordered_factor"
#   , contains = c("annotated_factor", "ordered")
# )

# ------------------------------------------------------------------------------
# Classes for named vectors.
#
# Each one extends its corresponding unnamed class by slot `names`.


#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_named_numeric"
  , slots = c(
    names = "character"
  )
  , contains = "annotated_numeric"
)

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_named_integer"
  , slots = c(
    names = "character"
  )
  , contains = "annotated_numeric"
)

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_named_character"
  , slots = c(
    names = "character"
  )
  , contains = "annotated_character"
)

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_named_logical"
  , slots = c(
    names = "character"
  )
  , contains = "annotated_logical"
)

#' @rdname annotated_vector-class
#' @keywords internal

setClass(
  "annotated_named_factor"
  , slots = c(
    names = "character"
  )
  , contains = "annotated_factor"
)


# Ordered factors are not supported, yet.

# setClass(
#   "annotated_named_ordered_factor"
#   , slots = c(
#     names = "character"
#   )
#   , contains = "annotated_ordered_factor"
# )

# ------------------------------------------------------------------------------
# Class Unions (superclasses) for named and unnamed vectors.
# These are hierarchically ordered, i.e., superclass annotated_named_vector
# is itself a member of superclass annotated_vector.

#' @rdname annotated_vector-class
#' @export

setClassUnion(
  name = "annotated_named_vector"
  , members = c(
    "annotated_named_numeric"
    , "annotated_named_integer"
    , "annotated_named_character"
    , "annotated_named_logical"
    , "annotated_named_factor"
    # , "annotated_named_ordered_factor"
  )
)


#' @rdname annotated_vector-class
#' @export

setClassUnion(
  name = "annotated_vector"
  , members = c(
    "annotated_numeric"
    , "annotated_integer"
    , "annotated_character"
    , "annotated_logical"
    , "annotated_factor"
    # , "annotated_ordered_factor"
    # Make annotated_vector a superclass of annotated_named_vector:
    , "annotated_named_vector"
  )
)


#' Generate an Annotated Vector
#'
#' \code{\link{initialize}}-method for class union \code{annotated_vector}.
#' It takes care that \code{.Object@annotation@label} is copied to
#' \code{.Object@label}.
#'
#' @keywords internal

setMethod(
  "initialize"
  , "annotated_vector"
  , function(.Object, ...){
    # Copy `@annotation@label` to `@label`:
    args <- list(...)
    args$.Object <- .Object
    if(is.null(args$annotation)){
      args$label <- character(0)
    } else {
      args$label <- args$annotation@label
    }
    # When calling the standard initialize-method, label and annotation@label
    # already need to be identical, because at the end of callNextMethod,
    # the validity method (see below) is already called for the first time.
    .Object <- do.call("callNextMethod", args)
    .Object
  }
)

# ------------------------------------------------------------------------------
# The corresponding validity method.
#
# It is checked whether slot label is identical to the label slot in the
# annotation object in slot annotation. It is possibly a good idea to hide
# the label slot from the user, as I only provide it for compatibility with
# RStudio (that reads from the S3-ish label-attribute).
#
#
# The error message about names is a safeguard against gross misuse. However,
# if someone does this:
#
# names(object@.Data) <- "The worst way to assign names."
#
# the to-be assigned names are not stored in the object -- a behavior I like.
#
# Instead, this is the desired way to assign names:
#
# names(object) <- c("some", "names", "for", "your", "entries")

setValidity(
  "annotated_vector"
  , method = function(object){
    value <- list()

    if(!identical(object@label, object@annotation@label)){
       value$label <- 'Slot "label" is not identical to slot "annotation@label"'
    }

    if(!is.null(names(object@.Data))){
      value$names <- "There are hidden names in slot \".Data\".
      Please report this problem to www.github.com/crsh/papaja."
    }
    unlist(value)
  }
)


# ------------------------------------------------------------------------------
# Show methods for annotated_vector-superclass
#
# All methods print meta-data to the first two lines, what follows mimicks
# the console output for standard vector types.
# Special care is necessary for named vectors and factors.
#
# Showing a vector_annotation object (the meta-data container) has its own
# method, making it easy to standardize outputs.



#' Show a vector annotation
#'
#' Display a vector annotation by printing to the console.
#'
#' @keywords internal

setMethod(
  f = "show"
  , signature = "vector_annotation"
  , def = function(object){
    cat(
          "Variable label     : ", object@label
      , "\nUnit of measurement: ", object@unit
      , "\n"
      , sep = ""
    )
  }
)


# This might be an alternative solution, printing only used slots ----
# setMethod(
#   f = "show"
#   , signature = "vector_annotation"
#   , def = function(object){
#     cat(
#       if(length(object@label)>0){   "Variable label     : "}else{NULL}
#       , object@label
#       , if(length(object@unit)>0){"\nUnit of measurement: "}else{NULL}
#       , object@unit
#       , "\n"
#       , sep = ""
#     )
#   }
# )
# ----


# This might be an alternative solution that only needs one line ----
#     cat(
#       format(
#         paste0(
#           "Variable label: "
#           , object@annotation@label
#           , "   " # some minimum whitespace separating label and unit
#         )
#         , width = 40
#       )
#       , "Unit of measurement: "
#       , object@annotation@unit
#       , "\n"
#       , sep = ""
#     )
# ----



#' Show an Annotated Vector
#'
#' Display an annotated vector by printing to the console.
#'
#' @rdname show-annotated_vector-method
#' @keywords internal

setMethod(
  "show"
  , "annotated_vector"
  , function(object){
    show(object@annotation)
    print(object@.Data)
  }
)

#' @rdname show-annotated_vector-method
#' @keywords internal

setMethod(
  "show"
  , "annotated_named_vector"
  , function(object){
    show(object@annotation)
    tmp <- object@.Data
    names(tmp) <- object@names
    print(tmp)
  }
)

# Factors needs some special treatment, as .Data contains integer values ----

#' @rdname show-annotated_vector-method
#' @keywords internal

setMethod(
  f = "show"
  , signature = "annotated_factor"
  , definition = function(object){
    show(object@annotation)
    print(object@levels[object@.Data], quote = FALSE)
    cat("Levels: ", object@levels)
  }
)

# Named factors are even a bit more tricky ----

#' @rdname show-annotated_vector-method
#' @keywords internal

setMethod(
  f = "show"
  , signature = "annotated_named_factor"
  , definition = function(object){
    show(object@annotation)
    tmp <- object@levels[object@.Data]
    names(tmp) <- object@names
    print(tmp, quote = FALSE)
    cat("Levels: ", object@levels)
  }
)

# A code-bit for implementing ordered factors
#
# #' @rdname show-annotated_vector-method
# #' @keywords internal
#
# setMethod("show", "annotated_ordered_factor", function(object){
#   show(object@annotation)
#   print(object@levels[object@.Data], quote = FALSE)
#   cat("Levels: ", paste(object@levels, collapse = " < "))
# })



#' Subsetting of Annotated Vectors
#'
#' When subsetting a vector by "[", all annotations (e.g., label and unit of
#' measurement) are preserved.
#' @param x The annotated_vector object to be subsetted.
#' @param i An index specifying elements to extract or replace.
#'
#' @examples
#' a <- new("annotated_numeric", .Data = 1:4)
#' test <- a[1:2]
#' @rdname sub_annotated
#' @keywords internal

setMethod(
  f = "["
  , signature = "annotated_vector"
  , definition = function(x, i){
  new(
    class(x)
    , .Data = x@.Data[i]
    , label = x@label
    , annotation = x@annotation
    )
  }
)

#' @rdname sub_annotated
#' @keywords internal

setMethod(
  f = "["
  , signature = "annotated_named_vector"
  , definition = function(x, i){
    new(
      class(x)
      , .Data = x@.Data[i]
      , names = x@names[i]
      , label = x@label
      , annotation = x@annotation
    )
  }
)

#' @rdname sub_annotated
#' @keywords internal

setMethod(
  f = "["
  , signature = "annotated_factor"
  , definition = function(x, i){
    new(
      "annotated_factor"
      , .Data = x@.Data[i]
      , label = x@label
      , annotation = x@annotation
      , levels = levels(x)
    )
  }
)

#' @rdname sub_annotated
#' @keywords internal

setMethod(
  f = "["
  , signature = "annotated_named_factor"
  , definition = function(x, i){
    new(
      "annotated_named_factor"
      , .Data = x@.Data[i]
      , names = x@names[i]
      , label = x@label
      , annotation = x@annotation
      , levels = levels(x)
    )
  }
)

#' Drop Unused Levels from Factors
#'
#' The function \code{droplevels} is used to drop unused levels from a factor
#' or, more commonly, from factors in a \code{data.frame}. This method preserves
#' meta-data (e.g., label and unit of measurement).
#' @param x An annotated factor from which to drop unused levels.
#' @param ... Further arguments that can be passed, see \code{\link{droplevels}}
#' for further options.
#'
#' @seealso \code{\link{droplevels}}
#' @rdname droplevels-annotated_factor-method
#' @export

setMethod(
  f = "droplevels"
  , signature = c(x = "annotated_factor")
  , definition = function(x, ...){

    args <- list(...)
    if("exclude" %in% names(args)){
      exclude <- args$exclude
    } else {
      exclude <- if(anyNA(levels(x))) NULL else NA
    }

    fac <- as(factor(x, exclude = exclude), "annotated_factor")

    new("annotated_factor"
      , .Data = fac@.Data
      , label = x@label
      , annotation = x@annotation
      , levels = fac@levels
    )
  }
)

#' @rdname droplevels-annotated_factor-method
#' @export

setMethod(
  f = "droplevels"
  , signature = c(x = "annotated_named_factor")
  , definition = function(x, ...){

    args <- list(...)
    if("exclude" %in% names(args)){
      exclude <- args$exclude
    } else {
      exclude <- if(anyNA(levels(x))) NULL else NA
    }

    fac <- as(factor(x, exclude = exclude), "annotated_factor")

    new("annotated_named_factor"
        , .Data = fac@.Data
        , names = x@names
        , label = x@label
        , annotation = x@annotation
        , levels = fac@levels
    )
  }
)

#' Drop Unused Levels from Vectors
#'
#' This is an additional method that allows for more convenient programming:
#' If a vector (annotated or not) is not a factor, do not change anything.
#' @param x An atomic vector or annotated vector.
#' @return x, unchanged.
#' @keywords internal

setMethod(
  f = "droplevels"
  , signature = "vector"
  , definition = function(x) {
    x
  }
)

#' Reorder Levels of Annotated Factor
#'
#' The levels of an annotated factor are re-ordered so that the level specified
#' by \code{ref} is first and the others are moved down. This is useful
#' for \code{contr.treatment} contrasts which take the first level as the
#' reference.
#'
#' @param x An object of class \code{annotated_factor}.
#' @param ref The reference level, typically a character. Contrary to the
#' original behavior, you can also specify a character vector of any length:
#' As many levels as you specified will be re-ordered (standard behavior issued
#' an error).
#' @return An annotated factor of the same length as x.
#' @seealso \code{\link{relevel}}
#' @details Currently, two methods for reordering the levels of an \code{annotated_factor} are implemented:
#' The \code{annotated_factor,character}-method reorders the levels according to the ordering in a character vector \code{ref}.
#' The \code{annotated_factor,integer}-method reorders the levels according to their position within the vector of levels
#' (as would be returned by \code{levels}).
#'
#' @rdname relevel_annotated
#' @export

setMethod(
  f = "relevel"
  , signature = c(x = "annotated_factor", ref = "character")
  , definition = function(x, ref){
    # reorder integer vector:
    index <- 1:length(x@levels)
    new_levels <- c(ref, levels(x)[!(levels(x)%in%ref)])
    names(index) <- new_levels
    .Data <- index[x@levels[x@.Data]]
    names(.Data) <- NULL

    new(
      "annotated_factor"
      , .Data = .Data
      , label = x@label
      , annotation = x@annotation
      , levels = new_levels
    )
  }
)

#' @rdname relevel_annotated
#' @export

setMethod(
  f = "relevel"
  , signature = c(x = "annotated_factor", ref = "integer")
  , definition = function(x, ref){
    # reorder integer vector:
    index <- 1:length(x@levels)
    new_levels <- x@levels[c(ref, (1:length(x@levels))[-ref])]
    names(index) <- new_levels
    new_.Data <- index[x@levels[x@.Data]]
    names(new_.Data) <- NULL

    new(
      "annotated_factor"
      , .Data = new_.Data
      , label = x@label
      , annotation = x@annotation
      , levels = new_levels
    )
  }
)



#' Coerce annotated factors to character vectors
#'
#' Methods to coerce an annotated factor to an atomic character vector.
#' Necessary for compatibility reasons (View() in RStudio).
#' @param x An object of class annotated_factor.
#'
#' @rdname as_character
#' @keywords internal

setMethod(
  f = "as.character"
  , signature = "annotated_factor"
  , definition = function(x){
    as(x, "character")
  }
)

setAs(
  from = "annotated_factor"
  , to = "character"
  , def = function(from){
    from@levels[from@.Data]
  }
)

# setAs(
#   from = "annotated_factor"
#   , to = "factor"
#   , def = function(from){
#     factor(x = from@levels[from@.Data], levels = from@levels)
#   }
# )

setGeneric(
  "as.factor"
)




#' Coerce Annotated Vectors to Factors
#'
#' Methods to coerce an annotated vector to an (atomic) factor.
#' Necessary for compatibility reasons.
#' @param x An object of virtual class annotated_vector.
#'
#' @rdname as_factor
#' @keywords internal
#' @export

setMethod(
  f = "as.factor"
  , signature = "annotated_vector"
  , definition = function(x){
    as(x, "factor")
  }
)

#' Coerce vectors to annotated vectors
#'
#' Internal workhorse function. \emph{This function is not exported.}
#' @param An annotated vector or a data.frame.
#'
#' @rdname annotate
#' @keywords internal

setGeneric(
  name = "annotate"
  , def = function(object){
    standardGeneric("annotate")
  }
)

#' @rdname annotate
#' @keywords internal

setMethod(
  f = "annotate"
  , signature = "data.frame"
  , definition = function(object){
    # ----
    # optional=TRUE suppresses changing colnames
    as.data.frame(lapply(X = object, FUN = annotate), stringsAsFactors = FALSE, optional = TRUE)
  }
)

#' @rdname annotate
#' @keywords internal

setMethod(
  f = "annotate"
  , signature = "vector"
  , definition = function(object){
    if(is.null(names(object))){
      as(object, paste0("annotated_", class(object)))
    } else {
      as(object, paste0("annotated_named_", class(object)))
    }
  }
)

#' @rdname annotate
#' @keywords internal

setMethod(
  f = "annotate"
  , signature = "annotated_vector"
  , definition = function(object){
    object
  }
)






#' Set Levels of an Annotated Factor
#'
#' This method is as wrapper around \code{\link{levels}} and should do the same.
#' Annotations are preserved.
#'
#' @param x An object of \code{annotated_factor-class}.
#' @param value Character. The valid value for \code{levels(x)}.
#'
#' @keywords internal

setMethod(
  "levels<-"
  , "annotated_factor"
  , definition = function(x, value){
    fac <- factor(x@levels[x@.Data], levels = x@levels)
    levels(fac) <- value
    new("annotated_factor", fac, levels = levels(fac), annotation = x@annotation, label = x@label)
  }
)



#' Replicate Elements of Annotated Vectors
#'
#' Methods for annotated vectors to replicate values, calls \code{\link{rep}}
#' and preserves annotations.
#'
#' @param x An object of class \code{annotated_vector}.
#' @param ... Further arguments that can be passed to\code{\link{rep}}.
#'
#'
#' @rdname rep
#' @keywords internal

setMethod(
  "rep"
  , signature = "annotated_vector"
  , definition = function(x, ...){
    new(
      class(x)
      , .Data = rep(x@.Data, ...)
      , annotation = x@annotation
      , label = x@label
    )
  }
)

#' @rdname rep
#' @keywords internal

setMethod(
  "rep"
  , signature = "annotated_factor"
  , definition = function(x, ...){
    new(
      "annotated_factor"
      , .Data = rep(x@.Data, ...)
      , annotation = x@annotation
      , levels = x@levels
      , label = x@label
    )
  }
)




#' The Names of an Annotated Vector
#'
#' Methods to set the names of an annotated vector.
#' If names are specified for an unnamed annotated_vector, its class is changed
#' to the corresponding class from annotated_named_vector-superclass.
#' @param x An annotated vector.
#' @param value A character vector of the same length as x, or NULL.
#' @details
#'   See \code{\link{names}} for details. If the names of an object of an
#'   annotated_named_vector are removed by setting \code{names(x) <- NULL}, it
#'   is coerced to the corresponding class from annotated_vector-superclasses
#'   (i.e., an annotated vector without a names slot).
#' @rdname names-set-annotated_vector-ANY-method
#' @keywords internal

setMethod(
  f = "names<-"
  , signature = "annotated_vector"
  , definition = function(x, value){
    value_ <- rep(NA, length.out = length(x@.Data))
    value_[1:length(value)] <- value
    new(
      gsub(class(x), pattern = "annotated", replacement = "annotated_named")
      , .Data = x@.Data
      , names = as.character(value_)
      , label = x@label
      , annotation = x@annotation
    )
  }
)

#' @rdname names-set-annotated_vector-ANY-method
#' @keywords internal

setMethod(
  f = "names<-"
  , signature = "annotated_named_vector"
  , definition = function(x, value){
    value_ <- rep(NA, length.out = length(x@.Data))
    value_[1:length(value)] <- value
    new(
      class(x)
      , .Data = x@.Data
      , names = as.character(value_)
      , label = x@label
      , annotation = x@annotation
    )
  }
)

#' Replace the Names Slot with NULL
#'
#' If \code{value(x) <- NULL} is assigned, an unnamed vector keeps its class,
#' a named vector is coerced to its unnamed complement.
#'
#' @rdname replacement_with_NULL_names
#' @keywords internal

setMethod(
  f = "names<-"
  , signature = c(x = "annotated_vector", value = "NULL")
  , definition = function(x, value){
    x
  }
)

#' @rdname replacement_with_NULL_names
#' @keywords internal

setMethod(
  f = "names<-"
  , signature = c(x = "annotated_named_vector", value = "NULL")
  , definition = function(x, value){
    as(x, gsub(class(x), pattern = "annotated_named", replacement = "annotated"))
  }
)

# ------------------------------------------------------------------------------
# This section defines methods to coerce named vectors to unnamed vectors.
# Method selection uses class inheritance only on the first argument; thus, it
# it is necessary to define a method for each target-class.

setAs(
  from = "annotated_named_vector"
  , to = "annotated_numeric"
  , def = function(from){
    new(
      "annotated_numeric"
      , .Data = as(from@.Data, "numeric")
      , label = from@label
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_named_vector"
  , to = "annotated_integer"
  , def = function(from){
    new(
      "annotated_integer"
      , .Data = as(from@.Data, "integer")
      , label = from@label
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_named_vector"
  , to = "annotated_character"
  , def = function(from){
    new(
      "annotated_character"
      , .Data = as(from@.Data, "character")
      , label = from@label
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_named_vector"
  , to = "annotated_logical"
  , def = function(from){
    new(
      "annotated_character"
      , .Data = as(from@.Data, "logical")
      , label = from@label
      , annotation = from@annotation
    )
  }
)

# ------------------------------------------------------------------------------
# This next section defines methods to coerce unnamed vectors to named vectors.
# Method selection uses class inheritance only on the first argument; thus, it
# it is necessary to define a method for each target-class.

setAs(
  from = "annotated_vector"
  , to = "annotated_named_numeric"
  , def = function(from){
    new(
      "annotated_named_numeric"
      , .Data = as(from@.Data, "numeric")
      # Next line is not necessary, but an explicit description what happens.
      # , names = rep(NA_character_, length(from@.Data))
      , label = from@label
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_vector"
  , to = "annotated_named_integer"
  , def = function(from){
    new(
      "annotated_named_integer"
      , .Data = as(from@.Data, "integer")
      # Next line is not necessary, but an explicit description what happens.
      # , names = rep(NA_character_, length(from@.Data))
      , label = from@label
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_vector"
  , to = "annotated_named_character"
  , def = function(from){
    new(
      "annotated_named_character"
      , .Data = as(from@.Data, "character")
      # Next line is not necessary, but an explicit description what happens.
      # , names = rep(NA_character_, length(from@.Data))
      , label = from@label
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_vector"
  , to = "annotated_named_logical"
  , def = function(from){
    new(
      "annotated_named_logical"
      , .Data = as(from@.Data, "logical")
      # Next line is not necessary, but an explicit description what happens.
      # , names = rep(NA_character_, length(from@.Data))
      , label = from@label
      , annotation = from@annotation
    )
  }
)

# ------------------------------------------------------------------------------
# This next section defines methods to coerce annotated vectors to other types
# of annotated vectors.
# Class Inheritance hopefully works, but should be tested!

setAs(
  from = "annotated_vector"
  , to = "annotated_numeric"
  , def = function(from){
    new(
      "annotated_numeric"
      , .Data = as.numeric(from@.Data)
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_vector"
  , to = "annotated_integer"
  , def = function(from){
    new(
      "annotated_integer"
      , .Data = as.integer(from@.Data)
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_vector"
  , to = "annotated_character"
  , def = function(from){
    new(
      "annotated_character"
      , .Data = as.character(from@.Data)
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_vector"
  , to = "annotated_logical"
  , def = function(from){
    new(
      "annotated_logical"
      , .Data = as.logical(from@.Data)
      , annotation = from@annotation
    )
  }
)


# see below why this is commented out ----
# setAs(
#   from = "annotated_vector"
#   , to = "annotated_factor"
#   , def = function(from){
#
#     tmp <- factor(from@.Data)
#     new(
#       "annotated_factor"
#       , tmp
#       , annotation = from@annotation
#     )
#   }
# )


# ------------------------------------------------------------------------------
# For coercion to annotated_factor, it seems to be necessary to include
# methods, directly -- probably because factor inherits from integer???
setAs(
  from = "annotated_numeric"
  , to = "annotated_factor"
  , def = function(from){

    tmp <- factor(from@.Data)
    new(
      "annotated_factor"
      , tmp
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_integer"
  , to = "annotated_factor"
  , def = function(from){

    tmp <- factor(from@.Data)
    new(
      "annotated_factor"
      , tmp
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_character"
  , to = "annotated_factor"
  , def = function(from){

    tmp <- factor(from@.Data)
    new(
      "annotated_factor"
      , tmp
      , annotation = from@annotation
    )
  }
)

setAs(
  from = "annotated_logical"
  , to = "annotated_factor"
  , def = function(from){

    tmp <- factor(from@.Data)
    new(
      "annotated_factor"
      , tmp
      , annotation = from@annotation
    )
  }
)


# ------------------------------------------------------------------------------
# Coercion from basic data types to their annotated counterparts.

setAs(
  from = "vector"
  , to = "annotated_numeric"
  , def = function(from){
    new("annotated_numeric", .Data = as(from, "numeric"))
  }
)

setAs(
  from = "vector"
  , to = "annotated_integer"
  , def = function(from){
    new("annotated_integer", .Data = as(from, "integer"))
  }
)

setAs(
  from = "vector"
  , to = "annotated_character"
  , def = function(from){
    new("annotated_character", .Data = as(from, "character"))
  }
)

setAs(
  from = "vector"
  , to = "annotated_logical"
  , def = function(from){
    new("annotated_logical", .Data = as(from, "logical"))
  }
)

# ------------------------------------------------------------------------------
# Coercion to factor is horrible, method dispatch does not work if only
# superclass vector is specified

setAs(
  from = "numeric"
  , to = "annotated_factor"
  , def = function(from){
    tmp <- factor(from)
    new("annotated_factor", tmp)
  }
)

setAs(
  from = "integer"
  , to = "annotated_factor"
  , def = function(from){
    tmp <- factor(from)
    new("annotated_factor", tmp)
  }
)

setAs(
  from = "character"
  , to = "annotated_factor"
  , def = function(from){
    tmp <- factor(from)
    new("annotated_factor", tmp)
  }
)

setAs(
  from = "logical"
  , to = "annotated_factor"
  , def = function(from){
    tmp <- factor(from)
    new("annotated_factor", tmp)
  }
)

setAs(
  from = "factor"
  , to = "annotated_factor"
  , def = function(from){
    new("annotated_factor", from)
  }
)

# ------------------------------------------------------------------------------
# Coercion from annotated vectors to basic data types.

setAs(
  from = "annotated_vector"
  , to = "factor"
  , def = function(from){
    factor(from@.Data)
  }
)

setAs(
  from = "annotated_factor"
  , to = "factor"
  , def = function(from){
    factor(
      x = from@levels[from@.Data]
      , levels = from@levels
    )
  }
)
