#' @import methods
NULL


#' Vector annotations
#'
#' A class intended to keep all meta-data of a vector.
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

setValidity(
  Class = "vector_annotation"
  , method = function(object){

    # build a vector for containing all possible problems
    return_value <- rep(NA, length(slotNames(object)))
    names(return_value) <- slotNames(object)

    # for each slot, check if value has length 1 (or shorter)
    for (i in slotNames(object)){
      length_slot <- length(slot(object, i))
      return_value[i] <- if(length_slot<2){NA} else {paste0("Slot '", i, "' has length ", length_slot, ", needs to be of length 0 or 1.")}
    }

    # return only problems
    return_value[!is.na(return_value)]
  }
)

#' S4 class union 'annotated_vector'
#'
#' These are the details about this class..
#'
#' @slot label  Length-one character. Should be a copy of slot "label" in slot "annotation".
#' This slot is only included for nice printing in RStudio.
#' @slot annotation An object of class \code{\link{vector_annotation-class}}, containing all meta-data of the vector.
#' @keywords internal

setClass(
  "annotated_numeric"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "numeric"
)


setClass(
  "annotated_integer"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "integer"
)


setClass(
  "annotated_character"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "character"
)


setClass(
  "annotated_logical"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "logical"
)


## Factor class needs some more brain:
setOldClass(Classes = "factor")

setClass(
  "annotated_factor"
  , slots = c(
    label = "character"
    , annotation = "vector_annotation"
  )
  , contains = "factor"
)


setClassUnion(
  name = "annotated_vector"
  , members = c(
    "annotated_numeric"
    , "annotated_integer"
    , "annotated_character"
    , "annotated_logical"
    , "annotated_factor"
  )
)

setMethod(
  "initialize"
  , "annotated_vector"
  , function(.Object, ...){
    .Object <- callNextMethod()
    .Object@label <- .Object@annotation@label
    .Object
  }
)

setMethod(
  "initialize"
  , "annotated_factor"
  , function(.Object, ...){
    .Object <- callNextMethod()
    .Object@label <- .Object@annotation@label
    .Object
  }
)

setValidity(
  "annotated_vector"
  , method = function(object){
    if(length(object@label)>0|length(object@annotation@label))
    if(object@label!=object@annotation@label)
       return('Slot "label" is not equal to slot "annotation@label"')
  }
)

## show methods
setMethod("show", "annotated_vector", function(object){
  cat('An object of formal class "', class(object), '":\n', sep = "")
  print(object@.Data)
})

# Build a more detailed show method for annotated factors:
setMethod("show", "annotated_factor", function(object){
  cat('An object of formal class "', class(object), '":\n', sep = "")
  print(object@levels[object@.Data], quote = FALSE)
  cat("Levels: ", object@levels)
})


#' Indexing of annotated vectors
#'
#' When subsetting a vector by "[", meta-data (e.g., label and unit of measurement)
#' are preserved.
#' @param x The annotated vector object to be subsetted.
#' @param i An
#'
#' @examples
#' a <- new("annotated_numeric", .Data = 1:4)
#' test <- a[1:2]
#' @rdname sub_annotated
#' @keywords internal

setMethod("[", "annotated_vector", function(x, i){
 new(
   class(x)
   , .Data = x@.Data[i]
   , label = x@label
   , annotation = x@annotation
 )
})

#' @rdname sub_annotated
#' @keywords internal

setMethod("[", "annotated_factor", function(x, i){
  new(
    "annotated_factor"
    , .Data = x@.Data[i]
    , label = x@label
    , annotation = x@annotation
    , levels = levels(x)
  )
})

#' Drop Unused Levels from Factors
#'
#' The function \code{droplevels} is used to drop unused levels from a factor or, more commonly,
#' from factors in a \code{data.frame}. This method preserves meta-data (e.g., label and unit of measurement).
#' @param x An annotated factor from which to drop unused levels.
#' @param ... Further arguments that can be passed, see \code{\link{droplevels}} for further options.
#'
#' @seealso \code{\link{droplevels}}
#'
#' @keywords internal

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

#' @title Drop Unused Levels
#' @description
#' This is an additional method that allows for more convenient programming:
#' If a vector is not a factor (annotated or not), do not change anything.
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

# setMethod(
#   f = "droplevels"
#   , signature = "data.frame"
#   , definition = function(x, ...){
#     # apply droplevels to all columns, method dispatch solves your problems:
#     as.data.frame(lapply(X = x, FUN = droplevels), stringsAsFactors = FALSE)
#   }
# )

#' Reorder Levels of Annotated Factor
#'
#' The levels of an annotated factor are re-ordered so that the level specified
#' by \code{ref} is first and the others are moved down. This is useful
#' for \code{contr.treatment} contrasts which take the first level as the
#' reference.
#'
#' @param x An object of class \code{annotated_factor}.
#' @param ref The reference level, typically a character.
#' You can also specify a character vector of any length: As many levels as you specified will be re-ordered.
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
#' Necessary for compatibility reasons.
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


#' Coerce vectors to annotated vectors
#'
#' Internal workhorse function wthat will soon be dropped.
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
    for (i in 1:ncol(object)){
      object[, i] <- as(object[, i], paste0("annotated_", class(object[, i])))
    }
    object
  }
)



#' @keywords internal

setAs(
  from = "vector"
  , to = "annotated_numeric"
  , def = function(from){
    new("annotated_numeric", .Data = as(from, "numeric"))
  }
)



#' @keywords internal

setAs(
  from = "vector"
  , to = "annotated_integer"
  , def = function(from){
    new("annotated_integer", .Data = as(from, "integer"))
  }
)


#' @keywords internal

setAs(
  from = "vector"
  , to = "annotated_character"
  , def = function(from){
    new("annotated_character", .Data = as(from, "character"))
  }
)



#' @keywords internal

setAs(
  from = "vector"
  , to = "annotated_logical"
  , def = function(from){
    new("annotated_logical", .Data = as(from, "logical"))
  }
)



#' @keywords internal

setAs(
  from = "vector"
  , to = "annotated_factor"
  , def = function(from){
    tmp <- factor(from)
    new("annotated_factor", tmp, levels = as(levels(tmp), "character"))
  }
)



#' @keywords internal

setAs(
  from = "integer"
  , to = "annotated_factor"
  , def = function(from){
    tmp <- factor(from)
    new("annotated_factor", tmp, levels = levels(tmp))
  }
)



#' @keywords internal

setAs(
  from = "annotated_factor"
  , to = "factor"
  , def = function(from){
    factor(from@levels[from@.Data], levels = from@levels)
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




# setMethod(
#   f = "reorder"
#   , signature = c(x = "annotated_factor")
#   , definition = function(x, ...){
#     cat("test")
#   }
# )

# setClass(
#   "annotated_data_frame"
#   , slots = c(
#     label = "character"
#   )
#   , contains = "list"
# )

# setAs(
#   from = "annotated_data_frame"
#   , to = "data.frame"
#   , def = function(from){
#     vlist <- lapply(X = from@.Data, FUN = function(x){x@.Data})
#     value <- structure(vlist, row.names = if(length(vlist)>0){1:length(vlist[[1]])}, .Names = names(vlist), class = "data.frame")
#     value
#   }
# )

# setMethod(
#   "show"
#   , "annotated_data_frame"
#   , function(object){
#     print(as(object = object, Class = "data.frame"))
#   }
# )

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

