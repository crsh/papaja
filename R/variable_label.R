#' Variable labels
#'
#' Functions used to assign and extract variable labels of a vector or the columns (i.e., vectors) of a \code{data.frame}.
#'
#' @param x Either a vector or a \code{data.frame} containing the variables you want to label.
#' @param value A vector with the variable label(s) to be assigned. Can be anything like, e.g. character, expression, etc.
#'
#' @return
#'         \code{variable_label} returns the variable labels stored as attributes to the coulumns of a \code{data.frame}.
#'         \code{assign_label} return \code{x} with the added variable labels.
#'
#' @rdname variable_label
#' @export

assign_label <- function(x, value) {
  if(missing(value)) stop("You did not provide a value to assign as a variable label.")
  UseMethod("assign_label", x)
}



#' @rdname variable_label
#' @export

assign_label.default <- function(x, value) {

  validate(value, check_dim = 1, check_length = 1)
  attr(x, which = "label") <- value

  if(!("labelled" %in% class(x))) {
    class(x) <- c("labelled", class(x))
  }

  return(x)
}



#' @rdname variable_label
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
        , "\nThe number of labels needs to match the number of columns."
      )
    )
  }
  d <- mapply(FUN = assign_label, x = x, value = value, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  as.data.frame(d, col.names = names(x))
}



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
  mapply(x = x, FUN = variable_label, SIMPLIFY = FALSE)
}


#' @rdname variable_label
#' @export

"variable_label<-" <- function(x, ..., value) {
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

#' @export

"[.labelled"<- function(x, ...) {
  at1 <- attr(x, which = "label")
  at2 <- attr(x, which = "class")
  x <- NextMethod("[")
  attr(x, which = "label") <- at1
  attr(x, which = "class") <- at2
  x
}

setGeneric("factor")

#' Beautifual
#'
#' beautifuls tuff to write down
#'
#'
#' @method factor labelled
#' @export

factor.labelled <-function(x, ...){
  original_labels <- variable_label(x)
  original_classes <- class(x)
  x <- base::factor(x, ...)
  variable_label(x) <- original_labels
  class(x) <- original_classes
  x
}


setGeneric("factor")

factor.default <- factor

#' methods for droplevels
#'
#' stuff stuff stuff
#'
#' @export
droplevels.labelled <- function(x, exclude = if (anyNA(levels(x))) NULL else NA, ...){
  papaja::factor.labelled(x, exclude = exclude)
}


#' methods for droplevels
#'
#' stuff stuff stuff
#'
#' @export
relevel.labelled <- function(x){
  relevel(x)
}


#' Set default variable labels from column names
#'
#' description fhowhrfguh serhfg
#'

default_label <- function(x){
  UseMethod("default_label", x)
}


#' @rdname default_label
#' @export

default_label.data.frame <- function(x){
  columns <- sapply(X = variable_label(x), FUN = is.null, simplify = TRUE)
  # print(columns)
  if(any(columns)){
    variable_label(x[, columns]) <- colnames(x)[columns]
  }
  x
}


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

tex_conv <- function(x){
  if(!is.null(x)){
    if(!is.expression(x)){
      if(papaja:::package_available("latex2exp")){
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




