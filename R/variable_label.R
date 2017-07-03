#' Variable labels
#'
#' Functions used to assign and extract variable labels of a vector or the columns of a \code{data.frame}.
#'
#' @param x Either a vector or a \code{data.frame} containing the variables you want to label.
#' @param value A vector with the variable label(s) to be assigned. Can be character, expression, etc.
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

  attr(x, which = "label") <- value

  if(!("labelled" %in% class(x))) {
    class(x) <- c(class(x), "labelled")
  }

  return(x)
}



#' @rdname variable_label
#' @export

assign_label.data.frame <- function(x, value){
  d <- mapply(FUN = assign_label, x = x, value = value, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  names(d) <- names(x)
  as.data.frame(d)
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


#' @rdname variable_lable
#' @export

"variable_label<-.default" <- function(x, value) {
  assign_label.default(x, value)
}

#' @rdname variable_lable
#' @export
"variable_label<-.data.frame" <- function(x, value) {
  assign_label.data.frame(x, value)
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
  variable_label(x[, columns]) <- colnames(x[, columns])
  x
}


combine_plotmath <- function(a, b, c = NULL, d = NULL){
  if (is.null(c)){
    if(any(is.expression(c(a, b)))){
      a <- as.expression(a)
      b <- as.expression(b)
      as.expression(substitute(paste(a, ": ", b), list(a = a[[1]], b = b[[1]])))
    } else {
      paste0(a, ": ", b)
    }
  } else {
    if(any(is.expression(c(a, b, c, d)))){
      a <- as.expression(a)
      b <- as.expression(b)
      c <- as.expression(c)
      d <- as.expression(d)
      as.expression(substitute(paste(a, ": ", b, " & ", c, ": ", d), list(a = a[[1]], b = b[[1]], c = c[[1]], d = d[[1]])))
    } else{
      paste0(a, ": ", b, " & ", c, ": ", d)
}
  }
}

#' #' Indexing of labelled stuff
#' #'
#' #'   This one helps preserve the attributes/label if column is modified
#' #' @export
#'
#' "[.labelled"<- function(x, ...) {
#'   original_labels <- papaja::variable_label(x)
#'   x <- NextMethod("[")
#'   papaja::variable_label(x) <- original_labels
#'   if(!("labelled" %in% class(x))) {
#'     class(x) <- c(class(x), "labelled")
#'   }
#'   x
#' }
#'
#'




# variable_label(npk$L) <- "test2"
# class(npk$L)
# a <- expression(italic(A))
# b <- "b"
#
# plot.new()
# plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
# text(0, 0, combine_plotmath(a, b))
