#' Typeset statistics
#'
#' A generic function that takes objects from various statistical methods to
#' create formatted character strings to report the results in accordance with
#' APA manuscript guidelines. The function invokes particular methods, which
#' depend on the \code{\link{class}} of the first argument.
#'
#' @param x A model object.
#' @param ... Additional arguments passed to methods.
#'
#' @evalRd apa_resutls_return_value()
#'
#' @family apa_print
#' @examples
#' # List methods for apa_print()
#' methods("apa_print")
#' @export

apa_print <- function(x, ...) {
  UseMethod("apa_print", x)
}


# #' @importClassesFrom BayesFactor BFBayesFactor
setGeneric("apa_print")

#' @method apa_print default
#' @export

apa_print.default <- function(x, ...) no_method(x)
