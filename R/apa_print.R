#' Format statistics (APA 6th edition)
#'
#' A generic function that takes objects from various statistical methods to create formatted character
#' strings to report the results in accordance with APA manuscript guidelines. The function invokes particular
#' methods, which depend on the \code{\link{class}} of the first argument.
#'
#' @param x Output object. See details.
#' @param ... Additional arguments passed to other methods. See details.
#' @details Currently the following output objects are supported:
#'
#'    \itemize{
#'      \item \code{htest}
#'      \item \code{lm} and \code{summary.lm}
#'      \item \code{aov}, \code{aovlist}, \code{summary.aov}, and \code{summary.aovlist}
#'      \item \code{anova} and \code{Anova.mlm}
#'    }
#'
#' @return The form of the value returned by \code{apa_print} depends on the class of \code{x}. See the
#'    documentation of the particular methods for details (e.g., \code{?apa_print.htest}).
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
