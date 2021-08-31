#' Format Statistics (APA 6th edition)
#'
#' A generic function that takes objects from various statistical methods to create formatted character
#' strings to report the results in accordance with APA manuscript guidelines. The function invokes particular
#' methods, which depend on the \code{\link{class}} of the first argument.
#'
#' @param x A model object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#'  All methods return a named list of four elements (with additional class `apa_results`).
#'
#'  The first three elements (`$estimate`, `$statistic`, and `$full_result`) contain character strings that
#'  may be used to include the results of a statistical analysis into the text
#'  body of an R Markdown document using an in-line code chunk.
#'
#'  By contrast, `$table` may be used to include a complete results table
#'  into the document with the help of [apa_table()].
#'
#'  - `$estimate` contains (sometimes standardized) estimates of effect sizes,
#'  - `$statistic` contains the results from a corresponding statistical test,
#'  - `$full_result` combines `estimate` and `statistic`, and
#'  - `$table` contains a tabular summary of the analysis.
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
