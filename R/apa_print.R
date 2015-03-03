#' Format statistics (APA 6th edition)
#'
#' \code{apa_print} is a generic function that takes objects from various statistical methods to create
#' formated chraracter strings to report the results in accordance with APA manuscript guidelines. The
#' function invokes particular methods which depend on the \code{\link{class}} of the first argument.
#'
#' @param x Output object. See details.
#' @param ... Additional arguments passed to other methods. See details.
#' @details Currently the following output objects are supported:
#'
#'    \itemize{
#'      \item \code{htest}
#'      \item \code{lm} and \code{summary.lm}
#'      %\item \code{anova}
#'    }
#'
#' @return The form of the valued returned by \code{apa_print} depends on the class of \code{x}. See the
#'    documentation of the particular methods for details of what is produced by that method (e.g.,
#'    \code{?apa_print.htest}).
#'
#' @family apa_print
#' @examples
#' # List methods for apa_print()
#' methods("apa_print")
#' @export

apa_print <- function(x, ...) {
  validate(x)
  UseMethod("apa_print", x)
}

apa_print.default <- function(x, ...) {
  stop(paste0("Objects of class '", class(x), "' are currently not supported (no method defined).
              Visit https://github.com/crsh/papaja/issues to request support for this class."))
}