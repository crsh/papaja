#' Format Bayes factors (APA 6th edition)
#'
#' These methods take result objects from the \code{\pkg{BayesFactor}} package to create
#'  formatted chraracter strings to report the results in accordance with APA manuscript
#'  guidelines.
#'
#' @param x Output object. See details.
#' @param ratio_subscript Character. A brief description of the model comparison in the form of \code{"M1/M2"}.
#' @param auto_inverse Logical. Indicates whether the Bayes factor should be inverted (including \code{ratio_subscript}) if it is less than 1.
#' @param scientific Logical. Indicates whether to use scientific notation.
#' @param ...
#'
#' @return
#'   ...
#' @family apa_print
#' @export
#'
#' @examples
#' data(sleep)
#' bayesian_anova <- anovaBF(
#'   extra ~ group + ID
#'   , data = sleep
#'   , whichRandom = "ID"
#'   , progress=FALSE
#' )
#'
#' apa_print(bayesian_anova)

apa_print.BFBayesFactor <- function(
    x
    , ratio_subscript = "10"
    , auto_invert = TRUE
    , scientific = TRUE
    # , max = 1000 # Yields 99.9% posterior probability (assuming equal prior odds)
    # , min = 1 / max
    # , logbf = FALSE
    , ...
) {
  validate(x@bayesFactor["bf"], check_NA = TRUE)
  validate(ratio_subscript, check_class = "character", check_length = 1)
  validate(auto_invert, check_class = "logical", check_length = 1)
  validate(scientific, check_class = "logical", check_length = 1)
  # validate(max, check_class = "numeric", check_length = 1)
  # validate(logbf, check_class = "logical", check_length = 1)

  ellipsis <- list(...)

  bf <- as.vector(x)
  ellipsis$x <- bf

  if(auto_invert) {
    to_invert <- bf < 1
    bf[to_invert] <- 1 / bf[to_invert]

    ratio_subscript <- rep(ratio_subscript, length(x))
    ratio_subscript[to_invert] <- invert_subscript(ratio_subscript)
  }

  eq <- " = "

  if(scientific) {
    ellipsis$format <- "e"
    if(is.null(ellipsis$digits)) ellipsis$digits <- 2

    bf <- do.call("formatC", ellipsis)
    bf <- typeset_scientific(bf)
  } else {
    if(is.null(ellipsis$zero)) ellipsis$zero <- FALSE
    bf <- do.call("printnum", ellipsis)
  }

  bf <- paste0("$BF_{", ratio_subscript, "}", eq, bf, "$")
  bf <- setNames(bf, names(x@numerator))
  bf
}


#' @rdname apa_print.BFBayesFactor
#' @export
setMethod("apa_print", "BFBayesFactor", apa_print.BFBayesFactor)


#' @rdname apa_print.BFBayesFactor
#' @export

apa_print.BFBayesFactorList <- function(x, ...) {
  bf <- sapply(x, apa_print.BFBayesFactor, ...)
  names(bf) <- names(x)
  bf
}


#' @rdname apa_print.BFBayesFactor
#' @export
setMethod("apa_print", "BFBayesFactorList", apa_print.BFBayesFactorList)


invert_subscript <- function(x) {
  seperator <- if(nchar(x) == 2) "" else "/"
  paste(rev(unlist(strsplit(x, seperator))), collapse = "")
}

typeset_scientific <- function(x) {
  x <- gsub("e\\+00$", "", x)
  x <- gsub("e\\+0?(\\d+)$", " \\\\times 10\\^\\{\\1\\}", x)
  x <- gsub("e\\-0?(\\d+)$", " \\\\times 10\\^\\{-\\1\\}", x)
  x
}
