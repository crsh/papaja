#' Format Bayes factors (APA 6th edition)
#'
#' These methods take result objects from the \pkg{BayesFactor} package to create
#'  formatted character strings to report the results in accordance with APA manuscript
#'  guidelines.
#'
#' @param x Output object. See details.
#' @param ratio_subscript Character. A brief description of the model comparison in the form of \code{"M1/M2"}.
#' @param auto_inverse Logical. Indicates whether the Bayes factor should be inverted (including \code{ratio_subscript}) if it is less than 1.
#' @param scientific Logical. Indicates whether to use scientific notation.
#' @param evidential_boost Numeric. Vector of the same length as \code{x} containing evidential boost factors for the
#'   corresponding models (see details).
#' @param ...
#'
#' @details For models with order restrictions, evidential boosts can be calculated based on the prior and posterior
#'   odds of the restriction (Morey & Wagenmakers, 2014). If evidential boost factors are passed to
#'   \code{evidential_boost} they are multiplied with the corresponding Bayes factor before the results are formatted.
#' @return
#'   ...
#' @references
#' Morey, R. D., & Wagenmakers, E.-J. (2014). Simple relation between Bayesian order-restricted and point-null
#'   hypothesis tests. \emph{Statistics & Probability Letters}, 92, 121--124. doi:
#'   \href{https://doi.org/10.1016/j.spl.2014.05.010}{10.1016/j.spl.2014.05.010}
#' @family apa_print
#' @importFrom stats formula terms setNames
#' @export
#'
#' @examples
#' \dontrun{
#' data(sleep)
#' bayesian_anova <- anovaBF(
#'   extra ~ group + ID
#'   , data = sleep
#'   , whichRandom = "ID"
#'   , progress=FALSE
#' )
#'
#' apa_print(bayesian_anova)
#' }

apa_print.BFBayesFactor <- function(x, ...) {
  if(length(x) > 1) {
    bf <- c()
    for(i in seq_along(x)) {
      bf[i] <- print_bf(x[i], ...)
    }
    bf <- as.list(bf)
    names(bf) <- names(x)$numerator
  } else bf <- print_bf(x, ...)

  apa_res <- apa_print_container()
  apa_res$statistic <- bf

  apa_res
}

#' @rdname apa_print.BFBayesFactor
#' @export

setMethod("apa_print", "BFBayesFactor", apa_print.BFBayesFactor)


#' @rdname apa_print.BFBayesFactor
#' @export

apa_print.BFBayesFactorTop <- function(x, ...) {
  x_BFBayesFactor <- BayesFactor::as.BFBayesFactor(x)

  ellipsis <- list(...)

  if(is.null(ellipsis$ratio_subscript)) ellipsis$ratio_subscript <- "01"

  bf <- c()
  for(i in seq_along(x_BFBayesFactor)) {
    ellipsis$x <- x_BFBayesFactor[i]
    bf <- c(bf, do.call("print_bf", ellipsis))
  }

  full_terms <- bf_term_labels(x@denominator)
  full_terms <- bf_sort_terms(full_terms)
  restricted_terms <- lapply(x@numerator, bf_term_labels)
  restricted_terms <- lapply(restricted_terms, bf_sort_terms)
  omitted_terms <- lapply(restricted_terms, function(x) full_terms[!full_terms %in% x])
  names(bf) <- sanitize_terms(omitted_terms)

  apa_res <- apa_print_container()
  apa_res$statistic <- as.list(rev(bf))

  apa_res
}


#' @rdname apa_print.BFBayesFactor
#' @export

setMethod("apa_print", "BFBayesFactorTop", apa_print.BFBayesFactorTop)


#' @rdname apa_print.BFBayesFactor
#' @export

apa_print.BFBayesFactorList <- function(x, ...) {
  bf <- vapply(x, print_bf, as.character(as.vector(x[[1]])))
  names(bf) <- names(x)

  apa_res <- apa_print_container()
  apa_res$statistic <- as.list(bf)

  apa_res
}

#' @rdname apa_print.BFBayesFactor
#' @export

setMethod("apa_print", "BFBayesFactorList", apa_print.BFBayesFactorList)



print_bf <- function(
  x
  , ratio_subscript = "10"
  , auto_invert = TRUE
  , scientific = TRUE
  , max = 1000
  , min = 1 / max
  , evidential_boost = NULL
  # , logbf = FALSE
  , ...
) {
  validate(x@bayesFactor["bf"], check_NA = TRUE)
  validate(ratio_subscript, check_class = "character", check_length = 1)
  validate(auto_invert, check_class = "logical", check_length = 1)
  validate(scientific, check_class = "logical", check_length = 1)
  validate(max, check_class = "numeric", check_length = 1)
  validate(min, check_class = "numeric", check_length = 1)
  validate(evidential_boost, check_class = "numeric", check_length = length(as.vector(x)))
  # validate(logbf, check_class = "logical", check_length = 1)

  ellipsis <- list(...)
  ellipsis$x <- as.vector(x)

  if(!is.null(evidential_boost)) {
    ellipsis$x <- ellipsis$x * evidential_boost
  }

  if(auto_invert) {
    to_invert <- ellipsis$x < 1
    ellipsis$x[to_invert] <- 1 / ellipsis$x[to_invert]

    ratio_subscript <- rep(ratio_subscript, length(x))
    ratio_subscript[to_invert] <- invert_subscript(ratio_subscript)
  }

  if(scientific & (ellipsis$x > max - 1 | ellipsis$x < min)) {
    ellipsis$format <- "e"
    if(is.null(ellipsis$digits)) ellipsis$digits <- 2

    bf <- do.call("formatC", ellipsis)
    bf <- typeset_scientific(bf)
  } else {
    if(is.null(ellipsis$zero)) ellipsis$zero <- FALSE
    bf <- do.call("printnum", ellipsis)
  }

  if(!grepl("<|>", bf)) eq <- " = " else eq <- " "

  bf <- paste0("$\\mathrm{BF}_{\\textrm{", ratio_subscript, "}}", eq, bf, "$")
  bf <- setNames(bf, names(x@numerator))
  bf
}


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

bf_term_labels <- function(x) {
  model_formula <- formula(x@identifier$formula)
  attr(terms(model_formula), "term.labels")
}

bf_sort_terms <- function(x) {
  sapply(strsplit(x, ":"), function(y) paste(sort(y), collapse = ":"))
}


# typeset_ratio_subscript <- function(x) {
#   gsub(
#     "(?<!\\\\)\\b([a-z:;,.]+[a-z:;,.\\- ]*[a-z:;,.]+|[a-z:;,.])"
#     , "\\textrm{\\1}"
#     , x
#     , perl = TRUE
#     , ignore.case = TRUE
#   )
# }
# typeset_ratio_subscript("M_{a und b}/M_{\alpha}")
