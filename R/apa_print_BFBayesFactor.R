#' Format Bayes factors (APA 6th edition)
#'
#' These methods take result objects from the \pkg{BayesFactor} package to create
#'  formatted character strings to report the results in accordance with APA manuscript
#'  guidelines. \emph{These methods are not properly tested and should be considered experimental.}
#'
#' @param x Output object. See details.
#' @param iterations Numeric. Number of iterations of the MCMC sampler to estimate HDIs from the posterior.
#' @param central_tendency Function to calculate central tendency of MCMC samples to obtain a point estimate from
#'   the posterior.
#' @param hdi Numeric. A single value (range [0, 1]) giving the credibility level of the HDI.
#' @param standardized Logical. Indicates whether to return standardized or unstandardized effect size estimates.
#' @param ratio_subscript Character. A brief description of the model comparison in the form of \code{"M1/M2"}.
#' @param auto_invert Logical. Indicates whether the Bayes factor should be inverted (including \code{ratio_subscript}) if it is less than 1.
#' @param scientific Logical. Indicates whether to use scientific notation.
#' @param max Numeric. Upper limit of the Bayes factor before switching to scientific notation.
#' @param min Numeric. Lower limit of the Bayes factor before switching to scientific notation.
#' @param evidential_boost Numeric. Vector of the same length as \code{x} containing evidential boost factors for the
#'   corresponding models (see details).
#' @inheritDotParams printnum.numeric -x -margin
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
#' @importFrom stats formula terms setNames median
#' @keywords internal
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

apa_print.BFBayesFactor <- function(
  x
  , iterations = 10000
  , central_tendency = median
  , hdi = 0.95
  , standardized = FALSE
  , ratio_subscript = "10"
  , auto_invert = TRUE
  , scientific = TRUE
  , max = 1000
  , min = 1 / max
  , evidential_boost = NULL
  , ...
) {
  if(length(x) > 1) {
    ellipsis <- list(...)
    ellipsis$ratio_subscript <- ratio_subscript
    ellipsis$auto_invert <- auto_invert
    ellipsis$scientific <- scientific
    ellipsis$max <- max
    ellipsis$min <- min

    bf <- c()
    for(i in seq_along(x)) {
      ellipsis$x <- x[i]
      if(!is.null(evidential_boost)) ellipsis$evidential_boost <- evidential_boost[i]
      # bf[i] <- print_bf(x[i], ...)
      bf[i] <- do.call("apa_print_bf", ellipsis)
    }
    bf <- as.list(bf)
    names(bf) <- names(x)$numerator
  } else bf <- apa_print_bf(x, ...)

  apa_res <- apa_print_container()
  apa_res$statistic <- bf

  if(class(x@numerator[[1]]) %in% c("BFoneSample", "BFindepSample")) {
    posterior_samples <- BayesFactor::posterior(x, index = 1, iterations = iterations)
    apa_res$estimate <- bf_estimates(
      x@numerator[[1]]
      , posterior_samples
      , central_tendency = central_tendency
      , hdi = hdi
      , standardized = standardized
    )
  }


  apa_res$full_result <- paste0(apa_res$estimate, ", ", apa_res$statistic)

  apa_res
}


#' @rdname apa_print.BFBayesFactor
#' @keywords internal
#' @export

apa_print.BFBayesFactorTop <- function(x, ...) {
  x_BFBayesFactor <- BayesFactor::as.BFBayesFactor(x)

  ellipsis <- list(...)

  if(is.null(ellipsis$ratio_subscript)) ellipsis$ratio_subscript <- "01"
  if(!is.null(ellipsis$evidential_boost)) evidential_boost <- ellipsis$evidential_boost

  bf <- c()
  for(i in seq_along(x_BFBayesFactor)) {
    ellipsis$x <- x_BFBayesFactor[i]
    if(!is.null(ellipsis$evidential_boost)) ellipsis$evidential_boost <- evidential_boost[i]
    bf <- c(bf, do.call("apa_print_bf", ellipsis))
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
#' @keywords internal
#' @export

apa_print.BFBayesFactorList <- function(x, ...) {
  bf <- vapply(x, apa_print_bf, as.character(as.vector(x[[1]])), ...)
  names(bf) <- names(x)

  apa_res <- apa_print_container()
  apa_res$statistic <- as.list(bf)

  apa_res
}


#' @rdname apa_print.BFBayesFactor
#' @export

setMethod(f = "apa_print", signature = "BFBayesFactor", definition = apa_print.BFBayesFactor)

#' @rdname apa_print.BFBayesFactor
#' @export

setMethod(f = "apa_print", signature = "BFBayesFactorTop", definition = apa_print.BFBayesFactorTop)


#' @rdname apa_print.BFBayesFactor
#' @export

setMethod("apa_print", "BFBayesFactorList", apa_print.BFBayesFactorList)


apa_print_bf <- function(x, ...) {
  UseMethod("apa_print_bf", x)
}

apa_print_bf.default <- function(x, ...) no_method(x)

apa_print_bf.numeric <- function(
  x
  , ratio_subscript = "10"
  , auto_invert = TRUE
  , escape = TRUE
  , scientific = TRUE
  , max = 1000
  , min = 1 / max
  , evidential_boost = NULL
  # , logbf = FALSE
  , ...
) {
  validate(x, check_NA = TRUE, check_infinite = FALSE)
  validate(ratio_subscript, check_class = "character", check_length = 1)
  validate(auto_invert, check_class = "logical", check_length = 1)
  validate(scientific, check_class = "logical", check_length = 1)
  validate(max, check_class = "numeric", check_length = 1)
  validate(min, check_class = "numeric", check_length = 1)
  if(!is.null(evidential_boost)) validate(evidential_boost, check_class = "numeric", check_length = length(x))
  # validate(logbf, check_class = "logical", check_length = 1)

  ellipsis <- list(...)
  ellipsis$x <- as.vector(x)
  ellipsis$use_math <- FALSE

  if(!is.null(evidential_boost)) {
    ellipsis$x <- ellipsis$x * evidential_boost
  }

  if(auto_invert) {
    to_invert <- ellipsis$x < 1
    ellipsis$x[to_invert] <- 1 / ellipsis$x[to_invert]

    ratio_subscript <- rep(ratio_subscript, length(x))
    ratio_subscript[to_invert] <- invert_subscript(ratio_subscript)
  }

  if(escape) {
    ratio_subscript <- paste0("\\textrm{", escape_latex(ratio_subscript), "}")
  }

  if(scientific & (ellipsis$x > max - 1 | ellipsis$x < min)) {
    ellipsis$format <- "e"
    if(is.null(ellipsis$digits)) ellipsis$digits <- 2

    bf <- do.call("printnum", ellipsis)
    bf <- typeset_scientific(bf)
  } else {
    if(is.null(ellipsis$zero)) ellipsis$zero <- FALSE
    bf <- do.call("printnum", ellipsis)
  }

  if(!grepl("<|>", bf)) eq <- " = " else eq <- " "

  bf <- paste0("$\\mathrm{BF}_{", ratio_subscript, "}", eq, bf, "$")
  bf <- setNames(bf, names(x))
  bf
}

apa_print_bf.BFBayesFactor <- function(x, ...) {
  validate(as.vector(x), check_NA = TRUE)
  bf <- apa_print_bf(as.vector(x))
  bf <- setNames(bf, names(x@numerator))
  bf
}


invert_subscript <- function(x) {
  seperator <- if(nchar(x) == 2) "" else "/"
  paste0(rev(unlist(strsplit(x, seperator))), collapse = seperator)
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


# bf_estimates <- function(x, ...) UseMethod("bf_estimates", x)

# bf_estimates.default <- function(x, ...) no_method(x)

setGeneric(
  name = "bf_estimates"
  , def = function(x, ...) standardGeneric("bf_estimates")
)

bf_estimates_ttest <- function(
  x
  , samples
  , central_tendency = median
  , hdi = 0.95
  , standardized = FALSE
) {
  validate(samples, check_class = "mcmc")
  validate(central_tendency, check_class = "function")
  validate(hdi, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(standardized, check_class = "logical", check_length = 1)

  if(standardized) {
    estimate <- "delta"
    est_name <- "d"
  } else {
    estimate <- ifelse(class(x) == "BFoneSample", "mu", "beta (x - y)")
    est_name <- "M"
  }

  samples <- as.numeric(samples[, estimate])

  est_mean <- central_tendency(samples)
  est_hdi <- hd_int(samples, level = hdi)

  estimate <- paste0("$", est_name, " = ", printnum(est_mean), "$ ", print_hdint(est_hdi))
  estimate
}

suppressMessages(
  setMethod(f = "bf_estimates", signature = "BFoneSample", definition = bf_estimates_ttest)
)

suppressMessages(
  setMethod(f = "bf_estimates", signature = "BFindepSample", definition = bf_estimates_ttest)
)




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
