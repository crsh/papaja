
# Classes
# ~~BFBayesFactor~~
# ~~BFBayesFactorTop~~
# ~~BFcontigencyTable~~
# ~~BFcorrelation~~
# ~~BFindepSample~~
# ~~BFoneSample~~
# ~~BFlinearModel~~
# ~~BFproportion~~
# BFBayesFactorList
# BFprobability


#' Typeset Bayes Factors
#'
#' These methods take result objects from the \pkg{BayesFactor} package to
#' create formatted character strings to report the results in accordance with
#' APA manuscript guidelines.
#'
#' @param x Output object. See details.
#' @param stat_name Character. If `NULL` (the default), the name given in `x`
#'   is used for the *test statistic*, otherwise the supplied name is used.
#'   See details.
#' @param est_name Character. If `NULL` (the default), the name given in `x`
#'   (or a formally correct adaptation) is used for the *estimate*, otherwise
#'   the supplied name is used. See details.
#' @param subscript Character. Index used to specify the model comparison for
#'   the Bayes factors, e.g., `"+0"` yields \eqn{BF_{+0}}. If `NULL` default to
#'   "10".
#' @param escape_subscript Logical. If `TRUE` special LaTeX characters, such as
#'   \code{\%} or \code{_}, in `subscript` are escaped.
#' @param scientific_threshold Numeric. Named vector of length 2 taking the form
#'   `c(min = 1/10, max = 1e6)`. Bayes factors that exceed these thresholds will
#'   be printed in scientific notation.
#' @param reciprocal Logical. If `TRUE` the reciprocal of all Bayes factors is
#'   taken before results are formatted. The advantage over specifying
#'   `x = t(x)` is that the default (*only* the default) index specifying the
#'   model comparison is automatically reversed, see `subscript`.
#' @param log Logical. If `TRUE` the logarithm of the Bayes factor is reported.
#' @param mcmc_error Logical. If `TRUE` estimation error of the Bayes factor(s)
#'   is reported.
#' @param iterations Numeric. Number of iterations of the MCMC sampler to
#'   estimate HDIs from the posterior.
#' @param standardized Logical. Whether to return standardized or
#'   unstandardized effect size estimates.
#' @param central_tendency Function to calculate central tendency of MCMC
#'   samples to obtain a point estimate from the posterior.
#' @param interval Function to calculate an interval estimate of MCMC
#'   samples from the posterior. The returned object must be either a named
#'   vector or matrix with (column) names giving the interval bounds
#'   (e.g., `2.5%` and `97.5%`) or with an attribute `conf.level` (e.g., `0.95`).
#' @param interval_type Character. Used to specify the type of interval in the
#'   formatted text.
#' @param bf_r1 Numeric. Vector of the same length as `x` giving Bayes factors
#'   in favor of an order constraint relative to the unconstrained model
#'   (see details). Must be on log-scale if `log = TRUE`.
#' @param bf_1r Numeric. Same as `bf_r1` (see details).
#' @inheritDotParams apa_num.numeric -x
#'
#' @details
#'
#'   `stat_name` and `est_name` are placed in the output string and are
#'   thus passed to pandoc or LaTeX through \pkg{knitr}. To the extent it is
#'   supported by the final document type, you can pass LaTeX-markup to format
#'   the final text (e.g., \code{M_\\Delta} yields \eqn{M_\Delta}).
#'
#'   For models with order constraint, the evidence for the order constraint
#'   relative to the null model can be obtained by multiplying the
#'   Bayes factor \eqn{BF_{r1}} for the order constraint relative to the
#'   unconstrained model (`bf_r1`) with the Bayes factor \eqn{BF_{10}} for the
#'   unconstrained model relative to the null model,
#'
#'   \deqn{\frac{p(y \mid {\cal M}_r)}{p(y \mid {\cal M}_0)} = \frac{p(y \mid {\cal M}_r)}{p(y \mid {\cal M}_1)} \times \frac{p(y \mid {\cal M}_1)}{p(y \mid {\cal M}_0)}}.
#'
#'   \eqn{BF_{r1}} can be calculated from the prior and posterior odds of the
#'   order constraint (e.g., Morey & Wagenmakers, 2014). If `bf_r1` (or
#'   `bf_1r`) is specified they are multiplied with the corresponding Bayes
#'   factor supplied in `x` before the reciprocal is taken and the results are
#'   formatted. Note, that it is not possible to determine whether `x` gives
#'   \eqn{BF_{10}} or \eqn{BF_{01}} and, hence, `bf_r1` and `bf_1r` are treated
#'   identically; the different argument names only serve to ensure the
#'   expressiveness of the code. It is the user's responsibility to ensure that
#'   the supplied Bayes factor is correct!
#'
#' @evalRd apa_results_return_value()
#'
#' @references
#' Morey, R. D., & Wagenmakers, E.-J. (2014). Simple relation between Bayesian
#'   order-restricted and point-null hypothesis tests. \emph{Statistics &
#'   Probability Letters}, 92, 121--124. doi:
#'   \doi{10.1016/j.spl.2014.05.010}
#' @family apa_print
#' @importFrom stats formula terms setNames median
#' @method apa_print BFBayesFactor
#' @export
#'
#' @examples
#' # ANOVA
#' \donttest{
#' data(sleep, package = "BayesFactor")
#' bayesian_anova <- BayesFactor::anovaBF(
#'   extra ~ group + ID
#'   , data = sleep
#'   , whichRandom = "ID"
#'   , progress = FALSE
#' )
#'
#' # Paired t-test
#' ttest_paired <- BayesFactor::ttestBF(
#'   x = sleep$extra[sleep$group == 1]
#'   , y = sleep$extra[sleep$group == 2]
#'   , paired = TRUE
#' )
#'
#' # Results for paired t-tests are indistinguishable
#' # from one-sample t-tests. We therefore specify the
#' # appropriate `est_name` manually.
#' apa_print(
#'   ttest_paired
#'   , est_name = "M_D"
#'   , iterations = 1000
#' )
#'
#' apa_print(
#'   ttest_paired
#'   , iterations = 1000
#'   , interval = function(x) quantile(x, probs = c(0.025, 0.975))
#'   , interval_type = "CrI"
#' )
#' }

apa_print.BFBayesFactor <- function(
  x
  , stat_name = NULL
  , est_name = NULL
  , subscript = NULL
  , escape_subscript = FALSE
  , scientific_threshold = NULL
  , reciprocal = FALSE
  , log = FALSE
  , mcmc_error = any(x@bayesFactor$error > 0.05)
  , iterations = 10000
  , standardized = FALSE
  , central_tendency = median
  , interval = hd_int
  , interval_type = "HDI"
  , bf_r1 = NULL
  , bf_1r = NULL
  , ...
) {

  if(!is.null(stat_name)) validate(stat_name, check_class = "character", check_length = 1)
  if(!is.null(est_name)) validate(est_name, check_class = "character", check_length = 1)
  if(!is.null(subscript)) validate(subscript, check_class = "character", check_length = 1)

  validate(escape_subscript, check_class = "logical", check_length = 1)
  if(!is.null(scientific_threshold)) validate(scientific_threshold, check_class = "numeric", check_length = 2, check_infinite = FALSE)
  validate(reciprocal, check_class = "logical", check_length = 1)
  validate(log, check_class = "logical", check_length = 1)

  validate(interval_type, check_class = "character", check_length = 1)

  if(!is.null(bf_r1) & !is.null(bf_1r)) {
    stop("Please supply either `bf_r1` or `bf_1r`, not both.")
  }
  if(!is.null(bf_r1)) validate(bf_r1, check_class = "numeric", check_range = c(0, Inf))
  if(!is.null(bf_1r)) {
    validate(bf_1r, check_class = "numeric", check_range = c(0, Inf))
    bf_r1 <- bf_1r
  }

  ellipsis <- list(...)
  ellipsis$standardized <- isTRUE(standardized)

  if(!is.null(ellipsis$scientific)) {
    warning(
      "The argument `scientific` has been deprecated."
      , " To disable scientific notation use "
      , "`scientific_threshold = c(min = -Inf, max = Inf)`."
    )

    ellipsis$scientific <- NULL
  }

  if(!is.null(ellipsis$min) && !is.null(ellipsis$max)) {
    warning(
      "The arguments `min` and `max` have been deprecated."
      , " Please use `scientific_threshold` instead."
    )

    scientific_threshold <- c(min = ellipsis$min, max = ellipsis$max)
    ellipsis$min <- NULL
    ellipsis$max <- NULL
  }

  if(!is.null(ellipsis$auto_invert)) {
    warning(
      "The argument `auto_invert` has been deprecated."
      , " To control the direction of the Bayes factor use `reciprocal`."
    )

    ellipsis$auto_invert <- NULL
  }

  if(!is.null(ellipsis$ratio_subscript)) {
    warning(
      "The argument `ratio_subscript` has been deprecated."
      , " Please use `subscript` instead."
    )

    subscript <- ellipsis$ratio_subscript
    ellipsis$ratio_subscript <- NULL
  }

  if(!is.null(ellipsis$escape)) {
    warning(
      "The argument `escape` has been deprecated."
      , " Please use `escape_subscript` instead."
    )
    escape_subscript <- ellipsis$escape
    ellipsis$escape <- NULL
  }

  if(!is.null(ellipsis$evidential_boost)) {
    warning(
      "The argument `evidential_boost` has been deprecated."
      , " Please use `bf_r1` or `bf_1r` instead."
    )
    bf_r1 <- ellipsis$evidential_boost
    ellipsis$evidential_boost <- NULL
  }

  if(!is.null(ellipsis$hdi)) {
    warning(
      "The argument `hdi` has been deprecated."
      , " Please use `interval` instead."
    )
    ellipsis$hdi <- NULL
  }

  args_stat <- list()

  bf_colname <- "bf10"

  x_df <- as.data.frame(x)
  x_df$error <- x_df$error * 100 # Error to error percent
  x_df <- add_alternative(x_df, range = bf_theta_range(x))
  x_df <- rename_column(x_df, "bf", bf_colname)
  x_df$code <- NULL
  x_df$time <- NULL

  if(log) {
    x_df[[bf_colname]] <- x@bayesFactor$bf

    old_bf_colname <- bf_colname
    bf_colname <- "logbf10"
    x_df <- rename_column(x_df, old_bf_colname, bf_colname)

    if(is.null(scientific_threshold)) {
      scientific_threshold <- c(min = -1e6, max = 1e6)
    }
  } else {
    if(is.null(scientific_threshold)) {
      scientific_threshold <- c(min = 1/10, max = 1e6)
    }
  }

  if(is.null(names(scientific_threshold))) {
    names(scientific_threshold) <- c("min", "max")
  }

  if(!is.null(bf_r1)) {
    boost <- if(log) `+` else `*`
    x_df[[bf_colname]] <- boost(x_df[[bf_colname]], bf_r1)
  }

  if(reciprocal) {
    x_df[[bf_colname]] <- if(log) -x_df[[bf_colname]] else 1/x_df[[bf_colname]]
    old_bf_colname <- bf_colname
    bf_colname <- gsub("10", "01", bf_colname)
    x_df <- rename_column(x_df, old_bf_colname, bf_colname)
  }

  scientific <- any(x_df[[bf_colname]] < scientific_threshold["min"]) ||
    any(x_df[[bf_colname]] > scientific_threshold["max"])

  if(scientific) {
    args_stat$format <- "e"
    if(is.null(args_stat$digits)) args_stat$digits <- 2
  }

  x_df <- bf_add_names(x, x_df)

  x_df <- bf_add_estimates(
    x
    , x_df
    , iterations = iterations
    , standardized = standardized
    , central_tendency = central_tendency
    , interval = interval
  )

  x_canonized <- canonize(x_df, interval_type = interval_type)
  if(is.null(stat_name) && !is.null(subscript)) {
    if(escape_subscript) {
      subscript <- escape_latex(subscript)
    }
    variable_label(x_canonized$statistic) <-
    gsub("_\\{\\\\textrm\\{.+\\}\\}", paste0("_{\\\\textrm{", subscript, "}}"), variable_label(x_canonized$statistic))
  }

  mcmc_error_na <- all(is.na(x_canonized$mcmc.error))

  # error_label <- variable_label(x_canonized$mcmc.error)
  x_canonized$mcmc.error <- apa_num(x_canonized$mcmc.error, na_string = "")
  # variable_label(x_canonized$mcmc.error) <- error_label

  ellipsis$x <- x_canonized
  ellipsis$args_stat <- args_stat
  if(any("proportion" %in% colnames(x_df)) & is.null(ellipsis$gt1)) {
    ellipsis$gt1 <- FALSE
  }

  x_beautified <- do.call("beautify", ellipsis)
  if(!mcmc_error || mcmc_error_na) x_beautified$mcmc.error <- NULL
  if(mcmc_error_na) message("All MCMC errors for Bayes factors are NA. The column `mcmc.error` will be omitted. Consider setting `mcmc_error = FALSE`.")

  if(!is.null(est_name)) {
    if(!("estimate" %in% colnames(x_beautified))) stop("No estimate available in results table. `est_name` cannot be used.")
    variable_label(x_beautified) <- c(estimate = paste0("$", est_name, "$"))
  }
  if(!is.null(stat_name)) {
    if(!("statistic" %in% colnames(x_beautified))) stop("No statistic available in results table. `stat_name` cannot be used.")
    variable_label(x_beautified) <- c(statistic = paste0("$", stat_name, "$"))
  }

  term_names <- NULL
  if("alternative" %in% colnames(x_beautified)) {
    term_names <- c("interval", "inverse_interval")
  }

  # Create output object ----
  glue_apa_results(
    x_beautified
    , est_glue = construct_glue(x_beautified, "estimate")
    , stat_glue = construct_glue(x_beautified, "statistic")
    , term_names = term_names
  )
}


#' @rdname apa_print.BFBayesFactor
#' @method apa_print BFBayesFactorTop
#' @export

apa_print.BFBayesFactorTop <- function(x, reciprocal = FALSE, ...) {
  x_BFBayesFactor <- BayesFactor::as.BFBayesFactor(x)

  full_terms <- bf_term_labels(x@denominator)
  full_terms <- bf_sort_terms(full_terms)
  restricted_terms <- lapply(x@numerator, bf_term_labels)
  restricted_terms <- lapply(restricted_terms, bf_sort_terms)
  omitted_terms <- lapply(restricted_terms, function(x) full_terms[!full_terms %in% x])

  x_BFBayesFactor@numerator <- mapply(
    function(y, z) {
      y@shortName <- z; y
    }
    , y = x_BFBayesFactor@numerator
    , z = omitted_terms
  )

  ellipsis <- list(...)
  ellipsis$x <- x_BFBayesFactor
  ellipsis$reciprocal <- reciprocal
  if(is.null(ellipsis$subscript)) {
    ellipsis$subscript <- if(!reciprocal) "01" else "10"
  }

  res <- do.call("apa_print", ellipsis)

  res$table <- rename_column(res$table, "model", "term")
  variable_label(res$table$term) <- "Term"

  res
}

bf_term_labels <- function(x) {
  model_formula <- formula(x@identifier$formula)
  attr(terms(model_formula), "term.labels")
}

bf_sort_terms <- function(x) {
  sapply(strsplit(x, ":"), function(y) paste(sort(y), collapse = ":"))
}

# #' @rdname apa_print.BFBayesFactor
# #' @keywords internal
# #' @method apa_print BFBayesFactorList
# #' @export

# apa_print.BFBayesFactorList <- function(x, ...) {
#   bf <- vapply(x, apa_print_bf, as.character(as.vector(x[[1]])), ...)
#   names(bf) <- names(x)

#   apa_res <- init_apa_results()
#   apa_res$statistic <- as.list(bf)

#   apa_res

  # test <- setNames(
  #     lapply(bfList@.Data, as.data.frame)
  #     , names(bfList)
  # )

  # do.call("rbind", test) |>
  #   tibble::rownames_to_column(var = "model") |>
  #   tidyr::separate("model", into = c("num", "denom"), sep = "\\.") # Not sure the order is correct

# }

#' @keywords internal

bf_add_estimates <- function(x, ...) UseMethod("bf_add_estimates", x@numerator[[1]])

#' @keywords internal

bf_add_estimates.default <- function(x, data_frame, ...) data_frame

#' @keywords internal

bf_add_estimates.BFoneSample   <- function(
  x
  , data_frame
  , standardized = FALSE
  , ...
) {
  validate(standardized, check_class = "logical", check_length = 1)

  posterior_samples <- BayesFactor::posterior(
    x
    , index = 1
    , iterations = 1
    , progress = FALSE
  )

  if(standardized) {
    est_name <- "delta"
    estimate <- "delta"
  } else {
    est_name <- "mean"
    estimate <- if(inherits(x@numerator[[1]], "BFoneSample")) {
      "mu"
    } else {
      # part in parentheses changes if formula method is used, so we have to grep 'beta'
      colnames(posterior_samples)[grep(colnames(posterior_samples), pattern = "beta", fixed = TRUE)]
    }
  }

  bf_sample_summarize(
    x
    , data_frame
    , estimate = estimate
    , est_name = est_name
    , ...
  )
}

#' @keywords internal

bf_add_estimates.BFindepSample <- function(
  x
  , data_frame
  , standardized = FALSE
  , ...
) {
  validate(standardized, check_class = "logical", check_length = 1)

  posterior_samples <- BayesFactor::posterior(
    x
    , index = 1
    , iterations = 1
    , progress = FALSE
  )

  if(standardized) {
    est_name <- "delta"
    estimate <- "delta"
  } else {
    est_name <- "difference.in.means"
    estimate <- colnames(posterior_samples)[grep(colnames(posterior_samples), pattern = "beta", fixed = TRUE)]
  }

  bf_sample_summarize(
    x
    , data_frame
    , estimate = estimate
    , est_name = est_name
    , ...
  )
}

#' @keywords internal

bf_add_estimates.BFcorrelation <- function(
  x
  , data_frame
  , ...
) {

  bf_sample_summarize(
    x
    , data_frame
    , estimate = "rho"
    , est_name = "cor"
    , ...
  )
}

#' @keywords internal

bf_add_estimates.BFproportion <- function(
  x
  , data_frame
  , ...
) {

  estimate <- "p"
  est_name <- "p"

  bf_sample_summarize(
    x
    , data_frame
    , estimate = "p"
    , est_name = "proportion"
    , ...
  )
}

#' @keywords internal

bf_sample_summarize <- function(
  x
  , data_frame
  , iterations = 10000
  , estimate = NULL
  , est_name = NULL
  , central_tendency = median
  , interval = hd_int
  , ...
) {
  validate(iterations, check_class = "numeric", check_length = 1, check_range = c(0, Inf))
  validate(estimate, check_class = "character", check_length = 1)
  validate(interval, check_class = "function", check_length = 1)

  # model column only contains HA short name
  data_frame$model <- NULL

  posterior_samples <- BayesFactor::posterior(
    x
    , index = 1
    , iterations = iterations
    , progress = FALSE
  )

  posterior_samples <- as.numeric(posterior_samples[, estimate])

  est_mean <- central_tendency(posterior_samples)
  est_interval <- interval(posterior_samples)

  data_frame[[est_name]] <- est_mean
  data_frame$hd.int <- list(est_interval)

  data_frame
}

#' @keywords internal

bf_add_names <- function(x, ...) UseMethod("bf_add_names", x@numerator[[1]])

#' @keywords internal

bf_add_names.default <- function(x, data_frame, ...) data_frame

#' @keywords internal

bf_add_names.BFlinearModel <- function(x, data_frame, ...) {
  cbind(
    model = names(x)$numerator
    , data_frame
  )
}

bf_theta_range <- function(x) {
  switch(
    class(x@numerator[[1]])
    , BFcorrelation = c(-1, 1)
    , BFproportion = c(0, 1)
    , c(-Inf, Inf)
  )
}

add_alternative <- function(x, range = NULL) {
  alt <- rownames(x)

  interval_hypothesis <- any(grepl("<", alt))
  if(interval_hypothesis) {
    intervals <- .str_extract_first(
      alt
      , "(!\\()*[\\w.]+<\\w+<[\\w.]+\\)*"
      , perl = TRUE
    )

    interval_bounds <- .str_extract_all(intervals, "[\\d.]+|-*Inf", perl = TRUE)
    is_inverse <- grepl("!", intervals)
    intervals <- sapply(
      seq_along(interval_bounds)
      , function(x) {
        if(!is_inverse[x]) {
          apa_interval(
            as.numeric(interval_bounds[[x]])
            , use_math = FALSE
          )
        } else {
          bounds <- as.numeric(interval_bounds[[x]])

          on_bound <- bounds %in% range
          if(any(on_bound)) {
            bounds <- sort(c(bounds[!on_bound], range[!on_bound]))
            apa_interval(
              bounds
              , use_math = FALSE
            )
          } else {
            bounds <- list(c(range[1], bounds[1]), c(bounds[2], range[2]))
            inverse_interval <- sapply(bounds, apa_interval, use_math = FALSE)
            # paste0("(", paste(inverse_interval, collapse = "~\\cup~"), ")")
            paste(inverse_interval, collapse = "~\\cup~")
          }
        }
      }
    )

    # scale <- .str_extract_first(alt, "r=\\d+\\.\\d+")
    # paste0("$\\text{Cauchy}(", scale, ")^{\\mathcal{T}", intervals, "}$")
    res <- cbind(x, alternative = paste0("$", intervals, "$"))
  } else {
    res <- x
  }

  rownames(res) <- NULL
  res
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
