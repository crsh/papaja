#' Typeset Bayes Factors
#'
#' These methods take result objects from the \pkg{BayesFactor} package to
#' create formatted character strings to report the results in accordance with
#' APA manuscript guidelines. *These methods are not properly tested and should
#' be considered experimental.*
#'
#' @param x Output object. See details.
#' @param iterations Numeric. Number of iterations of the MCMC sampler to
#'   estimate HDIs from the posterior.
#' @param central_tendency Function to calculate central tendency of MCMC
#'   samples to obtain a point estimate from the posterior.
#' @param hdi Numeric. A single value (range \[0, 1\]) giving the credibility
#'   level of the HDI.
#' @param standardized Logical. Whether to return standardized or
#'   unstandardized effect size estimates.
#' @param ratio_subscript Character. A brief description of the model
#'   comparison, in the form of `"M1/M2"`.
#' @param auto_invert Logical. Indicates whether the Bayes factor should be
#'   inverted (including `ratio_subscript`) if it is less than 1.
#' @param scientific Logical. Indicates whether to use scientific notation.
#' @param max Numeric. Upper limit of the Bayes factor before switching to
#'   scientific notation.
#' @param min Numeric. Lower limit of the Bayes factor before switching to
#'   scientific notation.
#' @param evidential_boost Numeric. Vector of the same length as `x`
#'   containing evidential boost factors for the corresponding models
#'   (see details).
#' @inheritDotParams printnum.numeric -x
#'
#' @details
#'   For models with order restrictions, evidential boosts can be calculated
#'   based on the prior and posterior odds of the restriction
#'   (e.g., Morey & Wagenmakers, 2014). If evidential boost factors are passed
#'   to `evidential_boost` they are multiplied with the corresponding Bayes
#'   factor before the results are formatted.
#'
#' @evalRd apa_results_return_value()
#'
#' @references
#' Morey, R. D., & Wagenmakers, E.-J. (2014). Simple relation between Bayesian
#'   order-restricted and point-null hypothesis tests. \emph{Statistics &
#'   Probability Letters}, 92, 121--124. doi:
#'   \href{https://doi.org/10.1016/j.spl.2014.05.010}{10.1016/j.spl.2014.05.010}
#' @family apa_print
#' @importFrom stats formula terms setNames median
#' @keywords internal
#' @method apa_print BFBayesFactor
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

# TODO: Update documentation
# TODO: Note in documentation that it's not possible to determine if paired = TRUE

# set.seed(123)
#     ttest_paired <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2], paired = TRUE)
#
#     ttest_paired_output <- apa_print(
#       ttest_paired
#       , est_name = "\\Delta M"
#       , central_tendency = median
#       , iterations = 10000
#     )

# Classes
# ~~BFBayesFactor~~
# ~~BFBayesFactorTop~~
# ~~BFBayesFactorList~~
# ~~BFcontigencyTable~~
# ~~BFcorrelation~~
# ~~BFindepSample~~
# ~~BFoneSample~~
# ~~BFlinearModel~~
# BFprobability
# ~~BFproportion~~

apa_print.BFBayesFactor <- function(
  x
  , est_name = NULL
  , stat_name = NULL
  , subscript = NULL
  , escape_subscript = FALSE
  , scientific_threshold = c(min = 1/10, max = 1e6)
  , inverse = FALSE
  , log = FALSE
  , mcmc_error = TRUE
  , evidential_boost = NULL
  , iterations = 10000
  , central_tendency = median
  , hdi = 0.95
  , standardized = FALSE
  , ...
) {
  # TODO: deprecate ratio_subscript, escape, auto_invert, scientific, min, max
  # TODO: Add conf_fun and conf_level instead of hdi, deprecate hdi

  if(!is.null(stat_name)) validate(stat_name, check_class = "character", check_length = 1)
  if(!is.null(est_name)) validate(est_name, check_class = "character", check_length = 1)
  if(!is.null(subscript)) validate(subscript, check_class = "character", check_length = 1)
  
  validate(escape_subscript, check_class = "logical", check_length = 1)
  validate(scientific_threshold, check_class = "numeric", check_length = 2, check_infinite = FALSE)
  validate(inverse, check_class = "logical", check_length = 1)
  validate(log, check_class = "logical", check_length = 1)

  if(!is.null(evidential_boost)) validate(evidential_boost, check_class = "numeric", check_range = c(0, Inf))

  ellipsis <- list(...)
  ellipsis$standardized <- isTRUE(standardized)
  
  args_stat <- list()

  bf_colname <- "bf10"

  x_df <- as.data.frame(x)
  x_df <- add_alternative(x_df, range = bf_theta_range(x))
  x_df <- rename_column(x_df, "bf", bf_colname)
  x_df$code <- NULL
  x_df$time <- NULL

  if(log) {
    x_df[[bf_colname]] <- x@bayesFactor$bf

    old_bf_colname <- bf_colname
    bf_colname <- "logbf10"
    x_df <- rename_column(x_df, old_bf_colname, bf_colname)

    scientific_threshold <- c(min = -1e6, max = 1e6)
  }

  if(!is.null(evidential_boost)) {
    boost <- if(log) `+` else `*`
    x_df[[bf_colname]] <- boost(x_df[[bf_colname]], log(evidential_boost))
  }

  if(inverse) {
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
    , central_tendency = median
    , hdi = 0.95
    , iterations = iterations
    , standardized = standardized
  )
  
  x_canonized <- canonize(x_df, interval_type = "HDI")
  if(is.null(stat_name) && !is.null(subscript)) {
    if(escape_subscript) if(escape) {
      subscript <- escape_latex(subscript)
    }
    variable_label(x_canonized$statistic) <- 
    gsub("_\\{\\\\textrm\\{.+\\}\\}", paste0("_{\\\\textrm{", subscript, "}}"), variable_label(x_canonized$statistic))
  }

  mcmc_error_na <- all(is.na(x_canonized$mcmc.error))

  # error_label <- variable_label(x_canonized$mcmc.error)
  x_canonized$mcmc.error <- print_num(x_canonized$mcmc.error, na_string = "")
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
#' @keywords internal
#' @method apa_print BFBayesFactorTop
#' @export

apa_print.BFBayesFactorTop <- function(x, inverse = FALSE, ...) {
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
  ellipsis$inverse <- inverse
  if(is.null(ellipsis$subscript)) {
    ellipsis$subscript <- if(!inverse) "01" else "10"
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
# }


bf_add_estimates <- function(x, ...) UseMethod("bf_add_estimates", x@numerator[[1]])

bf_add_estimates.default <- function(x, data_frame, ...) data_frame

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
    est_name <- "mean.difference"
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


bf_sample_summarize <- function(
  x
  , data_frame
  , iterations = 10000
  , estimate = NULL
  , est_name = NULL
  , central_tendency = median
  , hdi = 0.95
  , ...
) {
  validate(iterations, check_class = "numeric", check_length = 1, check_range = c(0, Inf))
  validate(estimate, check_class = "character", check_length = 1)
  validate(hdi, check_class = "numeric", check_length = 1, check_range = c(0, 1))

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
  est_hdi <- hd_int(posterior_samples, level = hdi)

  data_frame[[est_name]] <- est_mean
  data_frame$hd.int <- list(est_hdi)

  data_frame
}


bf_add_names <- function(x, ...) UseMethod("bf_add_names", x@numerator[[1]])

bf_add_names.default <- function(x, data_frame, ...) data_frame

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
          print_interval(
            as.numeric(interval_bounds[[x]])
            , use_math = FALSE
          )
        } else {
          bounds <- as.numeric(interval_bounds[[x]])

          on_bound <- bounds %in% range
          if(any(on_bound)) {
            bounds <- sort(c(bounds[!on_bound], range[!on_bound]))
            print_interval(
              bounds
              , use_math = FALSE
            )
          } else {
            bounds <- list(c(range[1], bounds[1]), c(bounds[2], range[2]))
            inverse_interval <- sapply(bounds, print_interval, use_math = FALSE)
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
