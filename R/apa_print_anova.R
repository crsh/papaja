#' Typeset Statistical Results from ANOVA
#'
#' These methods take objects from various R functions that calculate ANOVA to
#' create formatted character strings to report the results in accordance with
#' APA manuscript guidelines. For `anova`-objects from model comparisons see
#' \code{\link{apa_print.list}}.
#'
#' @param x An object containing the results from an analysis of variance ANOVA
#' @param correction Character. For repeated-measures ANOVA, the type of
#'   sphericity correction to be used. Possible values are `"GG"` for the
#'   Greenhouse-Geisser method (the default), `"HF"` for the Huyn-Feldt method,
#'   or `"none"` for no correction.
#' @param intercept Logical. Indicates if the intercept term should be included
#'   in output.
#' @param estimate Character, function, or data frame. Determines which
#'   estimate of effect size is to be used. See details.
#' @param mse Logical. Indicates if mean squared errors should be included in
#'   output. The default is `TRUE`, but this can be changed either by supplying
#'   a different value in the function call or by changing the global default
#'   via `options(papaja.mse = FALSE)`.
#' @param observed Character. The names of the factors that are observed,
#'   i.e., not manipulated. Necessary only for calculating *generalized* eta
#'   squared; otherwise ignored. If `x` is of class `afex_aov`, `observed` is
#'   automatically deduced from `x`.
#' @inheritParams beautify
#' @details
#'   The factor names are sanitized to facilitate their use as list names (see
#'   Value section). Parentheses are omitted and other non-word characters are
#'   replaced by `_`.
#'
#'   Argument `estimate` determines which measure of effect size is to be used:
#'   It is currently possible to provide one of three characters to specify the
#'   to-be-calculated effect size: \code{"ges"} for generalized \eqn{eta^2},
#'   \code{"pes"} for partial \eqn{eta^2}, and \code{"es"} for \eqn{eta^2}.
#'   Note that \eqn{eta^2} is calculated correctly if and only if the design is
#'   balanced.
#'
#'   It is also possible to provide a `data.frame` with columns `estimate`,
#'   `conf.low`, and `conf.high`, which allows for including custom effect-
#'   size measures.
#'
#'   A third option is to provide a function from the \pkg{effectsize} package
#'   that will be used to calculate effect-size measures from `x`. If
#'   \pkg{effectsize} is installed (and \pkg{papaja} is loaded), this is the
#'   new default. This default can be changed via
#'   `options(papaja.estimate_anova = ...)`.
#'
#' @evalRd apa_results_return_value()
#'
#' @references
#'    Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. \emph{Behavior Research Methods}
#'    , 37 (3), 379--384. doi:\href{https://doi.org/10.3758/BF03192707}{10.3758/BF03192707}
#'
#' @family apa_print
#' @seealso [aov()], [car::Anova()], [apa_print.list()]
#' @examples
#'    ## From Venables and Ripley (2002) p. 165.
#'    npk_aov <- aov(yield ~ block + N * P * K, npk)
#'    apa_print(npk_aov)
#'
#'    # Use the effectsize package to calculate partial eta-squared with
#'    # confidence intervals
#'    apa_print(npk_aov, estimate = effectsize::omega_squared)
#' @method apa_print aov
#' @export

apa_print.aov <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  , ...
) {
  apa_print(
    summary(x)
    , .x = x
    , intercept = intercept
    , estimate = estimate
    , mse = mse
    , observed = observed
    , in_paren = in_paren
    , ...
  ) # apa_print.anova
}


#' @rdname apa_print.aov
#' @method apa_print summary.aov
#' @export

apa_print.summary.aov <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  , ...
) {
  apa_print(
    x[[1]]
    , intercept = intercept
    , estimate = estimate
    , mse = mse
    , observed = observed
    , in_paren = in_paren
    , ...
  ) # apa_print.anova
}


#' @rdname apa_print.aov
#' @method apa_print aovlist
#' @export

apa_print.aovlist <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  , ...
) {
  apa_print(
    summary(x)
    , .x = x
    , estimate = estimate
    , mse = mse
    , observed = observed
    , intercept = intercept
    , in_paren = in_paren
    , ...
  ) # apa_print.summary.aovlist
}


#' @rdname apa_print.aov
#' @method apa_print summary.aovlist
#' @export

apa_print.summary.aovlist <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  , ...
) {

  ellipsis <- list(...)
  .x <- ellipsis$.x
  ellipsis$.x <- NULL

  intercept <- isTRUE(intercept)


  canonical_table <- arrange_anova(x)

  tinylabels::variable_labels(canonical_table) <- c(
    term = "Effect"
    , df = "$\\mathit{df}$"
    , statistic = "$F$"
    , p.value = "$p$"
    , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
  )

  canonical_table <- add_custom_effect_sizes(
    canonical_table = canonical_table
    , .x = .x
    , estimate = estimate
    , mse = mse
    , observed = observed
    , intercept = intercept
  )

  if(!intercept) canonical_table <- canonical_table[canonical_table$term != "(Intercept)", , drop = FALSE]


  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      digits = 3L
      , gt1 = FALSE
    )
  )
  ellipsis$x <- canonical_table
  beautiful_table <- do.call("beautify", ellipsis)

  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , term_names = sanitize_terms(unlabel(canonical_table$term))
    , in_paren = in_paren
    , est_first = FALSE
    , simplify = FALSE
  )
}


#' @rdname apa_print.aov
#' @method apa_print Anova.mlm
#' @export

apa_print.Anova.mlm <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , correction = getOption("papaja.sphericity_correction")
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  , ...
) {

  if(correction != "none") {
    summary_x <- summary(x, multivariate = FALSE) # car:::summary.Anova.mlm
  } else { # Corrections are always calculated and can throw warnings; hope I don't regret this
    summary_x <- suppressWarnings(summary(x, multivariate = FALSE)) # car:::summary.Anova.mlm
  }

  apa_print(
    summary_x
    , .x = x
    , correction = correction
    , intercept = intercept
    , estimate = estimate
    , mse = mse
    , observed = observed
    , in_paren = in_paren
  )
}


#' @rdname apa_print.aov
#' @method apa_print summary.Anova.mlm
#' @export

apa_print.summary.Anova.mlm <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , correction = getOption("papaja.sphericity_correction")
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  , ...
) {

  intercept <- isTRUE(intercept)
  in_paren  <- isTRUE(in_paren)

  arranged_table <- arrange_anova(x, correction) # arrange_anova.summary.Anova.mlm

  ellipsis <- list(...)
  .x <- ellipsis$.x
  ellipsis$.x <- NULL

  canonical_table <- canonize(arranged_table)
  canonical_table <- add_custom_effect_sizes(
    canonical_table
    , estimate = estimate
    , mse = mse
    , observed = observed
    , intercept = intercept
    , .x = .x
  )

  # Remove intercept if the user doesn't want it:
  if(!intercept) canonical_table <- canonical_table[canonical_table$term != "(Intercept)", , drop = FALSE]

  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      digits = 3L
      , gt1 = FALSE
    )
  )
  ellipsis$x <- canonical_table
  beautiful_table <- do.call("beautify", ellipsis)


  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , term_names = sanitize_terms(unlabel(canonical_table$term))
    , in_paren = in_paren
    , est_first = FALSE
    , simplify = FALSE
  )
}




#' @rdname apa_print.aov
#' @method apa_print afex_aov
#' @export

apa_print.afex_aov <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , correction = getOption("papaja.sphericity_correction")
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  , ...
) {
  validate(intercept, check_class = "logical", check_length = 1)

  afex_aov_intercept <- "(Intercept)" %in% rownames(x$anova_table)
  if(afex_aov_intercept != intercept & afex_aov_intercept) {
    warning("In your call of afex::aov_car() you requested the intercept term, but now you did not (in apa_print 'intercept = FALSE' is the default). Thus, the intercept term will be omitted; make sure this is what you want.")
  }

  ellipsis <- list(...)
  ellipsis$estimate <- estimate
  ellipsis$intercept <- intercept
  ellipsis$mse <- mse
  ellipsis$in_paren <- in_paren

  if(is.null(observed)) {
    # If 'observed' is not specified, obtain information from afex_aov object
    observed <- attr(x$anova_table, "observed")
  } else if(!setequal(observed, attr(x$anova_table, "observed"))){
    # If it is specified, warn if the sets of observed factors are unequal
    print_terms <- function(x) { # This helper function could be moved to utils.R
      n_terms <- length(x)
      y <- encodeString(x, quote = "\"")
      del <- rep(", ", length.out = n_terms)
      del[n_terms] <- " "
      if(n_terms == 2L) del[n_terms - 1L] <- " and "
      if(n_terms > 2L) del[n_terms - 1L] <- ", and "
      paste0(y, del)
    }

    warning(
      "In your call to apa_print(), you specified "
      , if(length(observed) == 0L) "no " else {"the "}
      , "model terms "
      , print_terms(observed)
      , "as observed, "
      , "whereas in your call to afex::aov_car(), you specified "
      , if(length(attr(x$anova_table, "observed")) == 0L) "no " else "the "
      , "model terms "
      , print_terms(attr(x$anova_table, "observed"))
      , "as observed. Make sure that this is what you want."
    )
  }

  ellipsis$observed <- observed


  if(inherits(x$Anova, "Anova.mlm")) {
    ellipsis$x <- summary(x$Anova)
    ellipsis$.x = x
    ellipsis$correction <- correction
  } else {
    ellipsis$x <- x$Anova
  }
  do.call("apa_print", ellipsis)
}


#' @rdname apa_print.aov
#' @method apa_print anova
#' @export

apa_print.anova <- function(
  x
  , estimate = getOption("papaja.estimate_anova", "ges")
  , observed = NULL
  , intercept = FALSE
  , mse = TRUE
  , in_paren = FALSE
  # , ci = 0.95
  , ...
) {
  # if(!is.null(ci)) validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  # Add method for levene test
  ellipsis <- list(...)

  .x <- ellipsis$.x
  ellipsis$.x <- NULL
  if(is.function(estimate) && is.null(.x)) .x <- x

  intercept <- isTRUE(intercept)

  object_heading <- attr(x, "heading")

  if (any(object_heading == "Models:")) {
    # Model comparisons from lmerTest::anova
    stop("Model-comparison objects of class 'anova' are not supported.")
  }

  # car::LeveneTest ----------------------------------------------------------
  if(length(object_heading) == 1 && grepl("Levene", object_heading)) {
    # if(!is.null(estimate)) stop("Effect sizes are not available for car::LeveneTest-objects.")

    y <- canonize(x)
    y <- remove_residuals_row(y)
    y <- beautify(y, ...)
    return(
      glue_apa_results(
        y
        , est_glue = construct_glue(y, "estimate")
        , stat_glue = construct_glue(y, "statistic")
        , in_paren = in_paren
        , simplify = TRUE
      )
    )
  } else if(any(grepl("Satterthwaite|Kenward", object_heading))) {
    # lmerTest::anova.merModLmerTest -------------------------------------------

    # determine correction type
    sub_heading <- object_heading[grepl("Satterthwaite|Kenward", object_heading)][[1]]
    attr(x, "df_correction") <- c("KR", "S")[c(grepl("Kenward", sub_heading), grepl("Satterth", sub_heading))]

    x$Effect <- rownames(x)

    # Canonize, beautify, and glue container
    canonical_table <- canonize(x)
    beautiful_table <- beautify(canonical_table, ...)

    return(
      glue_apa_results(
        beautiful_table
        , est_glue = construct_glue(beautiful_table, "estimate")
        , stat_glue = construct_glue(beautiful_table, "statistic")
        , term_names = sanitize_terms(x$Effect)
        , in_paren = in_paren
        , simplify = FALSE
      )
    )
  } else if(any(grepl("Mixed Model", object_heading))) {
    # afex::mixed --------------------------------------------------------------
    df_correction <- unname(
      c(KR = "KR", S = "S", PB = "none", LRT = "none")[attr(x, "method")]
    )
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    x$Effect <- rownames(x)

    # anova_table from mixed(method = "PB") contains
    # - two columns with *p* values,
    # - but also df from asymptotic theory.
    # To avoid ambiguity, we remove asymptotic p value and df:
    col_names <- colnames(x)
    if (any("Pr(>PB)" == col_names)) {
      x$`Chi Df` <- NULL
      x$`Pr(>Chisq)` <- NULL
    }
    # anova_table from mixed(method = "LRT") contains
    # - df of chi-squared test (column 'Chi Df') and
    # - df of "the model" (column 'Df').
    # To avoid ambiguity, we remove df of "the model":
    if(any("Chi Df" == col_names)) {
      x$Df <- NULL
    }

    attr(x, "df_correction") <- df_correction

    # Canonize, beautify, glue ----
    canonical_table <- canonize(x)
    beautiful_table <- beautify(canonical_table, ...)

    return(
      glue_apa_results(
        beautiful_table
        , est_glue = construct_glue(beautiful_table, "estimate")
        , stat_glue = construct_glue(beautiful_table, "statistic")
        , term_names = sanitize_terms(tinylabels::unlabel(x$Effect))
        , in_paren = in_paren
        , simplify = FALSE
      )
    )
    # lmerTest::ranova ---------------------------------------------------------
  } else if(identical(object_heading[1], "ANOVA-like table for random-effects: Single term deletions")) {
    stop("Single-term deletions are not supported, yet.\nVisit https://github.com/crsh/papaja/issues to request support.")
  }
  # anova::lm (single model) ----
  # Canonize, beautify, glue ----
  y <- as.data.frame(x, stringsAsFactors = FALSE)
  y$Effect <- trimws(rownames(y))

  canonical_table <- canonize(y)
  canonical_table <- remove_residuals_row(canonical_table)

  canonical_table <- add_custom_effect_sizes(
    estimate = estimate
    , canonical_table = canonical_table
    , mse = mse
    , observed = observed
    , intercept = intercept
    , .x = .x
  )

  if(!intercept) canonical_table <- canonical_table[canonical_table$term != "(Intercept)", , drop = FALSE]


  ellipsis <- defaults(
    ellipsis
    , set.if.null = list(
      digits = 3L
      , gt1 = FALSE
    )
  )
  ellipsis$x <- canonical_table

  beautiful_table <- do.call("beautify", ellipsis)

  return(
    glue_apa_results(
      beautiful_table
      , est_glue = construct_glue(beautiful_table, "estimate")
      , stat_glue = construct_glue(beautiful_table, "statistic")
      , term_names = sanitize_terms(tinylabels::unlabel(canonical_table$term))
      , in_paren = in_paren
      , est_first = FALSE
      , simplify = FALSE
    )
  )
}
