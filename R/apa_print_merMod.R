#' Format Statistics from Hierarchical (Generalized) Linear Models (APA 6th edition)
#'
#' These methods take objects from various R functions that calculate
#' hierarchical (generalized) linear models to create formatted character strings
#' to report the results in accordance with APA manuscript guidelines.
#'
#' @param x A fitted hierarchical linear model, either from [lme4::lmer()],
#'   [lmerTest::lmer()], or [afex::mixed()].
#' @param args_confint Named list. Additional arguments that are passed to [lme4::confint.merMod()], see details.
#' @param est_name     An optional character. The label to be used for fixed-effects coefficients.
#' @inheritParams print_anova
#' @param ... Further arguments that are passed to [printnum()].
#' @details
#'   Confidence intervals are calculated by calling [lme4::confint.merMod()]. For linear models,
#'   profile confidence intervals
#'
#' @family apa_print
#' @rdname apa_print.merMod
#' @method apa_print merMod
#' @export
#' @md

apa_print.merMod <- function(
  x
  , args_confint
  , in_paren = FALSE
  , est_name
  , ...
) {

  # Input validation and processing ----
  args <- list(...)

  if(!missing(args_confint) && !is.null(args_confint)) {
    validate(args_confint, check_class = "list")
  } else {
    args_confint <- list()
  }

  # `in_paren` is validated in `glue_apa_results()`

  if(missing(est_name) || is.null(est_name)) {
    est_name <- "$\\hat{\\beta}$"
  } else {
    validate(est_name, check_class = "character", check_length = 1L)
    est_name <- paste0("$", strip_math_tags(est_name), "$")
  }

  # GLMM with non-fixed scale? (cf. lme4::profile.merMod)
  no_profile <- lme4::isGLMM(x) && x@devcomp$dims[["useSc"]]

  args_confint <- defaults(
    args_confint
    , set = list(
      object = x
      , parm = "beta_"
    )
    , set.if.null = list(
      level = .95
      , method = if(no_profile){"boot"} else {"profile"}
      , nsim = 2e3L
    )
  )




  # Rearrange ----
  x_summary <- summary(x)

  res_table <- as.data.frame(
    x_summary$coefficients
    , row.names = NULL
  )

  res_table$Term <- rownames(x_summary$coefficients)
  rownames(res_table) <- NULL


  # Add confidence intervals ----
  confidence_intervals <-
    do.call("confint", args_confint)[rownames(x_summary$coefficients), ] # ensure same arrangement as in model object

  res_table$conf.int <- apply(X = confidence_intervals, MARGIN = 1, FUN = function(x) {
    as.list(x)
  })
  attr(res_table$conf.int, "conf.level") <- args_confint$level


  # canonize, beautify, glue ----
  canonical_table <- canonize(res_table, est_label = est_name)
  beautiful_table <- beautify(canonical_table, ...)

  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , term_names = sanitize_terms(res_table$Term)
    , in_paren = in_paren
  )
}

#' @rdname apa_print.merMod
#' @method apa_print mixed
#' @export

apa_print.mixed <- function(x, ...) {

  anova_table <- x$anova_table
  attr(anova_table, "method") <- attr(x, "method")
  apa_print(anova_table, ...)
}

