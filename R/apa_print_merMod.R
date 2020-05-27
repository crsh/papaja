#' Format Statistics from (Generalized) Hierarchical Linear Models (APA 6th edition)
#'
#' These methods take objects from various R functions that calculate
#' hierarchical linear models to create formatted character strings
#' to report the results in accordance with APA manuscript guidelines.
#'
#' @param x A fitted hierarchical linear model, either from [lme4::lmer()],
#'   [lmerTest::lmer()], or [afex::mixed()].
#' @param args_confint Named list. Additional arguments that are passed to [lme4::confint.merMod()].
#' @inheritParams print_anova
#' @param ... Further arguments, currently ignored.
#' @family apa_print
#' @rdname apa_print.merMod
#' @method apa_print merMod
#' @export
#' @md

apa_print.merMod <- function(
  x
  , args_confint
  , in_paren = FALSE, ...
) {

  # Input validation and processing ----
  args <- list(...)

  args_confint <- defaults(
    if(missing(args_confint)) { list() } else { args_confint }
    , set = list(
      object = x
      , parm = "beta_"
    )
    , set.if.null = list(
      level = 0.95
      , method = if(methods::is(x, "glmerMod")){"boot"} else {"profile"}
      , nsim = 2e3
    )
  )


  # Rearrange ----
  x_summary <- summary(x)

  res_table <- as.data.frame(
    x_summary$coefficients
    , row.names = NULL
  )
  res_table$term <- rownames(x_summary$coefficients)
  rownames(res_table) <- NULL


  # Add confidence intervals ----
  confidence_intervals <-
    do.call("confint", args_confint)[rownames(x_summary$coefficients), ] # ensure same arrangement as in model object

  res_table$conf.int <- apply(X = confidence_intervals, MARGIN = 1, FUN = function(x) {
    as.list(x)
  })
  attr(res_table$conf.int, "conf.level") <- args_confint$level


  # sanitize, prettify, create container ----
  sanitized_table  <- sanitize_table(res_table, est_label = "$b$")
  prettified_table <- print_table(sanitized_table)

  create_container(
    prettified_table
    , sanitized_terms = sanitize_terms(sanitized_table$term)
    , add_par  = NULL
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
