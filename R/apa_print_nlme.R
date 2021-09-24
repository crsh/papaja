#' Typeset Statistical Results from Nonlinear Hierarchical Models
#'
#' These methods take mixed-effects models fitted with the \pkg{nlme} package
#' and create formatted character strings report the results in accordance
#' with APA manuscript guidelines.
#'
#' @param x A (non-)linear mixed-effects model fitted with [nlme::lme()] or
#'   [nlme::nlme()]
#' @param args_confint Named list. Arguments that are passed to
#'   [nlme::intervals.lme()].
#' @inheritParams apa_print.merMod
#'
#' @evalRd apa_results_return_value()
#'
#' @examples
#'   library(nlme)
#'   fm1 <- lme(distance ~ age, data = Orthodont, method = "ML") # random is ~ age
#'   apa_print(fm1)
#'   # ANOVA-like tables
#'   single_anova <- anova(fm1)
#'   apa_print(single_anova)
#'
#' @family apa_print
#' @rdname apa_print.lme
#' @method apa_print lme
#' @export

apa_print.lme <- function(
  x
  , args_confint = NULL
  , in_paren = FALSE
  , est_name = NULL
  , ...
) {

  # Input validation ----
  if(is.null(args_confint)) args_confint <- list()
  validate(args_confint, check_class = "list")


  # `in_paren` is validated in `glue_apa_results()`

  if(is.null(est_name)) {
    est_name <- "$\\hat{\\beta}$"
  } else {
    validate(est_name, check_class = "character", check_length = 1L)
    est_name <- paste0("$", strip_math_tags(est_name), "$")
  }

  # Preprocess ----
  x_summary <- summary(x)


  res_table <- as.data.frame(
    x_summary$tTable
    , stringsAsFactors = FALSE
    , make.names = TRUE
  )

  args_confint <- defaults(
    args_confint
    , set = list(
      object = x
      , which = "fixed"
    )
    , set.if.null = list(
      level = .95
    )
  )

  # Add confidence intervals ----
  confidence_intervals <-
    do.call(nlme::intervals, args_confint)

  res_table$conf.int <- unlist(
    apply(X = confidence_intervals$fixed, MARGIN = 1, FUN = function(x) {
      list(unname(x[c("lower", "upper")]))
    })
    , recursive = FALSE
  )

  attr(res_table$conf.int, "conf.level") <- args_confint$level

  res_table$Term <- rownames(res_table)
  rownames(res_table) <- NULL

  res_table$estimate <- res_table$Value # "Value" could be any column in other objects
  res_table$Value <- NULL

  # Canonize, beautify, and glue ----
  canonical_table <- canonize(res_table, est_label = est_name)
  beautiful_table <- beautify(canonical_table, ...)

  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , term_names = sanitize_terms(res_table$Term)
    , in_paren = in_paren
    , simplify = FALSE
  )
}

#' @rdname apa_print.lme
#' @method apa_print anova.lme
#' @export

apa_print.anova.lme <- function(
  x
  , in_paren = FALSE
  , ...
) {

  if(anyNA(x)) stop("Model-comparison tables of class 'anova.lme' are not supported.")

  res_table <- as.data.frame(
    x
    , stringsAsFactors = FALSE
    , make.names = TRUE
  )

  res_table$Term <- rownames(res_table)
  rownames(res_table) <- NULL

  # Canonize, beautify, and glue ----
  canonical_table <- canonize(res_table)
  beautiful_table <- beautify(canonical_table, ...)

  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , term_names = sanitize_terms(res_table$Term)
    , in_paren = in_paren
    , simplify = FALSE
  )

}
