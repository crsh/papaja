
#' @param test For MANOVA, the multivariate test statistic to be reported, see \code{\link[stats]{summary.manova}}.
#' @inheritParams glue_apa_results
#' @rdname apa_print.aov
#' @method apa_print manova
#' @export

apa_print.manova <- function(x, test = "Pillai", in_paren = FALSE, ...) {
  summ_x <- summary(x, test = test)
  apa_print(summ_x, in_paren = in_paren, ...)
}

#' @inheritParams glue_apa_results
#' @rdname apa_print.aov
#' @method apa_print summary.manova
#' @export

apa_print.summary.manova <- function(x, in_paren = FALSE, ...) {

  validate(in_paren, check_class = "logical", check_length = 1L)

  resid_row <- apply(X = x$stats, MARGIN = 1L, anyNA)

  df <- data.frame(
    x$stats[!resid_row, , drop = FALSE]
    , stringsAsFactors = FALSE
  )
  df$Effect <- rownames(df)
  # df$multivariate.df <- df$Df
  # df$multivariate.df.residual <- x$stats[resid_row, "Df"]
  df$Df <- NULL

  canonical_table <- canonize(df)
  beautiful_table <- beautify(canonical_table, ...)

  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , term_names = sanitize_terms(df$Effect)
    , in_paren = in_paren
  )
}
