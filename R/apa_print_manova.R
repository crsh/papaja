
#' @param test For MANOVA, the multivariate test statistic to be reported, see \code{\link[stats]{summary.manova}}.
#' @inheritParams create_container
#' @rdname apa_print.aov
#' @method apa_print manova
#' @export

apa_print.manova <- function(x, test = "Pillai", in_paren = FALSE, ...) {
  summ_x <- summary(x, test = test)
  apa_print(summ_x, in_paren = in_paren, ...)
}

#' @inheritParams create_container
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
  # df$multivariate.df1 <- df$Df
  # df$multivariate.df2 <- x$stats[resid_row, "Df"]
  df$Df <- NULL

  canonical_table <- canonize(df)
  beautiful_table <- beautify(canonical_table, ...)

  create_container(
    beautiful_table
    , sanitized_terms = sanitize_terms(df$Effect)
    , in_paren = in_paren
  )
}
