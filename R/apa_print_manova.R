
#' @param test For MANOVA, the multivariate test statistic to be reported, see \code{\link{summary.manova}}.
#' @rdname apa_print.aov
#' @method apa_print manova
#' @export

apa_print.manova <- function(x, test = "Pillai", ...) {

  ellipsis <- list(...)
  ellipsis$x <- summary(x, test = test)
  # tidied_x <- as.data.frame(
  #   x = broom::tidy(x, test = test, ...)
  #   , stringsAsFactors = FALSE
  # )
  # print_manova(x = tidied_x, ...)
  do.call("apa_print", ellipsis)
}


#' @rdname apa_print.aov
#' @method apa_print summary.manova
#' @export

apa_print.summary.manova <- function(x, ...) {

  ellipsis <- defaults(
    list(...)
    , set.if.null = list(
      in_paren = FALSE
    )
  )

  resid_row <- apply(X = x$stats, MARGIN = 1L, anyNA)

  df <- data.frame(
    x$stats[!resid_row, , drop = FALSE]
    , stringsAsFactors = FALSE
  )
  df$Effect <- rownames(df)
  # df$multivariate.df1 <- df$Df
  # df$multivariate.df2 <- x$stats[resid_row, "Df"]
  df$Df <- NULL

  sanitized <- sanitize_table(df)
  prettified <- print_table(sanitized)
  create_container(
    prettified
    , sanitized_terms = sanitize_terms(df$Effect)
    , in_paren = ellipsis$in_paren
  )
}
