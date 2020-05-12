
#' @param test For MANOVA, the multivariate test statistic to be reported, see \code{\link{summary.manova}}.
#' @rdname apa_print.aov
#' @method apa_print manova
#' @export

apa_print.manova <- function(x, test = "Pillai", ...) {

  tidied_x <- as.data.frame(
    x = broom::tidy(x, test = test, ...)
    , stringsAsFactors = FALSE
  )
  print_manova(x = tidied_x, ...)
}


# #' @rdname apa_print.aov
# #' @method apa_print summary.manova
# #' @export
#
# apa_print.summary.manova <- function(x, ...) {
#
#   tidied_x <- as.data.frame(
#     x = broom::tidy(x, ...)
#     , stringsAsFactors = FALSE
#   )
#   print_manova(x = tidied_x, ...)
# }



#' @keywords internal

print_manova <- function(
  x
  , in_paren = FALSE
) {



  estimate_names <- c(
    pillai = "V"
    , wilks = "\\Lambda"
    , hl = "T"
    , roy = "\\theta"
  )


  estimate_names_with_dollars <- paste0("$", estimate_names, "$")
  names(estimate_names_with_dollars) <- names(estimate_names)

  multivariate_stat <- intersect(names(estimate_names), colnames(x))

  if(length(multivariate_stat) == 0L) {
    stop("No multivariate test statistic found in results object.")
  } # move this to input validation

  col_names <- c(
    term = "Effect"
    , df = "df"
    , pillai = "pillai"
    , wilks = "wilks"
    , hl = "hl"
    , roy = "roy"
    , statistic = "F"
    , num.df ="df1"
    , den.df = "df2"
    , p.value ="p"
  )
  colnames(x) <- col_names[colnames(x)]

  var_labels <- c(
    estimate_names_with_dollars
    , "Effect" = "Effect"
    , "F" = "$F$"
    , "df1" = "$\\mathit{df}_1$"
    , "df2" = "$\\mathit{df}_2$"
    , "p" = "$p$"
  )

  x <- x[x$Effect != "Residuals", colnames(x) != "df", drop = FALSE]

  sanitized_terms <- sanitize_terms(x$Effect)
  x[["Effect"]] <- prettify_terms(x[["Effect"]])
  x[[multivariate_stat]] <- printnum(x[[multivariate_stat]])
  x[["F"]] <- printnum(x[["F"]])
  x[["df1"]] <- print_df(x[["df1"]])
  x[["df2"]] <- print_df(x[["df2"]])
  x[["p"]] <- printp(x[["p"]])

  x <- sort_terms(x, "Effect")

  variable_labels(x) <- var_labels[colnames(x)]

  # assemble final output object
  res <- apa_print_container()

  res$statistic <- as.list(
    paste0(
      "$", estimate_names[[multivariate_stat]], " = ", x[[multivariate_stat]], "$, "
      , "$F(", x[["df1"]], ", ", x[["df2"]], ") = ", x[["F"]], "$, "
      , "$p ", add_equals(x[["p"]]), "$"
    )
  )
  if(in_paren) res$statistic <- in_paren(res$statistic)
  names(res$statistic) <- sanitized_terms

  res$full_result <- res$statistic

  res$table <- x
  class(res$table) <- c("apa_results_table", "data.frame")
  res
}




