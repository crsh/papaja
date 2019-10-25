
#' @export

apa_print.merMod <- function(x, ...) {

  if(package_available("lmerTest")) {
    x <- lmerTest::as_lmerModLmerTest(x)
  }
print(class(x))
  apa_print(summary(x, ...))
}


#' @export

apa_print.summary.merMod <- function(
  x
  , ...
) {
  lmerTest <- methods::is(x, "summary.lmerModLmerTest")

  term <- prettify_terms(
    rownames(x$coefficients)
  )

  res_table <- as.data.frame(
    x$coefficients
    , row.names = NULL
  )
  rownames(res_table) <- NULL

  renamers <- c(
    "Estimate" = "estimate"
    , "Std. Error" = "std.err"
    , "t value" = "statistic"
    , df = "df"
    , "Pr(>|t|)" = "p.value"
  )
  colnames(res_table) <- renamers[colnames(res_table)]

  res_table <- cbind(term, res_table)
  res_table$estimate <- printnum(res_table$estimate)
  res_table$std.err <- printnum(res_table$std.err)
  res_table$statistic <- printnum(res_table$statistic)

  if(lmerTest) {
    res_table$df <- print_df(res_table$df)
    res_table$p.value <- printp(res_table$p.value)
    res_table <- res_table[, c("term", "estimate", "std.err", "statistic", "df", "p.value")]
  }

  variable_labels(res_table) <- c(
    term = "Term"
    , estimate = "$b$"
    , std.err = "$\\mathit{SE}$"
    , statistic = "$t$"
    , df = "$\\mathit{df}$"
    , p.value = "$p$"
  )[colnames(res_table)]
  class(res_table) <- c("apa_results_table", "data.frame")

  res <- apa_print_container()
  res$table <- res_table

  res$estimate <- as.list(paste0("$b = ", res_table$estimate, "$"))
  names(res$estimate) <- sanitize_terms(res_table$term)

  if(lmerTest) {
    res$statistic <- as.list(paste0("$t(", res_table$df, ") = ", res_table$statistic, "$, $p = ", res_table$p.value, "$"))
  } else {
    res$statistic <- as.list(paste0("$t = ", res_table$statistic, "$"))
  }
  names(res$statistic) <- names(res$estimate)

  res$full_result <- as.list(paste0(res$estimate, ", ", res$statistic))
  names(res$full_result) <- names(res$estimate)

  # return
  res
}
