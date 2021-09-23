apa_resutls_return_value <- function() {

  prefix <- "\\code{apa_print()}-methods return a named list of class \\code{apa_results} containing the following elements:\n"

  list_elements <- list(
    estimate = "One or more character strings giving point estimates, confidence intervals, and confidence level. A single string is returned in a vector; multiple strings are returned as a named list. If no estimate is available the element is \\code{NULL}."
    , statistic = "One or more character strings giving the test statistic, parameters (e.g., degrees of freedom), and p-value. A single string is returned in a vector; multiple strings are returned as a named list. If no estimate is available the element is \\code{NULL}."
    , full_result = "One or more character strings comprised `estimate` and `statistic`. A single string is returned in a vector; multiple strings are returned as a named list. "
    , table = "A \\code{data.frame} of class \\code{apa_results_table} that contains all elements of \\code{estimate} and \\code{statistics}. This table can be passed to \\code{apa_table()} for reporting."
  )

  list_elements <- paste0("\\item{", names(list_elements), "}{", list_elements, "}")

  post_fix <- "\nColumn names in \\code{apa_results_table} are standardized following the \\href{https://www.tidymodels.org/learn/develop/broom/#glossary}{\\pkg{broom} glossary} (e.g., \\code{term}, \\code{estimate} \\code{conf.int}, \\code{statistic}, \\code{df}, \\code{df.residual}, \\code{p.value}). Additionally, each column is labelled (e.g., \\code{$\\\\hat{\\\\eta}^2_G$} or \\code{$t(23)$}) using the \\pkg{tinylabels} package and these labels are used as column names when an \\code{apa_results_table} is passed to \\code{apa_table()}.\n"

  paste(
    "\\value{"
    , prefix
    , paste(list_elements, collapse = "\n")
    , post_fix
    , "}"
    , sep = "\n"
    )
}
