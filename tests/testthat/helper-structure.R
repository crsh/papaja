container_names <- c("estimate", "statistic", "full_result", "table")


# Test the general structure of apa_results

expect_apa_results <- function(
  object
  , ...
) {

  # Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  expect_s3_class(object, c("apa_results", "list"), exact = TRUE)
  expect_identical(names(object), container_names)

  # estimate ----

  # statistic ----

  # full_result ----

  # table ----
  if(!is.null(object$table)) { # allow NULL until we can add table everywhere
    expect_s3_class(object$table, class = c("apa_results_table", "data.frame"), exact = TRUE)
    lapply(X = object$table, FUN = function(x){
      # All columns should be character and labelled
      expect_s3_class(!!x, class = c("papaja_labelled", "character"), exact = TRUE)
    })
  }
  # Invisibly return the value
  invisible(act$val)
}
