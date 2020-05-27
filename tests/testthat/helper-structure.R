container_names <- c("estimate", "statistic", "full_result", "table")


# Test the general structure of apa_results

expect_apa_results <- function(
  object
  , col.names = NULL
  , labels = NULL
  , ...
) {

  # Capture object and label
  act <- list(
    value = object
    , label = deparse(substitute(object))
  )

  expect_s3_class(object, c("apa_results", "list"), exact = TRUE)
  expect_identical(names(object), container_names)

  # estimate ----
  if (!is.null(object$est)) {
    if(is.list(object$est)) {
      lapply(X = object$est, FUN = expect_type, "character")
    } else {
      expect_type(object$est, "character")
    }
  }

  # statistic ----
  if (!is.null(object$stat)) {
    if(is.list(object$stat)) {
      lapply(X = object$stat, FUN = expect_type, "character")
    } else {
      expect_type(object$stat, "character")
    }
  }

  # full_result ----
  if (!is.null(object$full)) {
    if(is.list(object$full)) {
      lapply(X = object$full, FUN = expect_type, "character")
    } else {
      expect_type(object$full, "character")
    }
  }

  # table ----
  if(!is.null(object$table)) { # allow NULL until we can add table everywhere
    table_class <- class(object$table)
    expect(
      identical(table_class, c("apa_results_table", "data.frame"))
      , sprintf(
        "The table element of `%s` has class `%s`, not `%s`."
        , act$lab, paste(table_class, collapse = "/"), "apa_results_table/data.frame"
      )
    )


    for (i in colnames(object$table)) {
      # All columns should be of class papaja_labelled/character
      actual_class <- class(object$table[[i]])
      expect(
        identical(actual_class, c("papaja_labelled", "character"))#
        , sprintf("Column `%s` in table element of `%s` has class `%s`, not `%s`.",
                  i, act$lab, paste(actual_class, collapse = "/"), "papaja_labelled/character")
      )
    }
    if(!is.null(col.names)) {
      expect_identical(colnames(object$table), col.names)
    }
    if(!is.null(labels)) {
      expect_identical(variable_labels(object$table), labels)
    }
  }
  # Invisibly return the value
  invisible(act$val)
}

test_that(
  "expect_apa_results"
  , {
    test <- papaja:::apa_print_container()
    test$table <- data.frame(a = 1)
    # class(test$table) <- c("apa_results_table", "data.frame")
    # expect_failure(
    #   expect_apa_results(test)
    #   , "has class"
    # )
  }
)
