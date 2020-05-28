container_names <- c("estimate", "statistic", "full_result", "table")

# Test the general structure of apa_results ----
# 1. class apa_results/list
# 2. names container_names
# 3. anyNA?
# 4. reporting strings either character or list or NULL
# 5. table class: apa_results_table/data.frame or NULL
# 6. table columns: each column of class papaja_labelled/character
# 7. Optional: Test specific col.names
# 8. Optional: Test variable labels (and col.names)


expect_apa_results <- function(
  object
  , col.names = NULL
  , labels = NULL
  , ...
) {

  # Recursive helper function ----
  expect_reporting_string <- function(object, ...) {
    if(is.null(object)) return(invisible(object))
    y <- rapply(list(object), f = expect_type, type = "character")
    invisible(object)
  }


  # Capture object and label ---------------------------------------------------
  act <- list(
    value = object
    , label = deparse(substitute(object))
  )

  expect_s3_class(object, c("apa_results", "list"), exact = TRUE)
  expect_identical(names(object), container_names)

  # Check for missing values ----
  expect(
    !anyNA(object, recursive = TRUE)
    , sprintf("The object `%s` contains missing values.", act$lab)
  )

  # estimate ----
  expect_reporting_string(object$est)

  # statistic ----
  expect_reporting_string(object$stat)

  # full_result ----
  expect_reporting_string(object$full)

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

# Test the test, work in progress ----
# expect_failure() somehow doesn't detect failure

test_that(
  "expect_apa_results"
  , {
    test <- papaja:::apa_print_container()
    test$estimate <- NA_character_
    # test$table <- data.frame(a = 1)
    # expect_failure(expect_apa_results(test))
    # class(test$table) <- c("apa_results_table", "data.frame")
    # expect_failure(
    #   expect_apa_results(test)
    #   , "has class"
    # )
  }
)
