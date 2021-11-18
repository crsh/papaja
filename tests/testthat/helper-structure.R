container_names <- c("estimate", "statistic", "full_result", "table")

# Some defaults changed w/ afex 1.0.0, here we restore the behaviour of version 0.28.x
if(packageVersion("afex") >= '1.0.0') {
  afex::afex_options(
    emmeans_model = "univariate"
    , include_aov = TRUE
  )
}

# Test the general structure of apa_results ----
# 1. class apa_results/list
# 2. names container_names
# 3. anyNA?
# 4. reporting strings either character or list or NULL
# 5. table class: apa_results_table/data.frame or NULL
# 6. table columns: each column of class tiny_labelled/character
# 7. Optional: Test specific col.names
# 8. Optional: Test variable labels (and col.names)
# 9. Optional: Test names of terms in results


expect_apa_results <- function(
  object
  , col.names = NULL
  , labels = NULL
  , term_names = NULL
  , table_terms = TRUE
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
  expect_reporting_string(object$estimate)

  # statistic ----
  expect_reporting_string(object$statistic)

  # full_result ----
  expect_reporting_string(object$full_result)

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
      # All columns should be of class tiny_labelled/character
      actual_class <- class(object$table[[i]])
      expect(
        identical(actual_class, c("tiny_labelled", "character"))#
        , sprintf("Column `%s` in table element of `%s` has class `%s`, not `%s`.",
                  i, act$lab, paste(actual_class, collapse = "/"), "tiny_labelled/character")
      )
    }
    if(!is.null(col.names)) {
      expect_identical(colnames(object$table), col.names)
    }
    if(!is.null(labels)) {
      expect_identical(variable_labels(object$table), labels)
    }
    if(!is.null(term_names)) {
      if(!is.null(object$estimate)) {
        expect_identical(names(object$estimate), term_names)
      }
      if(!is.null(object$statistic)) {
        expect_identical(names(object$statistic), term_names)
      }
      expect_identical(names(object$full_result), term_names)

      term_names <- term_names[term_names != "modelfit"]
      expect_identical(nrow(object$table), length(term_names))
    }
  }

  # consistency between ordering of names of reporting strings and table
  if(!is.null(object$table$term)) {
    if(isTRUE(table_terms)) {
      expect_equal(
        tolower(sanitize_terms(unlabel(gsub(object$table$term, pattern = " $\\times$ ", replacement = "_", fixed = TRUE))))
        , tolower(names(object$full_result)[!names(object$full_result) == "modelfit"])
      )
    } else {
      expect_equivalent(unclass(object$table$term)[1:nrow(object$table)], table_terms)
    }
  }

  # Invisibly return the value
  invisible(act$val)
}


expect_apa_term <- function(object, term, estimate = NULL, statistic = NULL) {
  act <- list(
    value = object
    , label = deparse(substitute(object))
  )

  full_result <- paste(c(estimate, statistic), collapse = ", ")

  expect_identical(object$estimate[[term]], estimate)
  expect_identical(object$statistic[[term]], statistic)
  expect_identical(object$full_result[[term]], full_result)

  # Invisibly return the value
  invisible(act$val)
}

# Test the test, work in progress ----
# expect_failure() somehow doesn't detect failure

# test_that(
#   "expect_apa_results"
#   , {
#     test <- papaja:::init_apa_results()
#     test$table <- data.frame(a = 1)
#
#     class(test$table) <- c("apa_results_table", "data.frame")
#     expect_failure(
#       expect_apa_results(test)
#       , "has class"
#     )
#   }
# )
