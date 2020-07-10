context("apa_print()")

test_that(
  "Default method"
  , {
    unknown_object <- list()
    class(unknown_object) <- "foobar"
    expect_error(apa_print(unknown_object), regexp = "github.com/crsh/papaja/issues ")
  }
)
