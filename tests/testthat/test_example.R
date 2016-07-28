context("Render example document")

test_that(
  ""
  , {
    rmarkdown::render("../../example/example.Rmd")
  }
)
