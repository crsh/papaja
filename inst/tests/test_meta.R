library("testthat")
source("../../R/meta.R")

context("localize()")

test_that(
  "Localize document"
  , {
    lang <- localize("american")

    expect_that(lang, is_a("list"))
    expect_that(length(lang), equals(5))
    expect_that(names(lang), equals(c("abstract", "keywords", "table", "figure", "note")))
    expect_that(unlist(lapply(lang, class)), is_equivalent_to(rep("character", 5)))
  }
)
