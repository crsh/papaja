context("localize()")

test_that(
  "Localize document"
  , {
    lang <- papaja:::localize("american")

    expect_that(lang, is_a("list"))
    expect_that(length(lang), equals(6))
    expect_that(names(lang), equals(c("author_note", "abstract", "word_count", "table", "figure", "note")))
    expect_that(unlist(lapply(lang, class)), is_equivalent_to(rep("character", 6)))
  }
)
