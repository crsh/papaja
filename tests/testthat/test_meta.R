context("localize()")

test_that(
  "Localize document"
  , {
    lang <- papaja:::localize("english")

    expect_is(lang, "list")
    expect_equal(length(lang), 14)
    expect_equal(names(lang), c("author_note", "abstract", "keywords", "word_count", "table", "figure", "note", "correspondence", "email", "version", "and", "cite_r_packages_s", "cite_r_packages_pl", "cite_r_footnote"))
    expect_equivalent(sapply(lang, class), rep("character", 14))
  }
)
