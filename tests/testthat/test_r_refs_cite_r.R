context("r_refs() and cite_r()")

test_that(
  "r_refs() writes out all references"
  , {
    skip_on_cran()

    library("papaja")
    library("brms")

    r_refs("./test_cite_r_all.bib", type_pref = "not-available", tweak = TRUE, append = FALSE)
    r_refs("./test_cite_r_preferred.bib", type_pref = "Article", tweak = TRUE, append = FALSE)
    r_refs("./test_cite_r_preferred.bib", type_pref = c("Book", "Article"), tweak = TRUE, append = FALSE)
  }
)
