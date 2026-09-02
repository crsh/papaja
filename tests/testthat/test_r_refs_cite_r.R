context("r_refs() and cite_r()")

test_that(
  "r_refs() writes out all references"
  , {
    skip_on_cran()

    library("papaja")
    library("rmarkdown")

    cite_r_all  <- tempfile(fileext = ".bib")
    r_refs(cite_r_all, type_pref = "not-available", tweak = TRUE, append = FALSE)
    cite_r_preferred  <- tempfile(fileext = ".bib")
    r_refs(cite_r_preferred, type_pref = "Article", tweak = TRUE, append = FALSE)
    r_refs(cite_r_preferred, type_pref = c("Book", "Article"), tweak = TRUE, append = FALSE)
  }
)
