test_that(
  "Knit bare skeleton"
  , {
    skip_on_cran()

    # Render skeleton
    rmarkdown::draft(
      "~/bare_skeleton.Rmd"
      , system.file(
        "rmarkdown", "templates", "apa6"
        , package = "papaja")
      , create_dir = FALSE
      , edit = FALSE
    )

    rmarkdown::render("~/bare_skeleton.Rmd")
    expect_true(file.exists("bare_skeleton.pdf"))

    # Clean up
    file.remove(list.files(pattern = "bare"))
    file.remove("r-references.bib")
  }
)
