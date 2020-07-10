test_that(
  "Knit bare skeleton"
  , {
    skip_on_cran()

    # Render skeleton
    rmarkdown::draft(
      "bare_skeleton.Rmd"
      , system.file(
        "rmarkdown", "templates", "apa6"
        , package = "papaja")
      , create_dir = FALSE
      , edit = FALSE
    )

    try(
      rmarkdown::render(
        "bare_skeleton.Rmd"
        , output_format = "papaja::apa6_pdf"
        , quiet = TRUE
      )
    )
    expect_true(file.exists("bare_skeleton.pdf"))

    try(
      rmarkdown::render(
        "bare_skeleton.Rmd"
        , output_format = "papaja::apa6_docx"
        , quiet = TRUE
      )
    )
    expect_true(file.exists("bare_skeleton.docx"))

    # Clean up
    file.remove(list.files(pattern = "bare_skeleton"))
    file.remove("r-references.bib")
  }
)
