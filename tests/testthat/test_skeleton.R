test_that(
  "Knit bare skeleton"
  , {
    skip_on_cran()

    # Render skeleton
    bare_path <- file.path(tempdir(), "bare_skeleton.Rmd")

    rmarkdown::draft(
      bare_path
      , system.file(
        "rmarkdown", "templates", "apa6"
        , package = "papaja"
      )
      , create_dir = FALSE
      , edit = FALSE
    )

    try(
      rmarkdown::render(
        bare_path
        , output_format = "papaja::apa6_pdf"
        , quiet = TRUE
      )
    )
    expect_true(file.exists(gsub("\\.Rmd", ".pdf", bare_path)))

    try(
      rmarkdown::render(
        file.path(tempdir(), "bare_skeleton.Rmd")
        , output_format = "papaja::apa6_docx"
        , quiet = TRUE
      )
    )
    expect_true(file.exists(gsub("\\.Rmd", ".docx", bare_path)))
  }
)
