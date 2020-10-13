
test_that(
  "apa6_pdf() defaults"
  , {
    apa6_format <- apa6_pdf()

    expect_s3_class(apa6_format, "rmarkdown_output_format", exact = TRUE)
    expect_identical(
      apa6_format$knitr$opts_chunk
      , expected = list(
        dev = c("pdf", "png")
        , fig.width = 6.5
        , fig.height = 4.5
        , dev.args = list(
          pdf = list(
            useDingbats = FALSE
          )
        )
        , crop = TRUE
        , echo = FALSE
        , message = FALSE
        , fig.cap = " "
        , dpi = 300
      )
    )
  }
)

test_that(
  "apa6_docx() defaults"
  , {
    apa6_format <- apa6_docx()

    expect_s3_class(apa6_format, "rmarkdown_output_format", exact = TRUE)
    expect_identical(
      apa6_format$knitr$opts_chunk
      , expected = list(
        dev = c("png", "pdf")
        , dpi = 300
        , fig.width = 5
        , fig.height = 4
        , echo = FALSE
        , message = FALSE
      )
    )
  }
)
