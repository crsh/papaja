
test_that(
  "apa6_pdf() defaults"
  , {
    apa6_format <- apa6_pdf()

    expect_s3_class(apa6_format, "rmarkdown_output_format", exact = TRUE)
    expect_identical(apa6_format$knitr$opts_chunk$echo, FALSE)
    expect_identical(apa6_format$knitr$opts_chunk$message, FALSE)
    expect_identical(apa6_format$knitr$opts_chunk$fig.cap, " ")
    expect_identical(apa6_format$knitr$opts_knit$rmarkdown.pandoc.to, "latex")
    expect_identical(apa6_format$knitr$opts_chunk$dev, c("pdf", "png"))
    expect_identical(apa6_format$knitr$opts_chunk$dpi, 300)
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
