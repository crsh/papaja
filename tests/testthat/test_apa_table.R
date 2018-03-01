context("apa_table()")

test_that(
  ""
  , {
    load("data/mixed_data.rdata")

    descriptives <- dplyr::group_by(mixed_data, Dosage)

    descriptives <- dplyr::summarize(
      descriptives
        , Mean = printnum( mean(Recall) )
        , Median = printnum( median(Recall) )
        , SD = printnum( sd(Recall) )
        , Min = printnum( min(Recall) )
        , Max = printnum( max(Recall) )
      )

    expect_error(x <- capture.output(apa_table(descriptives, added_colnames = letters[1:5])))

    rmarkdown::render("test_apa_table.Rmd", quiet = TRUE)
    file.remove("test_apa_table.pdf", "test_apa_table.ttt")
  }
)
