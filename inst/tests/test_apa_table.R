context("apa_table()")

test_that(
  ""
  , {
    load("data/mixed_data.rdata")

    library("dplyr")
    descriptives <- mixed_data %>% group_by(Dosage) %>%
      summarize(
        Mean = printnum( mean(Recall) )
        , Median = printnum( median(Recall) )
        , SD = printnum( sd(Recall) )
        , Min = printnum( min(Recall) )
        , Max = printnum( max(Recall) )
      )

    expect_that(x <- capture.output(apa_table(descriptives, added_colnames = letters[1:5])), throws_error())
  }
)
