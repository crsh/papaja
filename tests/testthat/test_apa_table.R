context("apa_table()")

test_that(
  ""
  , {
    load("data/mixed_data.rdata")

    descriptives <- dplyr::group_by(mixed_data, Dosage)

    descriptives <- dplyr::summarize(
      descriptives
        , Mean = apa_num( mean(Recall) )
        , Median = apa_num( median(Recall) )
        , SD = apa_num( sd(Recall) )
        , Min = apa_num( min(Recall) )
        , Max = apa_num( max(Recall) )
      )

    expect_error(x <- capture.output(apa_table(descriptives, added_colnames = letters[1:5])))

    skip_on_cran()
    rmarkdown::render("test_apa_table.Rmd", quiet = TRUE)
    suppressWarnings(file.remove("test_apa_table.pdf", "test_apa_table.ttt"))
  }
)

test_that(
  "Check that midrules are added even if col.names = NULL"
  , {
    my_table <- t(apply(cars, 2, function(x) # Create data
      round(c(Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)), 2)
    ))

    res_table <- apa_table(my_table, midrules = c(1), caption = "should have midrules", col.names = NULL)
    table_lines <- unlist(strsplit(res_table, split = "\n", fixed = TRUE))

    expect_identical(
      table_lines[[13L]]
      , expected = "speed & 15.40 & 5.29 & 4.00 & 25.00\\\\ \\midrule"
    )

  }
)
