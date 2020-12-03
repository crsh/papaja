


test_that(
  "Extraction methods for apa_results_table"
  , {
    apa_results_table <- apa_print(t.test(yield~N, npk))$table

    p_value <- structure(
      ".022"
      , label = "$p$"
      , class = c("tiny_labelled", "character")
    )

    expect_identical(
      expect_warning(
        apa_results_table$p
        , regexp = "To improve consistency of apa_print() output, the column 'p' has been renamed to 'p.value'. The desired values were returned, but please update your code accordingly, as we will drop support for the old column names in a future release."
        , fixed = TRUE
      )
      , p_value
    )
    expect_identical(
      expect_warning(
        apa_results_table[["p", exact = FALSE]]
        , regexp = "To improve consistency of apa_print() output, the column 'p' has been renamed to 'p.value'. The desired values were returned, but please update your code accordingly, as we will drop support for the old column names in a future release."
        , fixed = TRUE
      )
      , p_value
    )
    expect_warning(
      p.value_by_double_brackets <- apa_results_table[["p", exact = TRUE]]
      , regexp = "To improve consistency of apa_print() output, the column 'p' has been renamed to 'p.value'. The desired values were returned, but please update your code accordingly, as we will drop support for the old column names in a future release."
      , fixed = TRUE
    )
    expect_warning(
      p.value_by_single_brackets <- apa_results_table[, "p"]
      , regexp = "To improve consistency of apa_print() output, the column(s) 'p' have been renamed to 'p.value'. The desired values were returned, but please update your code accordingly, as we will drop support for the old column names in a future release."
      , fixed = TRUE
    )
    expect_identical(
      p.value_by_double_brackets
      , expected = p_value
    )
    expect_identical(
      p.value_by_single_brackets
      , expected = p_value
    )
    expect_identical(
      apa_results_table[]
      , apa_results_table
    )

    attr(apa_results_table$conf.int, "conf.level") <- NULL
    expect_identical(
      apa_results_table[1, ]
      , apa_results_table
    )

    expect_warning(
      extract_three_columns <- apa_results_table[, c("conf.int", "F", "p")]
      , regexp = "To improve consistency of apa_print() output, the column(s) 'F', 'p' have been renamed to 'statistic', 'p.value'. The desired values were returned, but please update your code accordingly, as we will drop support for the old column names in a future release."
      , fixed = TRUE
    )
    expect_identical(
      extract_three_columns
      , expected = apa_results_table[, -c(1, 4)]
    )
    # Indexing by integer should still work:
    expect_identical(
      apa_results_table[, 5]
      , p_value
    )

    # Indexing with logical vector
    expect_identical(
      apa_results_table[, c(F, F, F, F, T)]
      , p_value
    )
    expect_identical(
      apa_results_table[, c(F, T, T, F, T)]
      , extract_three_columns
    )

    apa_results_table2 <- apa_print(afex::aov_4(yield ~ (N + P|block), npk))$table
    expect_warning(
      expect_identical(
        apa_results_table2$df1
        , apa_results_table2$df
      )
    )
    expect_warning(
      expect_identical(
        apa_results_table2[["df2"]]
        , apa_results_table2$df.residual
      )
    )
  }
)

test_that(
  "print.apa_results_table()"
  , {
    x <- apa_print(t.test(yield ~ N, npk, paired = TRUE))$table

    print_apa_results_table <- capture_output(print(x))
    expect_identical(
      print_apa_results_table
      , "A data.frame with 5 labelled columns:\n\n  estimate       conf.int statistic df p.value\n1    -5.62 [-9.93, -1.31]     -2.87 11    .015\n\nestimate : $M_d$ \nconf.int : 95\\\\% CI \nstatistic: $t$ \ndf       : $\\\\mathit{df}$ \np.value  : $p$ "
    )
  }
)
