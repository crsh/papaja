


test_that(
  "Extraction methods for apa_results_table"
  , {
    apa_results_table <- apa_print(t.test(yield~N, npk))$table

    p_value <- structure(
      ".022"
      , label = "$p$"
      , class = c("papaja_labelled", "character")
    )

    # Because of partial matching, this is legal:
    expect_identical(
      apa_results_table$p
      , p_value
    )
    expect_identical(
      apa_results_table[["p", exact = FALSE]]
      , p_value
    )
    # But these are not:
    expect_warning(
      p.value_by_double_brackets <- apa_results_table[["p", exact = TRUE]]
      , regexp = "Indexing an apa_results_table with '[[\"p\"]]' is deprecated. Use '[[\"p.value\"]]' instead."
      , fixed = TRUE
    )
    expect_warning(
      p.value_by_single_brackets <- apa_results_table[, "p"]
      , regexp = "Indexing an apa_results_table with '[..., \"p\"]' is deprecated. Use '[..., \"p.value\"]' instead."
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
      , regexp = "Indexing an apa_results_table with '[..., c(\"F\", \"p\")]' is deprecated. Use '[..., c(\"statistic\", \"p.value\")]' instead."
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



  }
)
