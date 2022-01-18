
test_that(
  "fuse_df()"
  , {
    # only one df, non-integer df, update label and remove extraneous column:
    t_out1 <- apa_print(t.test(formula = yield ~ N, data = npk))

    fused_1 <- fuse_df(t_out1)
    expect_identical(
      variable_label(fused_1$table$statistic)
      , "$t(21.88)$"
    )
    expect_identical(
      colnames(fused_1$table)
      , c("estimate", "conf.int", "statistic", "p.value")
    )

    # only one df, integer df, update label and remove extraneous column:
    t_out2 <- apa_print(t.test(formula = yield ~ N, data = npk, var.equal = TRUE))

    fused_2 <- fuse_df(t_out2)
    expect_identical(
      variable_label(fused_2$table$statistic)
      , "$t(22)$"
    )
    expect_identical(
      colnames(fused_2$table)
      , c("estimate", "conf.int", "statistic", "p.value")
    )

    # same df for multiple terms, update label and remove extraneous columns:
    between_anova_out <- apa_print(aov(yield ~ N*P, data = npk), estimate = "pes")

    fused_out <- fuse_df(between_anova_out)
    expect_identical(
      tinylabels::variable_label(fused_out$table$statistic)
      , "$F(1, 20)$"
    )
    expect_identical(
      colnames(fused_out$table)
      , c("term", "estimate", "statistic", "mse", "p.value")
    )

    # df vary by term, should result in an error when trying to fuse:
    npk2 <- rbind(npk, npk)
    npk2$N <- factor(as.integer(npk2$N) + rep(c(0, 2), each = nrow(npk)))
    between_anova_out <- apa_print(aov(yield ~ N*P, data = npk2), estimate = "pes")
    expect_error(
      fuse_df(between_anova_out)
      , fixed = TRUE
      , regexp = "Degrees of freedom (in column 'df') vary by model term."
    )

    multiple_df <- rbind(t_out1$table, t_out2$table)
    expect_error(
      fuse_df(multiple_df)
      , regexp = "Degrees of freedom (in column 'df') vary by model term."
      , fixed = TRUE
    )

    noninteger_df <- rbind(t_out1$table, t_out1$table)
    expect_warning(
      fuse_df(noninteger_df)
      , regexp = "Column 'df' contains non-integer values."
      , fixed = TRUE
    )

    # If no df columns are present in 'x', simply return 'x' but throw a message
    # if quietly = FALSE
    apa_no_df <- apa_print(wilcox.test(yield ~ N, data = npk, exact = F))

    # No message if quietly == TRUE:
    junk <- fuse_df(apa_no_df, quietly = TRUE)

    # By default, expect an informative message:
    expect_message(
      fused_out <- fuse_df(apa_no_df)
      , fixed = TRUE
      , regexp = "There are no df columns to fuse. Returning original input."
    )
    # Also check that x is not modified:
    expect_identical(
      apa_no_df
      , fused_out
    )
  }
)
