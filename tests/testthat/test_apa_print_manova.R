
context("apa_print() for MANOVA")

test_that(
  "Two-way MANOVA: apa_print.manova and apa_print.summary.manova"
  , {
    set.seed(42)

    data <- npk
    data$yield2 <- as.integer(data$N) * 2 + rnorm(n = nrow(data), sd = 2)

    x1 <- manova(formula = cbind(yield, yield2) ~ N * P, data = data)
    x2 <- summary(x1, test = "Wilks") # tidy.summary.manova not yet on cran

    out1 <- apa_print(x1) # Pillai
    # out2 <- apa_print(x2) # Wilks
    out3 <- apa_print(x1, test = "H") # Hotelling-Lawley, pmatch
    out4 <- apa_print(x1, test = "R", in_paren = TRUE) # Roy's root, pmatch

    expect_identical(
      object = out1$statistic$N
      , expected = "$V = 0.25$, $F(2, 19) = 3.22$, $p = .062$"
    )
    # expect_identical(
    #   object = out2$full_result$N_P
    #   , expected = "$\\Lambda = 0.96$, $F(2, 19) = 0.37$, $p = .697$"
    # )
    expect_identical(
      object = out3$full_result$P
      , expected = "$T = 0.02$, $F(2, 19) = 0.20$, $p = .821$"
    )

    # in_paren
    expect_identical(
      object = out4$statistic$N
      , expected = "$\\theta = 0.34$, $F[2, 19] = 3.22$, $p = .062$"
    )

    # apa_results_table
    expect_identical(
      object = variable_labels(out3$table)
      , expected = list(
        Effect = "Effect"
        , hl = "$T$"
        , `F` = "$F$"
        , df1 = "$\\mathit{df}_1$"
        , df2 = "$\\mathit{df}_2$"
        , p = "$p$"
      )
    )
    expect_identical(
      object = class(out1$table)
      , expected = c("apa_results_table", "data.frame")
    )
  }
)

test_that(
  "Anova.mlm from MANOVA not supported"
  , {
    set.seed(42)

    data <- npk
    data$yield2 <- as.integer(data$N) * 2 + rnorm(n = nrow(data), sd = 2)

    expect_error(
      apa_print(car::Manova(manova(formula = cbind(yield, yield2) ~ N * P, data = data), type = 3))
      , fixed = TRUE
      , regexp = "Anova.mlm objects from car::Manova are not supported, yet. Visit https://github.com/crsh/papaja/issues to request support for this class. You can try using stats::manova instead if Type I oder Type II sums of squares are adequate for your analysis."
    )
  }
)




