
context("apa_print() for MANOVA")

test_that(
  "Two-way MANOVA: apa_print.manova and apa_print.summary.manova"
  , {
    set.seed(42)

    data <- npk
    data$yield2 <- as.integer(data$N) * 2 + rnorm(n = nrow(data), sd = 2)

    x1 <- manova(formula = cbind(yield, yield2) ~ N * P, data = data)
    x2 <- summary(x1, test = "Wilks")

    manova1 <- apa_print(x1) # Pillai
    manova2 <- apa_print(x2) # Wilks
    manova3 <- apa_print(x1, test = "H") # Hotelling-Lawley, pmatch
    manova4 <- apa_print(x1, test = "R", in_paren = TRUE) # Roy's root, pmatch

    expect_apa_results(
      manova1
      , labels = list(
        term                     = "Effect"
        , multivariate.statistic = "$V$"
        , statistic              = "$F$"
        , df                     = "$\\mathit{df}$"
        , df.residual            = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value                = "$p$"
      )
    )
    expect_apa_results(
      manova2
      , labels = list(
        term                     = "Effect"
        , multivariate.statistic = "$\\Lambda$"
        , statistic              = "$F$"
        , df                     = "$\\mathit{df}$"
        , df.residual            = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value                = "$p$"
      )
    )
    expect_apa_results(
      manova3
      , labels = list(
        term                     = "Effect"
        , multivariate.statistic = "$T$"
        , statistic              = "$F$"
        , df                     = "$\\mathit{df}$"
        , df.residual            = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value                = "$p$"
      )
    )
    expect_apa_results(
      manova4
      , labels = list(
        term                     = "Effect"
        , multivariate.statistic = "$\\theta$"
        , statistic              = "$F$"
        , df                     = "$\\mathit{df}$"
        , df.residual            = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value                = "$p$"
      )
    )

    expect_identical(
      object = manova1$table$term
      , expected = structure(
        c("N", "P", "N $\\times$ P")
        , label = "Effect"
        , class = c("tiny_labelled", "character")
      )
    )

    expect_identical(
      object = manova1$statistic$N
      , expected = "$V = 0.25$, $F(2, 19) = 3.22$, $p = .062$"
    )
    expect_identical(
      object = manova2$full_result$N_P
      , expected = "$\\Lambda = 0.96$, $F(2, 19) = 0.37$, $p = .697$"
    )
    expect_identical(
      object = manova3$full_result$P
      , expected = "$T = 0.02$, $F(2, 19) = 0.20$, $p = .821$"
    )
    # in_paren
    expect_identical(
      object = manova4$statistic$N
      , expected = "$\\theta = 0.34$, $F[2, 19] = 3.22$, $p = .062$"
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




