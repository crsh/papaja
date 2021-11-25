context("apa_print.BFBayesFactor()")

test_that(
  "ttestBF(): One sample"
  , {
    set.seed(123)
    ttest <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1] - sleep$extra[sleep$group == 2])

    ttest_output <- apa_print(
      ttest
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      ttest_output
      , labels = list(
        estimate = "$M$"
        , conf.int = "95\\% HDI"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
      )
    )

    expect_equivalent(ttest_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 17.26$")
    expect_identical(ttest_output$esti, "$M = -1.43$ 95\\% HDI $[-2.33, -0.54]$")
    expect_identical(ttest_output$full, "$M = -1.43$ 95\\% HDI $[-2.33, -0.54]$, $\\mathrm{BF}_{\\textrm{10}} = 17.26$")
    
    set.seed(123)
    ttest_paired <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2], paired = TRUE)

    ttest_paired_output <- apa_print(
      ttest_paired
      , central_tendency = median
      , iterations = 10000
    )
    expect_identical(ttest_output, ttest_paired_output)

    # TODO: Decide on output structure (only one estimate?)
    # TODO: Decide on element names
    set.seed(123)
    ttest_paired_onesided <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2], nullInterval = c(0, Inf))
    
    ttest_paired_onesided_output <- apa_print(
      ttest_paired_onesided
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(ttest_paired_onesided_output)
  }
)

test_that(
  "ttestBF(): Independent samples"
  , {
    set.seed(123)
    ttest <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2])

    ttest_output <- apa_print(
      ttest
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      ttest_output
      , labels = list(
        estimate = "$M$"
        , conf.int = "95\\% HDI"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
      )
    )

    expect_equivalent(ttest_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 1.27$")
    expect_identical(ttest_output$esti, "$M = -1.13$ 95\\% HDI $[-2.76, 0.46]$")
    expect_identical(ttest_output$full, "$M = -1.13$ 95\\% HDI $[-2.76, 0.46]$, $\\mathrm{BF}_{\\textrm{10}} = 1.27$")

    # Formula method ----
    set.seed(123L)
    ttest_form <- BayesFactor::ttestBF(formula = extra ~ group, data = sleep)

    ttest_form_output <- apa_print(ttest_form)
    expect_identical(ttest_output, ttest_form_output)
  }
)

test_that(
  "correlationBF()"
  , {
    set.seed(123)
    corr <- correlationBF(
      x = iris$Sepal.Width
      , y = iris$Sepal.Length
    )

    corr_output <- apa_print(
      corr
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      corr_output
      , labels = list(
        estimate = "$r$"
        , conf.int = "95\\% HDI"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
      )
    )

    expect_equivalent(corr_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 0.51$")
    expect_identical(corr_output$esti, "$r = -0.11$ 95\\% HDI $[-0.26, 0.04]$")
    expect_identical(corr_output$full, "$r = -0.11$ 95\\% HDI $[-0.26, 0.04]$, $\\mathrm{BF}_{\\textrm{10}} = 0.51$")
  }
)

test_that(
  "proportionBF()"
  , {
    set.seed(123)
    prop <- proportionBF(y = 15, N = 25, p = .5)

    prop_output <- apa_print(
      prop
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      prop_output
      , labels = list(
        estimate = "$p$"
        , conf.int = "95\\% HDI"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
      )
    )

    expect_equivalent(prop_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 0.66$")
    expect_identical(prop_output$esti, "$p = .53$ 95\\% HDI $[.41, .74]$")
    expect_identical(prop_output$full, "$p = .53$ 95\\% HDI $[.41, .74]$, $\\mathrm{BF}_{\\textrm{10}} = 0.66$")
  }
)

test_that(
  "contingencyTableBF()"
  , {
    skip("Structure of output not decided, yet.")
    data(raceDolls)

    set.seed(123)
    cont <- contingencyTableBF(raceDolls, sampleType = "indepMulti", fixedMargin = "cols")

    cont_output <- apa_print(
      cont
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      cont_output
      , labels = list(
        estimate = "$p$"
        , conf.int = "95\\% HDI"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
      )
    )

    # TODO: Decide on output structure (what's the estimate here?)
    # expect_equivalent(cont_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 1.81$")
    # expect_identical(cont_output$esti, "$p = .38$ 95\\% HDI $[.41, .74]$")
    # expect_identical(cont_output$full, "$p = .38$ 95\\% HDI $[.41, .74]$, $\\mathrm{BF}_{\\textrm{10}} = 1.81$")
  }
)

test_that(
  "anovaBF()"
  , {
    data(puzzles)
    set.seed(123)
    anova_bf_main = anovaBF(
      RT ~ shape*color + ID
      , data = puzzles
      , whichRandom = "ID"
      , whichModels = "withmain"
      , progress = FALSE
    )

    anova_bf_main_output <- apa_print(anova_bf_main)

    expect_apa_results(
      anova_bf_main_output
      , labels = list(
        term = "Model"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
      )
    )

    expect_apa_term(
      anova_bf_main_output
      , term = "shape + ID"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 2.84$"
    )

    anova_bf_top = anovaBF(
      RT ~ shape*color + ID
      , data = puzzles
      , whichRandom = "ID"
      , whichModels = "top"
      , progress = FALSE
    )

    anova_bf_top_output <- apa_print(anova_bf_top)

    expect_apa_results(
      anova_bf_top_output
      , labels = list(
        term = "Effect"
        , statistic = "$\\mathrm{BF}_{\\textrm{01}}$"
      )
    )

    expect_apa_term(
      anova_bf_top_output
      , term = "color_shape"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{01}} = 2.74$"
    )

    anova_bf_top_output <- apa_print(anova_bf_top, auto_invert = TRUE)

    expect_apa_term(
      anova_bf_top_output
      , term = "color_shape"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 0.36$"
    )
  }
)

test_that(
  "regressionBF()"
  , {
    data(attitude)
    set.seed(123)
    regression_bf_all = regressionBF(
      rating ~ complaints + privileges + learning + raises
      , data = attitude
      , whichModels = "all"
      , progress = FALSE
    )

    regression_bf_all_output <- apa_print(regression_bf_all)

    expect_apa_results(
      regression_bf_all_output
      , labels = list(
        term = "Model"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
      )
    )

    expect_apa_term(
      regression_bf_all_output
      , term = "priveleges + raises"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 25.90$"
    )

    regression_bf_top = regressionBF(
      rating ~ complaints + privileges + learning + raises
      , data = attitude
      , whichModels = "top"
      , progress = FALSE
    )

    regression_bf_top_output <- apa_print(regression_bf_top)

    expect_apa_results(
      regression_bf_top_output
      , labels = list(
        term = "Term"
        , statistic = "$\\mathrm{BF}_{\\textrm{01}}$"
      )
    )

    expect_apa_term(
      regression_bf_top_output
      , term = "complaints"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{01}} = 2.85 \\times 10^{-3}$"
    )

    regression_bf_top_output <- apa_print(regression_bf_top, auto_invert = TRUE)

    expect_apa_term(
      regression_bf_top_output
      , term = "complaints"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 351.26$"
    )
  }
)

# TODO: lmBF
# TODO: generalTestBF
