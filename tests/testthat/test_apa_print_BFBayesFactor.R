context("apa_print.BFBayesFactor()")

test_that(
  "Parse interval hypotheses"
  , {
    alt <- data.frame(
      1:3
      , row.names = c(
        "Alt., r=0.707 0<d<Inf"
        , "Alt., r=0.707 !(0<d<Inf)"
        , "Alt., r=0.707 !(5<d<10)"
      )
    )

    alternative_intervals <- add_alternative(
      alt
      , range = c(-Inf, Inf)
    )[, "alternative", drop = FALSE]

    expect_identical(
      alternative_intervals
      , data.frame(
        alternative = c(
          "$[0.00, \\infty]$"
          , "$[-\\infty, 0.00]$"
          , "$[-\\infty, 5.00]~\\cup~[10.00, \\infty]$"
        )
      )
    )

    alt <- data.frame(
      1:2
      , row.names = c(
        "Alt., r=0.333 0<rho<0.5"
        , "Alt., r=0.333 !(0<rho<0.5)"
      )
    )

    alternative_intervals <- add_alternative(
      alt
      , range = c(-1, 1)
    )[, "alternative", drop = FALSE]

    expect_identical(
      alternative_intervals
      , data.frame(
        alternative = c(
          "$[0.00, 0.50]$"
          , "$[-1.00, 0.00]~\\cup~[0.50, 1.00]$"
        )
      )
    )
  }
)

test_that(
  "ttestBF(): One sample"
  , {
    set.seed(123)
    ttest <- BayesFactor::ttestBF(
      x = sleep$extra[sleep$group == 1] - sleep$extra[sleep$group == 2]
    )

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
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_equivalent(
      ttest_output$stat
      , "$\\mathrm{BF}_{\\textrm{10}} = 17.26$"
    )
    expect_identical(
      ttest_output$esti
      , "$M = -1.43$, 95\\% HDI $[-2.33, -0.54]$"
    )
    expect_identical(
      ttest_output$full
      , "$M = -1.43$, 95\\% HDI $[-2.33, -0.54]$, $\\mathrm{BF}_{\\textrm{10}} = 17.26$"
    )

    set.seed(123)
    ttest_paired <- BayesFactor::ttestBF(
      x = sleep$extra[sleep$group == 1]
      , y = sleep$extra[sleep$group == 2]
      , paired = TRUE
    )

    # TODO: Note in documentation that it's not possible to
    # determine if paired = TRUE
    ttest_paired_output <- apa_print(
      ttest_paired
      , est_name = "\\Delta M"
      , central_tendency = median
      , iterations = 10000
    )
    expect_equivalent(
      ttest_paired_output$stat
      , "$\\mathrm{BF}_{\\textrm{10}} = 17.26$"
    )
    expect_identical(
      ttest_paired_output$esti
      , "$\\Delta M = -1.43$, 95\\% HDI $[-2.33, -0.54]$"
    )
    expect_identical(
      ttest_paired_output$full
      , "$\\Delta M = -1.43$, 95\\% HDI $[-2.33, -0.54]$, $\\mathrm{BF}_{\\textrm{10}} = 17.26$"
    )
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
        estimate = "$\\Delta M$"
        , conf.int = "95\\% HDI"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_equivalent(ttest_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 1.27$")
    expect_identical(ttest_output$esti, "$\\Delta M = -1.13$, 95\\% HDI $[-2.76, 0.46]$")
    expect_identical(ttest_output$full, "$\\Delta M = -1.13$, 95\\% HDI $[-2.76, 0.46]$, $\\mathrm{BF}_{\\textrm{10}} = 1.27$")

    # Formula method ----
    set.seed(123L)
    ttest_form <- BayesFactor::ttestBF(formula = extra ~ group, data = sleep)

    ttest_form_output <- apa_print(
      ttest_form
      , central_tendency = median
      , iterations = 10000
    )
    expect_identical(ttest_output, ttest_form_output)

    set.seed(123)
    ttest_onesided <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2], nullInterval = c(0, Inf))

    ttest_onesided_output <- apa_print(
      ttest_onesided
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      ttest_onesided_output
      , labels = list(
        estimate = "$\\Delta M$"
        , conf.int = "95\\% HDI"
        , alternative = "$\\mathcal{H}_1$"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_apa_term(
      ttest_onesided_output
      , term = "interval"
      , estimate = "$\\Delta M = 0.26$, 95\\% HDI $[0.00, 1.00]$"
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 0.17$"
    )

    expect_apa_term(
      ttest_onesided_output
      , term = "inverse_interval"
      , estimate = "$\\Delta M = 0.26$, 95\\% HDI $[0.00, 1.00]$"
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 2.36$"
    )

    ttest_output <- apa_print(
      ttest_onesided
      , mcmc_error = FALSE
      , central_tendency = median
      , iterations = 10000
    )

    expect_null(ttest_output$table$mcmc.error)
  }
)

test_that(
  "correlationBF()"
  , {
    set.seed(123)
    corr <- BayesFactor::correlationBF(
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
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_equivalent(corr_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 0.51$")
    expect_identical(corr_output$esti, "$r = -0.11$, 95\\% HDI $[-0.27, 0.04]$")
    expect_identical(corr_output$full, "$r = -0.11$, 95\\% HDI $[-0.27, 0.04]$, $\\mathrm{BF}_{\\textrm{10}} = 0.51$")
  }
)

test_that(
  "proportionBF()"
  , {
    set.seed(123)
    prop <- BayesFactor::proportionBF(y = 15, N = 25, p = .5)

    prop_output <- apa_print(
      prop
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      prop_output
      , labels = list(
        estimate = "$\\hat\\pi$"
        , conf.int = "95\\% HDI"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_equivalent(prop_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 0.66$")
    expect_identical(prop_output$esti, "$\\hat\\pi = .58$, 95\\% HDI $[.40, .74]$")
    expect_identical(prop_output$full, "$\\hat\\pi = .58$, 95\\% HDI $[.40, .74]$, $\\mathrm{BF}_{\\textrm{10}} = 0.66$")
  }
)

test_that(
  "contingencyTableBF()"
  , {
    data(raceDolls, package = "BayesFactor")
    set.seed(123)
    cont <- BayesFactor::contingencyTableBF(raceDolls, sampleType = "indepMulti", fixedMargin = "cols")

    cont_output <- apa_print(
      cont
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(
      cont_output
      , labels = list(
        statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_identical(cont_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 1.81$")
    expect_identical(cont_output$full, cont_output$stat)
  }
)

test_that(
  "anovaBF()"
  , {
    data(puzzles, package = "BayesFactor")
    set.seed(123)
    anova_bf_main = BayesFactor::anovaBF(
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
        model = "Model"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_apa_term(
      anova_bf_main_output
      , term = "shape_ID"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 2.84$"
    )

    set.seed(123)
    anova_bf_top = BayesFactor::anovaBF(
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
        term = "Term"
        , statistic = "$\\mathrm{BF}_{\\textrm{01}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_apa_term(
      anova_bf_top_output
      , term = "color_shape"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{01}} = 2.65$"
    )

    anova_bf_top_output <- apa_print(
      anova_bf_top
      , scientific_threshold = c(min = 0.99, max = 1.1)
    )

    expect_apa_term(
      anova_bf_top_output
      , term = "color_shape"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{01}} = 2.65$"
    )

    expect_apa_term(
      anova_bf_top_output
      , term = "color"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{01}} = 2.33 \\times 10^{-1}$"
    )

    anova_bf_top_output <- apa_print(anova_bf_top, log = TRUE)

    expect_apa_results(
      anova_bf_top_output
      , labels = list(
        term = "Term"
        , statistic = "$\\log \\mathrm{BF}_{\\textrm{01}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

     expect_apa_term(
      anova_bf_top_output
      , term = "color"
      , estimate = NULL
      , statistic = "$\\log \\mathrm{BF}_{\\textrm{01}} = -1.45$"
    )
  }
)

test_that(
  "regressionBF()"
  , {
    data(attitude)
    set.seed(123)
    regression_bf_all = BayesFactor::regressionBF(
      rating ~ complaints + privileges + learning + raises
      , data = attitude
      , whichModels = "all"
      , progress = FALSE
    )

    regression_bf_all_output <- apa_print(regression_bf_all)

    expect_apa_results(
      regression_bf_all_output
      , labels = list(
        model = "Model"
        , statistic = "$\\mathrm{BF}_{\\textrm{10}}$"
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_apa_term(
      regression_bf_all_output
      , term = "privileges_raises"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 25.90$"
    )

    regression_bf_top = BayesFactor::regressionBF(
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
        , mcmc.error = "$\\pm\\%$"
      )
    )

    expect_apa_term(
      regression_bf_top_output
      , term = "complaints"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{01}} = 2.85 \\times 10^{-3}$"
    )

    regression_bf_top_output <- apa_print(regression_bf_top, inverse = TRUE)

    expect_apa_term(
      regression_bf_top_output
      , term = "complaints"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 351.26$"
    )

    regression_bf_top_output <- apa_print(
      regression_bf_top
      , log = TRUE
      , inverse = TRUE
    )

    expect_apa_term(
      regression_bf_top_output
      , term = "complaints"
      , estimate = NULL
      , statistic = "$\\log \\mathrm{BF}_{\\textrm{10}} = 5.86$"
    )
  }
)

test_that(
  "lmBF()"
  , {
    set.seed(1298)
    data(puzzles, package = "BayesFactor")

    ## Bayes factor of full model against null
    bf_full <- BayesFactor::lmBF(
      RT ~ shape + color + shape:color + ID + ID:shape + ID:color
      , data = puzzles
      , whichRandom = c("ID", "ID:shape", "ID:color")
    )

    expect_identical(
      apa_print(bf_full)$statistic
      , "$\\mathrm{BF}_{\\textrm{10}} = 3,421.93$"
    )

    # bf_shape <- lmBF(
    #   RT ~ color + shape:color + ID + ID:shape + ID:color
    #   , data = puzzles
    #   , whichRandom = c("ID", "ID:shape", "ID:color")
    # )

    # bf_color <- lmBF(
    #   RT ~ shape + shape:color + ID + ID:shape + ID:color
    #   , data = puzzles
    #   , whichRandom = c("ID", "ID:shape", "ID:color")
    # )

    # bf <- bf_full / c(bf_shape, bf_color)
    # bf_res <- apa_print(bf)

  }
)

test_that(
  "generalTestBF()"
  , {
    set.seed(1298)
    data(puzzles, package = "BayesFactor")

    ## Bayes factor of full model against null
    bf <-  BayesFactor::generalTestBF(
      RT ~ shape * color + ID
      , data = puzzles
      , whichRandom = "ID"
      , progress = FALSE
    )

    bf_res <- apa_print(bf)

    expect_apa_term(
      bf_res
      , term = "ID"
      , estimate = NULL
      , statistic = "$\\mathrm{BF}_{\\textrm{10}} = 1.12 \\times 10^{5}$"
    )
  }
)
