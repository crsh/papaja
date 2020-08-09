context("apa_print.BFBayesFactor()")

library("BayesFactor")

test_that(
  "ttestBF(): One sample"
  , {
    set.seed(123)
    ttest <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2], paired = TRUE)

    ttest_output <- apa_print(
      ttest
      , central_tendency = median
      , iterations = 10000
    )

    expect_apa_results(ttest_output)

    expect_equivalent(ttest_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 17.26$")
    expect_identical(ttest_output$esti, "$M = -1.43$ 95\\% HDI $[-2.33, -0.54]$")
    expect_identical(ttest_output$full, "$M = -1.43$ 95\\% HDI $[-2.33, -0.54]$, $\\mathrm{BF}_{\\textrm{10}} = 17.26$")
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

    expect_apa_results(ttest_output)

    expect_equivalent(ttest_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 1.27$")
    expect_identical(ttest_output$esti, "$M = -1.13$ 95\\% HDI $[-2.76, 0.46]$")
    expect_identical(ttest_output$full, "$M = -1.13$ 95\\% HDI $[-2.76, 0.46]$, $\\mathrm{BF}_{\\textrm{10}} = 1.27$")
  }
)

detach("package:BayesFactor")
