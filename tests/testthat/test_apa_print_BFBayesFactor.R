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

    expect_is(ttest_output, "list")
    expect_equal(names(ttest_output), container_names)

    # stat
    expect_equal(length(ttest_output$stat), 1)
    expect_is(ttest_output$stat, "character")

    expect_equivalent(ttest_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 17.26$", )

    # est
    expect_equal(length(ttest_output$est), 1)
    expect_is(ttest_output$est, "character")

    expect_equal(ttest_output$est, "$M = -1.43$ 95\\% HDI $[-2.33$, $-0.54]$")

    # full
    expect_equal(length(ttest_output$full), 1)
    expect_is(ttest_output$full, "character")

    expect_equal(ttest_output$full, "$M = -1.43$ 95\\% HDI $[-2.33$, $-0.54]$, $\\mathrm{BF}_{\\textrm{10}} = 17.26$")
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

    expect_is(ttest_output, "list")
    expect_equal(names(ttest_output), container_names)

    # stat
    expect_equal(length(ttest_output$stat), 1)
    expect_is(ttest_output$stat, "character")

    expect_equivalent(ttest_output$stat, "$\\mathrm{BF}_{\\textrm{10}} = 1.27$", )

    # est
    expect_equal(length(ttest_output$est), 1)
    expect_is(ttest_output$est, "character")

    expect_equal(ttest_output$est, "$M = -1.13$ 95\\% HDI $[-2.76$, $0.46]$")

    # full
    expect_equal(length(ttest_output$full), 1)
    expect_is(ttest_output$full, "character")

    expect_equal(ttest_output$full, "$M = -1.13$ 95\\% HDI $[-2.76$, $0.46]$, $\\mathrm{BF}_{\\textrm{10}} = 1.27$")
  }
)

detach("package:BayesFactor")
