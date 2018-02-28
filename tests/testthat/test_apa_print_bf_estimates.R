context("bf_estimates()")

library("BayesFactor")

test_that(
  "bf_estimates.BFoneSample()"
  , {
    set.seed(123)
    ttest <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2], paired = TRUE)
    posterior_samples <- BayesFactor::posterior(ttest, iterations = 5000)

    # Default
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
    )

    expect_length(estimates, 1)
    expect_is(estimates, "character")
    expect_equal(estimates, "$M = -1.43$ 95\\% HDI $[-2.33$, $-0.57]$")

    # Custom HDI coverage
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
      , hdi = 0.5
    )

    expect_equal(estimates, "$M = -1.43$ 50\\% HDI $[-1.74$, $-1.19]$")

    # Custom central tendency
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
      , central_tendency = mean
    )

    expect_equal(estimates, "$M = -1.41$ 95\\% HDI $[-2.33$, $-0.57]$")

    # Standardized
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
      , standardized = TRUE
    )

    expect_equal(estimates, "$d = -1.08$ 95\\% HDI $[-1.90$, $-0.25]$")
  }
)

test_that(
  "bf_estimates.BFindepSample()"
  , {
    set.seed(123)
    ttest <- BayesFactor::ttestBF(x = sleep$extra[sleep$group == 1], y = sleep$extra[sleep$group == 2])
    posterior_samples <- BayesFactor::posterior(ttest, iterations = 5000)

    # Default
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
    )

    expect_length(estimates, 1)
    expect_is(estimates, "character")
    expect_equal(estimates, "$M = -1.13$ 95\\% HDI $[-2.78$, $0.50]$")

    # Custom HDI coverage
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
      , hdi = 0.5
    )

    expect_equal(estimates, "$M = -1.13$ 50\\% HDI $[-1.56$, $-0.48]$")

    # Custom central tendency
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
      , central_tendency = mean
    )

    expect_equal(estimates, "$M = -1.16$ 95\\% HDI $[-2.78$, $0.50]$")

    # Standardized
    estimates <- bf_estimates(
      ttest@numerator$`Alt., r=0.707`
      , posterior_samples
      , standardized = TRUE
    )

    expect_equal(estimates, "$d = -0.58$ 95\\% HDI $[-1.47$, $0.24]$")
  }
)

# test_that(
#   "bf_estimates.BFlinearModel() - One-way ANOVA"
#   , {
#     set.seed(123)
#     linear_model_anova_oneway_between <- BayesFactor::anovaBF(extra ~ group, data = sleep, progress = FALSE)
#     posterior_samples <- BayesFactor::posterior(
#       linear_model_anova_oneway_between
#       , index = length(linear_model_anova_oneway_between@numerator)
#       , iterations = 5000
#     )
#
#     # Default
#     estimates <- bf_estimates(
#       tail(linear_model_anova_oneway_between@numerator, 1)[[1]]
#       , posterior_samples
#     )
#
#     expect_length(estimates, 1)
#     expect_is(estimates, "character")
#     expect_equal(estimates, "$M = -1.13$ 95\\% HDI $[-2.78$, $0.50]$")
#
#     # Custom HDI coverage
#     estimates <- bf_estimates(
#       ttest@numerator$`Alt., r=0.707`
#       , posterior_samples
#       , hdi = 0.5
#     )
#
#     expect_equal(estimates, "$M = -1.13$ 50\\% HDI $[-1.56$, $-0.48]$")
#
#     # Custom central tendency
#     estimates <- bf_estimates(
#       ttest@numerator$`Alt., r=0.707`
#       , posterior_samples
#       , central_tendency = mean
#     )
#
#     expect_equal(estimates, "$M = -1.16$ 95\\% HDI $[-2.78$, $0.50]$")
#
#     # Standardized
#     estimates <- bf_estimates(
#       ttest@numerator$`Alt., r=0.707`
#       , posterior_samples
#       , standardized = TRUE
#     )
#
#     expect_equal(estimates, "$d = -0.58$ 95\\% HDI $[-1.47$, $0.24]$")
#   }
# )
#
# test_that(
#   "bf_estimates.BFlinearModel() - Two-way ANOVA"
#   , {
#     set.seed(123)
#     linear_model_anova_twoway_between <- BayesFactor::anovaBF(yield ~ N * K, data = npk, progress = FALSE)
#     posterior_samples <- BayesFactor::posterior(
#       linear_model_anova_twoway_between
#       , index = length(linear_model_anova_twoway_between@numerator)
#       , iterations = 5000
#     )
#
#     # Default
#     estimates <- bf_estimates(
#       ttest@numerator$`Alt., r=0.707`
#       , posterior_samples
#     )
#
#     expect_length(estimates, 1)
#     expect_is(estimates, "character")
#     expect_equal(estimates, "$M = -1.13$ 95\\% HDI $[-2.78$, $0.50]$")
#
#     # Custom HDI coverage
#     estimates <- bf_estimates(
#       ttest@numerator$`Alt., r=0.707`
#       , posterior_samples
#       , hdi = 0.5
#     )
#
#     expect_equal(estimates, "$M = -1.13$ 50\\% HDI $[-1.56$, $-0.48]$")
#
#     # Custom central tendency
#     estimates <- bf_estimates(
#       ttest@numerator$`Alt., r=0.707`
#       , posterior_samples
#       , central_tendency = mean
#     )
#
#     expect_equal(estimates, "$M = -1.16$ 95\\% HDI $[-2.78$, $0.50]$")
#
#     # Standardized
#     estimates <- bf_estimates(
#       ttest@numerator$`Alt., r=0.707`
#       , posterior_samples
#       , standardized = TRUE
#     )
#
#     expect_equal(estimates, "$d = -0.58$ 95\\% HDI $[-1.47$, $0.24]$")
#   }
# )

# BFmetat, BFlinearModel, BFcontingencyTable, BFproportion, BFcorrelation, BFmodel

detach("package:BayesFactor")
