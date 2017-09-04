context("print_anova()")

test_that(
  "Calculation of eta-squared"
  , {
    ow_aov <- apa_print(aov(yield ~ N, npk), es = c("ges", "pes", "es"))
    expect_equal(ow_aov$estimate$N, "$\\eta^2_p = .216$, $\\eta^2_G = .216$, $\\eta^2 = .216$")
    tw_aov <- apa_print(aov(yield ~ N * P, npk), es = c("ges", "pes", "es"), observed = "N")
    expect_equal(tw_aov$estimate$N, "$\\eta^2_p = .224$, $\\eta^2_G = .224$, $\\eta^2 = .216$")
    expect_equal(tw_aov$estimate$P, "$\\eta^2_p = .013$, $\\eta^2_G = .010$, $\\eta^2 = .010$")
    expect_equal(tw_aov$estimate$N_P, "$\\eta^2_p = .031$, $\\eta^2_G = .025$, $\\eta^2 = .024$")
  }
)

test_that(
  "print_anova(): Throw an error if a missing variable is specified as `observed`."
  , {
    expect_error(
      apa_print(aov(formula = yield ~ N, data = npk), observed = "P")
      , "Observed variable not in data: P"
    )
  }
)
