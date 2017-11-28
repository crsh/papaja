context("print_anova()")

test_that(
  "Calculation of eta-squared"
  , {
    npk$id <- 1:nrow(npk)
    ow_aov <- apa_print(aov(yield ~ N, npk), es = c("ges", "pes", "es"))
    expect_equal(ow_aov$estimate$N, "$\\hat{\\eta}^2_p = .216$, $\\hat{\\eta}^2_G = .216$, $\\hat{\\eta}^2 = .216$")
    tw_aov <- apa_print(aov(yield ~ N * P, npk), es = c("ges", "pes", "es"), observed = "N")
    expect_equal(tw_aov$estimate$N, "$\\hat{\\eta}^2_p = .224$, $\\hat{\\eta}^2_G = .224$, $\\hat{\\eta}^2 = .216$")
    expect_equal(tw_aov$estimate$P, "$\\hat{\\eta}^2_p = .013$, $\\hat{\\eta}^2_G = .010$, $\\hat{\\eta}^2 = .010$")
    expect_equal(tw_aov$estimate$N_P, "$\\hat{\\eta}^2_p = .031$, $\\hat{\\eta}^2_G = .025$, $\\hat{\\eta}^2 = .024$")

    tw_aov_afex <- apa_print(afex::aov_ez(id = "id", dv = "yield", between = c("N", "P"), data = npk), es = c("ges", "pes", "es"), observed = "N")
    expect_identical(tw_aov$table, tw_aov_afex$table)
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
