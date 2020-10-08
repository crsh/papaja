context("print_anova()")

test_that(
  "Calculation of eta-squared"
  , {
    npk$id <- 1:nrow(npk)
    ow_aov1 <- expect_warning(apa_print(aov(yield ~ N, npk), es = c("ges", "pes", "es")))
    ow_aov2 <- expect_warning(apa_print(aov(yield ~ N, npk), es = c("pes", "es")))
    ow_aov3 <- apa_print(aov(yield ~ N, npk), es = c("es"))
    expect_equal(ow_aov1$estimate$N, "$\\hat{\\eta}^2_G = .216$")
    expect_equal(ow_aov2$estimate$N, "$\\hat{\\eta}^2_p = .216$")
    expect_equal(ow_aov3$estimate$N, "$\\hat{\\eta}^2 = .216$")
    tw_aov1 <- apa_print(aov(yield ~ N * P, npk), es = c("ges"), observed = "N")
    tw_aov2 <- apa_print(aov(yield ~ N * P, npk), es = c("pes"), observed = "N")
    tw_aov3 <- apa_print(aov(yield ~ N * P, npk), es = c("es"), observed = "N")
    expect_equal(tw_aov1$estimate$N, "$\\hat{\\eta}^2_G = .224$")
    expect_equal(tw_aov1$estimate$P, "$\\hat{\\eta}^2_G = .010$")
    expect_equal(tw_aov1$estimate$N_P, "$\\hat{\\eta}^2_G = .025$")

    expect_equal(tw_aov2$estimate$N, "$\\hat{\\eta}^2_p = .224$")
    expect_equal(tw_aov2$estimate$P, "$\\hat{\\eta}^2_p = .013$")
    expect_equal(tw_aov2$estimate$N_P, "$\\hat{\\eta}^2_p = .031$")

    expect_equal(tw_aov3$estimate$N, "$\\hat{\\eta}^2 = .216$")
    expect_equal(tw_aov3$estimate$P, "$\\hat{\\eta}^2 = .010$")
    expect_equal(tw_aov3$estimate$N_P, "$\\hat{\\eta}^2 = .024$")

    tw_aov_afex <- expect_warning(
      apa_print(afex::aov_ez(id = "id", dv = "yield", between = c("N", "P"), data = npk), es = c("ges", "pes", "es"), observed = "N")
    )
    expect_identical(tw_aov1$table, tw_aov_afex$table)
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
