
test_that(
  "apa_print.lme() and apa_print.anova.lme()"
  , {
      # From the examples section:
      fm1 <- nlme::lme(distance ~ age, data = nlme::Orthodont, method = "ML") # random is ~ age
      fm2 <- nlme::lme(distance ~ age + Sex, data = nlme::Orthodont, random = ~ 1, method = "ML")

      single_anova <- anova(fm1)
      model_comp <- anova(fm1, fm2)

      lme_model <- apa_print(fm1)
      lme_anova <- apa_print(single_anova)

      expect_apa_results(
        lme_model
        , labels = list(
          term        = "Term"
          , estimate  = "$\\hat{\\beta}$"
          , conf.int  = "95\\% CI"
          , statistic = "$t$"
          , df        = "$\\mathit{df}$"
          , p.value   = "$p$"
        )
      )
      expect_identical(
        lme_model$full_result
        , expected = list(
          Intercept = "$\\hat{\\beta} = 16.76$, 95\\% CI $[15.25, 18.28]$, $t(80) = 21.83$, $p < .001$"
          , age = "$\\hat{\\beta} = 0.66$, 95\\% CI $[0.52, 0.80]$, $t(80) = 9.35$, $p < .001$"
        )
      )

      expect_error(
        apa_print(model_comp)
        , "Model-comparison tables of class 'anova.lme' are not supported."
      )

      # Also works for nonlinear models of class 'c("nlme", "lme")'
      # from the example section of nlme::nlme()
      fm1 <- nlme::nlme(height ~ stats::SSasymp(age, Asym, R0, lrc),
                        data = datasets::Loblolly,
                        fixed = Asym + R0 + lrc ~ 1,
                        random = Asym ~ 1,
                        start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
      nlme_model <- apa_print(fm1)

      expect_apa_results(
        nlme_model
        , labels = list(
          term        = "Term"
          , estimate  = "$\\hat{\\beta}$"
          , conf.int  = "95\\% CI"
          , statistic = "$t$"
          , df        = "$\\mathit{df}$"
          , p.value   = "$p$"
        )
      )
      expect_identical(
        object = nlme_model$full_result
        , expected = list(
          Asym = "$\\hat{\\beta} = 101.45$, 95\\% CI $[96.63, 106.27]$, $t(68) = 41.21$, $p < .001$"
          , R0 = "$\\hat{\\beta} = -8.63$, 95\\% CI $[-9.25, -8.00]$, $t(68) = -27.13$, $p < .001$"
          , lrc = "$\\hat{\\beta} = -3.23$, 95\\% CI $[-3.30, -3.17]$, $t(68) = -94.36$, $p < .001$"
        )
      )

      expect_warning(
        apa_print(fm1, args_confint = .9)
        , "deprecated. Please use 'conf.int' instead."
      )
  }
)

test_that(
  "Deprecated 'args_confint' argument"
  , {
    fm1 <- nlme::nlme(height ~ stats::SSasymp(age, Asym, R0, lrc),
                      data = datasets::Loblolly,
                      fixed = Asym + R0 + lrc ~ 1,
                      random = Asym ~ 1,
                      start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
    expect_warning(
      nlme_model <- apa_print(fm1, args = .6)
      , regexp = "Using argument 'args_confint' in calls to 'apa_print()' is deprecated. Please use 'conf.int' instead."
      , fixed = TRUE
    )
    expect_identical(
      nlme_model$estimate$Asym
      , "$\\hat{\\beta} = 101.45$, 60\\% CI $[99.40, 103.50]$"
    )


  }
)
