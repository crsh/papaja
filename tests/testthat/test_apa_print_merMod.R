context("apa_print() for hierarchical linear models")

test_that(
  "Fixed-effects summaries"
  , {
    skip_on_cran()
    model_lme4 <- lme4::lmer(formula = yield ~ N + (1|block), data = npk)
    # model2_lme4 <- lme4::lmer(formula = yield ~ N + P + (1|block), data = npk)
    model_lmerTest <- lmerTest::as_lmerModLmerTest(model_lme4)

    apa_lme4 <- apa_print(model_lme4)
    # apa2_lme4 <-apa_print(model2_lme4)
    apa_lmerTest <- apa_print(model_lmerTest)
    apa_lmerTest_specialties <- apa_print(model_lmerTest, in_paren = TRUE, args_confint = list(level = .96), digits = 4L, est_name = "$\\gamma")

    expect_apa_results(
      apa_lme4
      , labels = list(
        term        = "Term"
        , estimate  = "$\\hat{\\beta}$"
        , conf.int  = "95\\% CI"
        , statistic = "$t$"
      )
    )
    expect_apa_results(
      apa_lmerTest
      , labels = list(
        term        = "Term"
        , estimate  = "$\\hat{\\beta}$"
        , conf.int  = "95\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value   = "$p$"
      )
    )
    expect_apa_results(
      apa_lmerTest_specialties
      , labels = list(
        term        = "Term"
        , estimate  = "$\\gamma$"
        , conf.int  = "96\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value   = "$p$"
      )
    )

    expect_identical(
      object = apa_lmerTest$estimate
      , expected = list(
        Intercept = "$\\hat{\\beta} = 52.07$, 95\\% CI $[48.29, 55.84]$"
        , N1 = "$\\hat{\\beta} = 5.62$, 95\\% CI $[2.02, 9.21]$"
      )
    )
    expect_identical(
      object = apa_lmerTest$statistic
      , expected = list(
        Intercept = "$t(8.17) = 27.06$, $p < .001$"
        , N1 = "$t(17.00) = 3.06$, $p = .007$"
      )
    )
    expect_identical(
      object = apa_lmerTest$full_result
      , expected = list(
        Intercept = "$\\hat{\\beta} = 52.07$, 95\\% CI $[48.29, 55.84]$, $t(8.17) = 27.06$, $p < .001$"
        , N1 = "$\\hat{\\beta} = 5.62$, 95\\% CI $[2.02, 9.21]$, $t(17.00) = 3.06$, $p = .007$"
      )
    )

    expect_identical(
      object = apa_lme4$estimate
      , expected = apa_lmerTest$estimate
    )
    expect_identical(
      object = apa_lme4$statistic
      , expected = list(
        Intercept = "$t = 27.06$"
        , N1 = "$t = 3.06$"
      )
    )
    expect_identical(
      object = apa_lme4$full_result
      , expected = list(
        Intercept = "$\\hat{\\beta} = 52.07$, 95\\% CI $[48.29, 55.84]$, $t = 27.06$"
        , N1 = "$\\hat{\\beta} = 5.62$, 95\\% CI $[2.02, 9.21]$, $t = 3.06$"
      )
    )

    expect_identical( # in_paren
      object = apa_lmerTest_specialties$full_result$N1
      , expected = "$\\gamma = 5.6167$, 96\\% CI $[1.8462, 9.3871]$, $t[17.00] = 3.06$, $p = .007$"
    )

    # Test reduction of (Days | Subject) to (1 | Subject):
    data <- lme4::sleepstudy

    fm1 <- lmerTest::lmer(Reaction ~ Days + (Days|Subject), data)
    ranova_out <- lmerTest::ranova(fm1)

    # Also test reduction of two terms to single:
    set.seed(12305L)
    data$Days2 <- rnorm(nrow(data))
    data$Reaction2 <- data$Reaction + data$Days2*(as.integer(data$Subject)-mean(as.integer(data$Subject)))
    fm2 <- lmerTest::lmer(Reaction2 ~ Days + (Days + Days2|Subject), data)
    lmerTest::ranova(fm2)

    expect_error(
      apa_print(ranova_out)
      , "Single-term deletions are not supported, yet.\nVisit https://github.com/crsh/papaja/issues to request support."
    )
  }
)

test_that(
  "Fixed effects from hierarchical generalized linear models"
  , {
    skip_on_cran()

    # from lme4::glmer examples:
    gm1 <- lme4::glmer(
      formula = cbind(incidence, size - incidence) ~ period + (1 | herd)
      , data = lme4::cbpp
      , family = stats::binomial
    )

    apa_gm1 <- apa_print(gm1, args_confint = list(level = .90))

    expect_apa_results(
      apa_gm1
      , labels =  list(
        term = "Term"
        , estimate  = "$\\hat{\\beta}$"
        , conf.int  = "90\\% CI"
        , statistic = "$z$"
        , p.value   = "$p$"
      )
    )

    expect_identical(
      object = apa_gm1$full_result
      , expected = list(
        Intercept = "$\\hat{\\beta} = -1.40$, 90\\% CI $[-1.78, -1.02]$, $z = -6.05$, $p < .001$"
        , period2 = "$\\hat{\\beta} = -0.99$, 90\\% CI $[-1.49, -0.49]$, $z = -3.27$, $p = .001$"
        , period3 = "$\\hat{\\beta} = -1.13$, 90\\% CI $[-1.66, -0.60]$, $z = -3.49$, $p < .001$"
        , period4 = "$\\hat{\\beta} = -1.58$, 90\\% CI $[-2.27, -0.89]$, $z = -3.74$, $p < .001$"
      )
    )
  }
)

test_that(
  "ANOVA tables from lmerTest::anova()"
  , {
    fm1 <- lmerTest::lmer(Reaction ~ Days + (Days|Subject), lme4::sleepstudy)
    fm2 <-lme4::lmer(Reaction ~ Days + (1|Subject)   , lme4::sleepstudy)
    fm3 <- lme4::lmer(Reaction ~ 1 + (1|Subject), lme4::sleepstudy)

    # single-model tables
    model_KR <- anova(fm1, type = "III", ddf = "Kenward")
    model_S <- anova(fm1, type = "II")

    apa_KR <- apa_print(model_KR)
    apa_S <- apa_print(model_S)

    expect_apa_results(
      apa_KR
      , labels = list(
        term          = "Effect"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}^{\\mathrm{KR}}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{KR}}$"
        , p.value     = "$p$"
      )
    )
    expect_apa_results(
      apa_S
      , labels = list(
        term          = "Effect"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}^{\\mathrm{S}}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{S}}$"
        , p.value     = "$p$"
      )
    )

    expect_identical(
      apa_KR$full_result
      , expected = "$F(1, 17.00) = 45.85$, $p < .001$"
    )
    expect_identical(
      apa_S$full_result
      , apa_KR$full_result
    )

    # Stop model-comparison tables
    model_comp <- anova(fm1, fm2, fm3)
    model_comp2 <- anova(fm2, fm3)
    expect_error(
      apa_print(model_comp)
      , "Model-comparison objects of class 'anova' are not supported."
    )

  }
)

test_that(
  "Type-3 tables from afex::mixed"
  , {
    set.seed(42L)
    captured <- capture_output({
      mixed_KR  <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "KR")
      mixed_S   <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "S")
      mixed_PB  <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "PB", args_test = list(nsim = 50))
      mixed_LRT <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "LRT")
    })
    #
    apa_KR  <- apa_print(mixed_KR)
    apa_S   <- apa_print(mixed_S)
    apa_PB  <- apa_print(mixed_PB)
    apa_LRT <- apa_print(mixed_LRT)

    expect_apa_results(
      apa_KR
      , labels = list(
        term          = "Effect"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}^{\\mathrm{KR}}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{KR}}$"
        , p.value     = "$p$"
      )
    )

    expect_apa_results(
      apa_S
      , labels = list(
        term          = "Effect"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}^{\\mathrm{S}}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{S}}$"
        , p.value     = "$p$"
      )
    )

    expect_apa_results(
      apa_PB
      , labels = list(
        term        = "Effect"
        , statistic = "$\\chi^2$"
        , p.value   = "$p$"
      )
    )

    expect_apa_results(
      apa_LRT
      , labels = list(
        term        = "Effect"
        , statistic = "$\\chi^2$"
        , df        = "$\\mathit{df}$"
        , p.value   = "$p$"
      )
    )

    expect_identical(
      apa_KR$table$term
      , structure(
        c("N", "P", "N $\\times$ P")
        , label = "Effect"
        , class = c("papaja_labelled", "character")
      )
    )


    expect_identical(
      object = apa_KR$statistic
      , expected = list(
        N = "$F(1, 15.00) = 9.04$, $p = .009$"
        , P = "$F(1, 15.00) = 0.40$, $p = .536$"
        , N_P = "$F(1, 15.00) = 1.02$, $p = .329$"
      )
    )
    expect_identical( # KR gives same results as S in this special case
      object = apa_S$statistic
      , expected = apa_KR$statistic
    )
    expect_identical(
      object = apa_PB$statistic
      , expected = list(
        N = "$\\chi^2 = 8.49$, $p = .039$"
        , P = "$\\chi^2 = 0.48$, $p = .549$"
        , N_P = "$\\chi^2 = 1.18$, $p = .255$"
      )
    )
    expect_identical(
      object = apa_LRT$statistic
      , expected = list(
        N = "$\\chi^2(1) = 8.49$, $p = .004$"
        , P = "$\\chi^2(1) = 0.48$, $p = .491$"
        , N_P = "$\\chi^2(1) = 1.18$, $p = .277$"
      )
    )
  }
)
