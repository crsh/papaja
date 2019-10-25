context("apa_print() for hierarchical linear models")

test_that(
  "first few tests"
  , {
    testthat::skip_on_travis()
    model_object <- lme4::lmer(formula = yield ~ N + (1|block), data = npk)
    summary_object <- summary(model_object)
    apa1 <- apa_print(model_object)

    expect_identical(
      object = apa1$estimate
      , expected = list(Intercept = "$b = 52.07$", N1 = "$b = 5.62$")
    )
    expect_identical(
      object = apa1$statistic
      , expected = list(Intercept = "$t(8.17) = 27.06$, $p = < .001$", N1 = "$t(17.00) = 3.06$, $p = .007$")
    )
    expect_identical(
      object = apa1$full_result
      , expected = list(
        Intercept = "$b = 52.07$, $t(8.17) = 27.06$, $p = < .001$"
        , N1 = "$b = 5.62$, $t(17.00) = 3.06$, $p = .007$"
      )
    )

    # model_object2 <- lmerTest::lmer(formula = yield ~ N + (1|block), data = npk)

  }
)
