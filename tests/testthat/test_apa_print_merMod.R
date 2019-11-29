context("apa_print() for hierarchical linear models")

test_that(
  "first few tests"
  , {
    testthat::skip_on_travis()
    model_lme4 <- lme4::lmer(formula = yield ~ N + (1|block), data = npk)
    model_lmerTest <- lmerTest::as_lmerModLmerTest(model_lme4)

    apa_lme4 <- apa_print(model_lme4)
    apa_lmerTest <- apa_print(model_lmerTest)

    expect_identical(
      object = apa_lmerTest$estimate
      , expected = list(
        Intercept = "$b = 52.07$, 95\\% CI $[48.17$, $55.97]$"
        , N1 = "$b = 5.62$, 95\\% CI $[1.92$, $9.31]$"
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
        Intercept = "$b = 52.07$, 95\\% CI $[48.17$, $55.97]$, $t(8.17) = 27.06$, $p < .001$"
        , N1 = "$b = 5.62$, 95\\% CI $[1.92$, $9.31]$, $t(17.00) = 3.06$, $p = .007$"
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
        Intercept = "$b = 52.07$, 95\\% CI $[48.17$, $55.97]$, $t = 27.06$"
        , N1 = "$b = 5.62$, 95\\% CI $[1.92$, $9.31]$, $t = 3.06$"
      )
    )


  }
)
