context("apa_print.lm()")

test_that(
  "Linear regression: lm()-fit"
  , {
    ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
    trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
    group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
    weight <- c(ctl, trt)
    lm_fit <- lm(weight ~ group)

    lm_fit_output <- apa_print(lm_fit)

    expect_is(lm_fit_output, "list")
    expect_equal(length(lm_fit_output), 4)
    expect_equal(names(lm_fit_output), c("stat", "est", "full", "table"))

    # stat
    expect_is(lm_fit_output$stat, "list")
    expect_equal(length(lm_fit_output$stat), 3)
    expect_equal(names(lm_fit_output$stat), c("Intercept", "groupTrt", "modelfit"))
    expect_is(lm_fit_output$stat$Intercept, "character")
    expect_is(lm_fit_output$stat$groupTrt, "character")
    expect_is(lm_fit_output$stat$modelfit, "list")
    expect_equal(length(lm_fit_output$stat$modelfit), 1)
    expect_equal(names(lm_fit_output$stat$modelfit), "r2")
    expect_is(lm_fit_output$stat$modelfit$r2, "character")

    expect_equal(lm_fit_output$stat$Intercept, "$t(18) = 22.85$, $p < .001$")
    expect_equal(lm_fit_output$stat$groupTrt, "$t(18) = -1.19$, $p = .249$")
    expect_equal(lm_fit_output$stat$modelfit$r2, "$F(1, 18) = 1.42$, $p = .249$")

    # est
    expect_is(lm_fit_output$est, "list")
    expect_equal(length(lm_fit_output$est), 3)
    expect_equal(names(lm_fit_output$est), c("Intercept", "groupTrt", "modelfit"))
    expect_is(lm_fit_output$est$Intercept, "character")
    expect_is(lm_fit_output$est$groupTrt, "character")
    expect_is(lm_fit_output$est$modelfit, "list")
    expect_equal(length(lm_fit_output$est$modelfit), 4)
    expect_equal(names(lm_fit_output$est$modelfit), c("r2", "r2_adj", "aic", "bic"))
    expect_true(all(sapply(lm_fit_output$est$modelfit, is.character)))

    expect_equal(lm_fit_output$est$Intercept, "$b = 5.03$, 95\\% CI $[4.57$, $5.49]$")
    expect_equal(lm_fit_output$est$groupTrt, "$b = -0.37$, 95\\% CI $[-1.03$, $0.28]$")
    expect_equal(lm_fit_output$est$modelfit$r2, "$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$")
    expect_equal(lm_fit_output$est$modelfit$r2_adj, "$R^2_{adj} = .02$")
    expect_equal(lm_fit_output$est$modelfit$aic, "$AIC = 46.18$")
    expect_equal(lm_fit_output$est$modelfit$bic, "$BIC = 49.16$")

    # full
    expect_is(lm_fit_output$full, "list")
    expect_equal(length(lm_fit_output$full), 3)
    expect_equal(names(lm_fit_output$full), c("Intercept", "groupTrt", "modelfit"))
    expect_is(lm_fit_output$full$Intercept, "character")
    expect_is(lm_fit_output$full$groupTrt, "character")
    expect_is(lm_fit_output$full$modelfit, "list")
    expect_equal(length(lm_fit_output$full$modelfit), 1)
    expect_equal(names(lm_fit_output$full$modelfit), "r2")
    expect_is(lm_fit_output$full$modelfit$r2, "character")

    expect_equal(lm_fit_output$full$Intercept, "$b = 5.03$, 95\\% CI $[4.57$, $5.49]$, $t(18) = 22.85$, $p < .001$")
    expect_equal(lm_fit_output$full$groupTrt, "$b = -0.37$, 95\\% CI $[-1.03$, $0.28]$, $t(18) = -1.19$, $p = .249$")
    expect_equal(lm_fit_output$full$modelfit$r2, "$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$, $F(1, 18) = 1.42$, $p = .249$")

    # table
    expect_is(lm_fit_output$table, "data.frame")
    expect_equal(nrow(lm_fit_output$table), 2)
    expect_equal(colnames(lm_fit_output$table), c("Predictor", "$b$", "95\\% CI", "$t(18)$", "$p$"))

    # Manual CI
    lm_fit_output <- apa_print(lm_fit, ci = matrix(c(1, 2), ncol = 2, nrow = 2, byrow = TRUE, dimnames = list(names(lm_fit$coefficients), c("2.5 \\%", "97.5 \\%"))))
    expect_equal(lm_fit_output$full$Intercept, "$b = 5.03$, 95\\% CI $[1.00$, $2.00]$, $t(18) = 22.85$, $p < .001$")
    expect_equal(lm_fit_output$full$groupTrt, "$b = -0.37$, 95\\% CI $[1.00$, $2.00]$, $t(18) = -1.19$, $p = .249$")
    expect_equal(lm_fit_output$full$modelfit$r2, "$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$, $F(1, 18) = 1.42$, $p = .249$")

    # Set name of estimate
    lm_fit_output <- apa_print(lm_fit, est_name = "\\beta")
    expect_equal(lm_fit_output$est$Intercept, "$\\beta = 5.03$, 95\\% CI $[4.57$, $5.49]$")
    expect_equal(lm_fit_output$est$groupTrt, "$\\beta = -0.37$, 95\\% CI $[-1.03$, $0.28]$")
    expect_equal(colnames(lm_fit_output$table), c("Predictor", "$\\beta$", "95\\% CI", "$t(18)$", "$p$"))

    # Standardized regression coefficients
    trt <- rep(trt, 2)
    ctl <- rep(ctl, 2)
    lm_fit <- lm(scale(trt) ~ scale(ctl))
    lm_fit_output <- apa_print(lm_fit, standardized = TRUE)

    expect_equal(lm_fit_output$full$Intercept, "$b^* = .00$, 95\\% CI $[-.43$, $.43]$, $t(18) = 0.00$, $p > .999$")
    expect_equal(lm_fit_output$full$z_ctl, "$b^* = -.46$, 95\\% CI $[-.90$, $-.02]$, $t(18) = -2.18$, $p = .042$")
    expect_equal(colnames(lm_fit_output$table), c("Predictor", "$b^*$", "95\\% CI", "$t(18)$", "$p$"))

    # No CI information
    expect_error(apa_print(lm_fit, ci = NULL), "The parameter 'ci' is NULL.")
  }
)


context("apa_print.summary.lm()")

test_that(
  "Linear regression: summary(lm())"
  , {
    ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
    trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
    group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
    weight <- c(ctl, trt)

    lm_fit <- lm(weight ~ group)
    lm_fit_output <- apa_print(lm_fit)

    lm_summary <- summary(lm_fit)
    lm_summary_output <- apa_print(lm_summary)

    expect_identical(lm_summary_output, lm_fit_output)

    lm_fit_output <- apa_print(lm_fit, digits = 0)
    expect_equal(lm_fit_output$est$Intercept, "$b = 5$, 95\\% CI $[5$, $5]$")
  }
)
