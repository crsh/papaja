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

    expect_that(lm_fit_output, is_a("list"))
    expect_that(length(lm_fit_output), equals(4))
    expect_that(names(lm_fit_output), equals(c("stat", "est", "full", "table")))

    # stat
    expect_that(lm_fit_output$stat, is_a("list"))
    expect_that(length(lm_fit_output$stat), equals(3))
    expect_that(names(lm_fit_output$stat), equals(c("Intercept", "groupTrt", "modelfit")))
    expect_that(lm_fit_output$stat$Intercept, is_a("character"))
    expect_that(lm_fit_output$stat$groupTrt, is_a("character"))
    expect_that(lm_fit_output$stat$modelfit, is_a("list"))
    expect_that(length(lm_fit_output$stat$modelfit), equals(1))
    expect_that(names(lm_fit_output$stat$modelfit), equals("r2"))
    expect_that(lm_fit_output$stat$modelfit$r2, is_a("character"))

    expect_that(lm_fit_output$stat$Intercept, equals("$t(18) = 22.85$, $p < .001$"))
    expect_that(lm_fit_output$stat$groupTrt, equals("$t(18) = -1.19$, $p = .249$"))
    expect_that(lm_fit_output$stat$modelfit$r2, equals("$F(1, 18) = 1.42$, $p = .249$"))

    # est
    expect_that(lm_fit_output$est, is_a("list"))
    expect_that(length(lm_fit_output$est), equals(3))
    expect_that(names(lm_fit_output$est), equals(c("Intercept", "groupTrt", "modelfit")))
    expect_that(lm_fit_output$est$Intercept, is_a("character"))
    expect_that(lm_fit_output$est$groupTrt, is_a("character"))
    expect_that(lm_fit_output$est$modelfit, is_a("list"))
    expect_that(length(lm_fit_output$est$modelfit), equals(4))
    expect_that(names(lm_fit_output$est$modelfit), equals(c("r2", "r2_adj", "aic", "bic")))
    expect_that(all(unlist(lapply(lm_fit_output$est$modelfit, is.character))), is_true())

    expect_that(lm_fit_output$est$Intercept, equals("$b = 5.03$, 95\\% CI $[4.57$, $5.49]$"))
    expect_that(lm_fit_output$est$groupTrt, equals("$b = -0.37$, 95\\% CI $[-1.03$, $0.28]$"))
    expect_that(lm_fit_output$est$modelfit$r2, equals("$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$"))
    expect_that(lm_fit_output$est$modelfit$r2_adj, equals("$R^2_{adj} = .02$"))
    expect_that(lm_fit_output$est$modelfit$aic, equals("$AIC = 46.18$"))
    expect_that(lm_fit_output$est$modelfit$bic, equals("$BIC = 49.16$"))

    # full
    expect_that(lm_fit_output$full, is_a("list"))
    expect_that(length(lm_fit_output$full), equals(3))
    expect_that(names(lm_fit_output$full), equals(c("Intercept", "groupTrt", "modelfit")))
    expect_that(lm_fit_output$full$Intercept, is_a("character"))
    expect_that(lm_fit_output$full$groupTrt, is_a("character"))
    expect_that(lm_fit_output$full$modelfit, is_a("list"))
    expect_that(length(lm_fit_output$full$modelfit), equals(1))
    expect_that(names(lm_fit_output$full$modelfit), equals("r2"))
    expect_that(lm_fit_output$full$modelfit$r2, is_a("character"))

    expect_that(lm_fit_output$full$Intercept, equals("$b = 5.03$, 95\\% CI $[4.57$, $5.49]$, $t(18) = 22.85$, $p < .001$"))
    expect_that(lm_fit_output$full$groupTrt, equals("$b = -0.37$, 95\\% CI $[-1.03$, $0.28]$, $t(18) = -1.19$, $p = .249$"))
    expect_that(lm_fit_output$full$modelfit$r2, equals("$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$, $F(1, 18) = 1.42$, $p = .249$"))

    # table
    expect_that(lm_fit_output$table, is_a("data.frame"))
    expect_that(nrow(lm_fit_output$table), equals(2))
    expect_that(colnames(lm_fit_output$table), equals(c("Predictor", "$b$", "95\\% CI", "$t(18)$", "$p$")))

    # Manual CI
    lm_fit_output <- apa_print(lm_fit, ci = matrix(c(1, 2), ncol = 2, nrow = 2, byrow = TRUE, dimnames = list(names(lm_fit$coefficients), c("2.5 \\%", "97.5 \\%"))))
    expect_that(lm_fit_output$full$Intercept, equals("$b = 5.03$, 95\\% CI $[1.00$, $2.00]$, $t(18) = 22.85$, $p < .001$"))
    expect_that(lm_fit_output$full$groupTrt, equals("$b = -0.37$, 95\\% CI $[1.00$, $2.00]$, $t(18) = -1.19$, $p = .249$"))
    expect_that(lm_fit_output$full$modelfit$r2, equals("$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$, $F(1, 18) = 1.42$, $p = .249$"))

    # Set name of estimate
    lm_fit_output <- apa_print(lm_fit, est_name = "\\beta")
    expect_that(lm_fit_output$est$Intercept, equals("$\\beta = 5.03$, 95\\% CI $[4.57$, $5.49]$"))
    expect_that(lm_fit_output$est$groupTrt, equals("$\\beta = -0.37$, 95\\% CI $[-1.03$, $0.28]$"))
    expect_that(lm_fit_output$est$modelfit$r2, equals("$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$"))
    expect_that(lm_fit_output$est$modelfit$r2_adj, equals("$R^2_{adj} = .02$"))
    expect_that(lm_fit_output$est$modelfit$aic, equals("$AIC = 46.18$"))
    expect_that(lm_fit_output$est$modelfit$bic, equals("$BIC = 49.16$"))
    expect_that(colnames(lm_fit_output$table), equals(c("Predictor", "$\\beta$", "95\\% CI", "$t(18)$", "$p$")))

    # In parentheses
    lm_fit_output <- apa_print(lm_fit, in_paren = TRUE)
    expect_that(lm_fit_output$full$Intercept, equals("$b = 5.03$, 95\\% CI $[4.57$, $5.49]$, $t[18] = 22.85$, $p < .001$"))
    expect_that(lm_fit_output$full$groupTrt, equals("$b = -0.37$, 95\\% CI $[-1.03$, $0.28]$, $t[18] = -1.19$, $p = .249$"))
    expect_that(lm_fit_output$full$modelfit$r2, equals("$R^2 = .07$, 90\\% CI $[0.00$, $0.29]$, $F[1, 18] = 1.42$, $p = .249$"))
    expect_that(colnames(lm_fit_output$table), equals(c("Predictor", "$b$", "95\\% CI", "$t(18)$", "$p$")))

    # Standardized regression coefficients
    trt <- rep(trt, 2)
    ctl <- rep(ctl, 2)
    lm_fit <- lm(scale(trt) ~ scale(ctl))
    lm_fit_output <- apa_print(lm_fit, standardized = TRUE)

    expect_that(lm_fit_output$full$Intercept, equals("$b^* = .00$, 95\\% CI $[-.43$, $.43]$, $t(18) = 0.00$, $p > .999$"))
    expect_that(lm_fit_output$full$z_ctl, equals("$b^* = -.46$, 95\\% CI $[-.90$, $-.02]$, $t(18) = -2.18$, $p = .042$"))
    expect_that(colnames(lm_fit_output$table), equals(c("Predictor", "$b^*$", "95\\% CI", "$t(18)$", "$p$")))

    # No CI information
    expect_that(apa_print(lm_fit, ci = NULL), throws_error("The parameter 'ci' is NULL."))
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

    expect_that(lm_summary_output, is_identical_to(lm_fit_output))

    lm_fit_output <- apa_print(lm_fit, digits = 0)
    expect_that(lm_fit_output$est$Intercept, equals("$b = 5$, 95\\% CI $[5$, $5]$"))
  }
)
