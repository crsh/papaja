library("testthat")
source("../../R/apa_print.R")
source("../../R/apa_print_lm.R")
source("../../R/printnum.R")
source("../../R/utils.R")

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
    expect_that(length(lm_fit_output), equals(3))
    expect_that(names(lm_fit_output), equals(c("stat", "est", "full")))

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

    correct_output <- structure(list(stat = structure(list(Intercept = "$t(18) = 22.85$, $p < .001$",
      groupTrt = "$t(18) = -1.19$, $p = .249$", modelfit = structure(list(r2 = "$F(1,18) = 1.42$, $p = .249$"), .Names = "r2")),
      .Names = c("Intercept", "groupTrt", "modelfit")), est = structure(list(Intercept = "$b = 5.03$, 95% CI $[4.57$, $5.49]$",
      groupTrt = "$b = -0.37$, 95% CI $[-1.03$, $0.28]$", modelfit = structure(list(r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$",
      r2_adj = "$R^2_{adj} = .02$", aic = "$AIC = 46.18$", bic = "$AIC = 49.16$"), .Names = c("r2", "r2_adj", "aic", "bic"))),
      .Names = c("Intercept", "groupTrt", "modelfit")), full = structure(list(Intercept = "$b = 5.03$, 95% CI $[4.57$, $5.49]$, $t(18) = 22.85$, $p < .001$",
      groupTrt = "$b = -0.37$, 95% CI $[-1.03$, $0.28]$, $t(18) = -1.19$, $p = .249$", modelfit = structure(list(
      r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$, $F(1,18) = 1.42$, $p = .249$"), .Names = "r2")), .Names = c("Intercept", "groupTrt", "modelfit"))),
      .Names = c("stat", "est", "full")
    )

    expect_that(lm_fit_output, is_identical_to(correct_output))

    lm_fit_output <- apa_print(lm_fit, ci = matrix(c(1, 2), ncol = 2, nrow = 2, byrow = TRUE, dimnames = list(names(lm_fit$coefficients), c("2.5 %", "97.5 %"))))

    correct_output <- structure(list(stat = structure(list(Intercept = "$t(18) = 22.85$, $p < .001$",
      groupTrt = "$t(18) = -1.19$, $p = .249$", modelfit = structure(list(r2 = "$F(1,18) = 1.42$, $p = .249$"), .Names = "r2")),
      .Names = c("Intercept", "groupTrt", "modelfit")), est = structure(list(Intercept = "$b = 5.03$, 95% CI $[1.00$, $2.00]$",
      groupTrt = "$b = -0.37$, 95% CI $[1.00$, $2.00]$", modelfit = structure(list(r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$",
      r2_adj = "$R^2_{adj} = .02$", aic = "$AIC = 46.18$", bic = "$AIC = 49.16$"), .Names = c("r2", "r2_adj", "aic", "bic"))),
      .Names = c("Intercept", "groupTrt", "modelfit")), full = structure(list(Intercept = "$b = 5.03$, 95% CI $[1.00$, $2.00]$, $t(18) = 22.85$, $p < .001$",
      groupTrt = "$b = -0.37$, 95% CI $[1.00$, $2.00]$, $t(18) = -1.19$, $p = .249$", modelfit = structure(list(
      r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$, $F(1,18) = 1.42$, $p = .249$"), .Names = "r2")), .Names = c("Intercept", "groupTrt", "modelfit"))),
      .Names = c("stat", "est", "full")
    )

    expect_that(lm_fit_output, is_identical_to(correct_output))

    lm_fit_output <- apa_print(lm_fit, stat_name = "\\beta")

    correct_output <- structure(list(stat = structure(list(Intercept = "$t(18) = 22.85$, $p < .001$",
      groupTrt = "$t(18) = -1.19$, $p = .249$", modelfit = structure(list(r2 = "$F(1,18) = 1.42$, $p = .249$"), .Names = "r2")),
      .Names = c("Intercept", "groupTrt", "modelfit")), est = structure(list(Intercept = "$\\beta = 5.03$, 95% CI $[4.57$, $5.49]$",
      groupTrt = "$\\beta = -0.37$, 95% CI $[-1.03$, $0.28]$", modelfit = structure(list(r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$",
      r2_adj = "$R^2_{adj} = .02$", aic = "$AIC = 46.18$", bic = "$AIC = 49.16$"), .Names = c("r2", "r2_adj", "aic", "bic"))),
      .Names = c("Intercept", "groupTrt", "modelfit")), full = structure(list(Intercept = "$\\beta = 5.03$, 95% CI $[4.57$, $5.49]$, $t(18) = 22.85$, $p < .001$",
      groupTrt = "$\\beta = -0.37$, 95% CI $[-1.03$, $0.28]$, $t(18) = -1.19$, $p = .249$", modelfit = structure(list(
      r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$, $F(1,18) = 1.42$, $p = .249$"), .Names = "r2")), .Names = c("Intercept", "groupTrt", "modelfit"))),
      .Names = c("stat", "est", "full")
    )

    expect_that(lm_fit_output, is_identical_to(correct_output))

    lm_fit_output <- apa_print(lm_fit, in_paren = TRUE)

    correct_output <- structure(list(stat = structure(list(Intercept = "$t[18] = 22.85$, $p < .001$",
      groupTrt = "$t[18] = -1.19$, $p = .249$", modelfit = structure(list(r2 = "$F[1,18] = 1.42$, $p = .249$"), .Names = "r2")),
      .Names = c("Intercept", "groupTrt", "modelfit")), est = structure(list(Intercept = "$b = 5.03$, 95% CI $[4.57$, $5.49]$",
      groupTrt = "$b = -0.37$, 95% CI $[-1.03$, $0.28]$", modelfit = structure(list(r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$",
      r2_adj = "$R^2_{adj} = .02$", aic = "$AIC = 46.18$", bic = "$AIC = 49.16$"), .Names = c("r2", "r2_adj", "aic", "bic"))),
      .Names = c("Intercept", "groupTrt", "modelfit")), full = structure(list(Intercept = "$b = 5.03$, 95% CI $[4.57$, $5.49]$, $t[18] = 22.85$, $p < .001$",
      groupTrt = "$b = -0.37$, 95% CI $[-1.03$, $0.28]$, $t[18] = -1.19$, $p = .249$", modelfit = structure(list(
      r2 = "$R^2 = .07$, 90% CI $[0.00$, $0.29]$, $F[1,18] = 1.42$, $p = .249$"), .Names = "r2")), .Names = c("Intercept", "groupTrt", "modelfit"))),
      .Names = c("stat", "est", "full")
    )

    expect_that(lm_fit_output, is_identical_to(correct_output))

    trt <- rep(trt, 2)
    ctl <- rep(ctl, 2)
    lm_fit <- lm(scale(trt) ~ scale(ctl))
    lm_fit_output <- apa_print(lm_fit, standardized = TRUE)

    correct_output <- structure(list(stat = structure(list(Intercept = "$t(18) = 0.00$, $p > .999$",
      z_ctl = "$t(18) = -2.18$, $p = .042$", modelfit = structure(list(
      r2 = "$F(1,18) = 4.77$, $p = .042$"), .Names = "r2")), .Names = c("Intercept",
      "z_ctl", "modelfit")), est = structure(list(Intercept = "$b^* = .00$, 95% CI $[-.43$, $.43]$",
      z_ctl = "$b^* = -.46$, 95% CI $[-.90$, $-.02]$", modelfit = structure(list(
      r2 = "$R^2 = .21$, 90% CI $[0.00$, $0.43]$", r2_adj = "$R^2_{adj} = .17$",
      aic = "$AIC = 57.03$", bic = "$AIC = 60.02$"), .Names = c("r2",
      "r2_adj", "aic", "bic"))), .Names = c("Intercept", "z_ctl",
      "modelfit")), full = structure(list(Intercept = "$b^* = .00$, 95% CI $[-.43$, $.43]$, $t(18) = 0.00$, $p > .999$",
      z_ctl = "$b^* = -.46$, 95% CI $[-.90$, $-.02]$, $t(18) = -2.18$, $p = .042$",
      modelfit = structure(list(r2 = "$R^2 = .21$, 90% CI $[0.00$, $0.43]$, $F(1,18) = 4.77$, $p = .042$"), .Names = "r2")), .Names = c("Intercept",
      "z_ctl", "modelfit"))), .Names = c("stat", "est", "full")
    )

    expect_that(lm_fit_output, is_identical_to(correct_output))

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
  }
)
