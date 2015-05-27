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

    # table
    expect_that(lm_fit_output$table, is_a("data.frame"))
    expect_that(nrow(lm_fit_output$table), equals(2))
    expect_that(colnames(lm_fit_output$table), equals(c("Term", "$b$", "95\\% CI", "$t$", "$df$", "$p$")))

    load("data/lm_fit_output1.Rdata")
    expect_that(lm_fit_output, is_identical_to(correct_output))

    lm_fit_output <- apa_print(lm_fit, ci = matrix(c(1, 2), ncol = 2, nrow = 2, byrow = TRUE, dimnames = list(names(lm_fit$coefficients), c("2.5 \\%", "97.5 \\%"))))
    load("data/lm_fit_output2.Rdata")
    expect_that(lm_fit_output, is_identical_to(correct_output))

    lm_fit_output <- apa_print(lm_fit, est_name = "\\beta")
    load("data/lm_fit_output3.Rdata")
    expect_that(lm_fit_output, is_identical_to(correct_output))

    lm_fit_output <- apa_print(lm_fit, in_paren = TRUE)
    load("data/lm_fit_output4.Rdata")
    expect_that(lm_fit_output, is_identical_to(correct_output))

    trt <- rep(trt, 2)
    ctl <- rep(ctl, 2)
    lm_fit <- lm(scale(trt) ~ scale(ctl))
    lm_fit_output <- apa_print(lm_fit, standardized = TRUE)

    load("data/lm_fit_output5.Rdata")
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

    lm_fit_output <- apa_print(lm_fit, digits = 0)
    expect_that(lm_fit_output$est$Intercept, equals("$b = 5$, 95\\% CI $[5$, $5]$"))
  }
)
