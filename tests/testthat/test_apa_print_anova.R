context("apa_print() for ANOVA")

# Data and examples from http://personality-project.org/r/r.guide.html#anova

# Use our own effect-size function for these tests
# Custom effect sizes via the 'effectsize' package are tested elsewhere
op <- options(papaja.estimate_anova = "ges", papaja.mse = TRUE)

test_that(
  "One-way between ANOVA"
  , {
    load("data/ow_data.rdata")
    ow_aov <- aov(Alertness ~ Dosage, data = ow_data)
    ow_aov_output <- apa_print(ow_aov)

    expect_apa_results(
      ow_aov_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )

    # stat
    expect_identical(names(ow_aov_output$stat), "Dosage")
    expect_identical(ow_aov_output$stat$Dosage, "$F(2, 15) = 8.79$, $\\mathit{MSE} = 24.25$, $p = .003$")

    # est
    expect_identical(names(ow_aov_output$est), "Dosage")
    expect_identical(ow_aov_output$est$Dosage, "$\\hat{\\eta}^2_G = .540$")

    # full
    expect_identical(names(ow_aov_output$full), "Dosage")
    expect_identical(ow_aov_output$full$Dosage, "$F(2, 15) = 8.79$, $\\mathit{MSE} = 24.25$, $p = .003$, $\\hat{\\eta}^2_G = .540$")

    # table
    expect_identical(nrow(ow_aov_output$table), 1L)

    # Other classes
    ow_aov_summary_output <- apa_print(summary(ow_aov))
    expect_identical(ow_aov_summary_output, ow_aov_output)

    ow_aov_Anova_output <- apa_print(car::Anova(ow_aov))
    expect_identical(ow_aov_Anova_output, ow_aov_output)

    ow_afex_data <- cbind(id = 1:nrow(ow_data), ow_data)
    ow_afex_aov <- afex::aov_ez(
      data = ow_afex_data
      , id = "id"
      , dv = "Alertness"
      , between = "Dosage"
    )

    ow_afex_aov_output <- apa_print(ow_afex_aov$aov)
    expect_identical(ow_afex_aov_output, ow_aov_output)

    ow_afex_aov_output <- apa_print(ow_afex_aov$Anova)
    expect_identical(ow_afex_aov_output, ow_aov_output)

    ow_afex_aov_output <- apa_print(ow_afex_aov)
    expect_identical(ow_afex_aov_output, ow_aov_output)

    # With intercept
    ow_afex_aov_output <- apa_print(ow_afex_aov$Anova, intercept = TRUE)

    expect_apa_results(
      ow_afex_aov_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )

    # stat
    expect_identical(names(ow_afex_aov_output$stat), c("Intercept", "Dosage"))
    expect_is(ow_afex_aov_output$stat$Intercept, "character")
    expect_identical(ow_afex_aov_output$stat$Intercept, "$F(1, 15) = 487.23$, $\\mathit{MSE} = 24.25$, $p < .001$")

    # est
    expect_identical(names(ow_afex_aov_output$est), c("Intercept", "Dosage"))
    expect_is(ow_afex_aov_output$est$Intercept, "character")
    expect_identical(ow_afex_aov_output$est$Intercept, "$\\hat{\\eta}^2_G = .970$")

    # full
    expect_identical(names(ow_afex_aov_output$full), c("Intercept", "Dosage"))
    expect_is(ow_afex_aov_output$full$Intercept, "character")
    expect_identical(ow_afex_aov_output$full$Intercept, "$F(1, 15) = 487.23$, $\\mathit{MSE} = 24.25$, $p < .001$, $\\hat{\\eta}^2_G = .970$")

    # table
    expect_identical(nrow(ow_afex_aov_output$table), 2L)


    expect_identical(
      ow_afex_aov_output$table$term
      , structure(
        c("Intercept", "Dosage")
        , class = c("tiny_labelled", "character")
        , label = "Effect"
      )
    )

    ow_afex_aov_output2 <- apa_print(ow_afex_aov, intercept = TRUE)
    expect_identical(ow_afex_aov_output2, ow_afex_aov_output)


    # Other effect sizes
    ow_aov_output <- apa_print(ow_aov, es = "pes")
    expect_identical(ow_aov_output$est$Dosage, "$\\hat{\\eta}^2_p = .540$")

    ow_aov_output <- apa_print(ow_aov, es = c("ges"))
    expect_apa_results(ow_aov_output)
    expect_identical(ow_aov_output$est$Dosage, "$\\hat{\\eta}^2_G = .540$")
  }
)


test_that(
  "Two-way between ANOVA"
  , {
    load("data/tw_data.rdata")
    tw_aov <- aov(Alertness ~ Gender * Dosage, tw_data)
    tw_aov_output <- apa_print(tw_aov)
    expect_apa_results(
      tw_aov_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )

    # stat
    expect_identical(names(tw_aov_output$stat), c("Gender", "Dosage", "Gender_Dosage"))
    expect_identical(tw_aov_output$stat$Gender, "$F(1, 12) = 2.95$, $\\mathit{MSE} = 25.94$, $p = .111$")
    expect_identical(tw_aov_output$stat$Dosage, "$F(1, 12) = 0.20$, $\\mathit{MSE} = 25.94$, $p = .666$")
    expect_identical(tw_aov_output$stat$Gender_Dosage, "$F(1, 12) = 0.00$, $\\mathit{MSE} = 25.94$, $p = .962$")

    # est
    expect_identical(names(tw_aov_output$est), c("Gender", "Dosage", "Gender_Dosage"))
    expect_identical(tw_aov_output$est$Gender, "$\\hat{\\eta}^2_G = .197$")
    expect_identical(tw_aov_output$est$Dosage, "$\\hat{\\eta}^2_G = .016$")
    expect_identical(tw_aov_output$est$Gender_Dosage, "$\\hat{\\eta}^2_G = .000$")

    # full
    expect_identical(names(tw_aov_output$full), c("Gender", "Dosage", "Gender_Dosage"))
    expect_identical(tw_aov_output$full$Gender, "$F(1, 12) = 2.95$, $\\mathit{MSE} = 25.94$, $p = .111$, $\\hat{\\eta}^2_G = .197$")
    expect_identical(tw_aov_output$full$Dosage, "$F(1, 12) = 0.20$, $\\mathit{MSE} = 25.94$, $p = .666$, $\\hat{\\eta}^2_G = .016$")
    expect_identical(tw_aov_output$full$Gender_Dosage, "$F(1, 12) = 0.00$, $\\mathit{MSE} = 25.94$, $p = .962$, $\\hat{\\eta}^2_G = .000$")

    # table
    expect_identical(nrow(tw_aov_output$table), 3L)
    expect_identical(
      tw_aov_output$table$term
      , expected = structure(
        c("Gender", "Dosage", "Gender $\\times$ Dosage")
        , label = "Effect"
        , class = c("tiny_labelled", "character")
      )
    )

    # Other classes
    tw_aov_Anova_output <- apa_print(car::Anova(tw_aov))
    expect_identical(tw_aov_Anova_output, tw_aov_output)

    tw_aov_summary_output <- apa_print(summary(tw_aov))
    expect_identical(tw_aov_summary_output, tw_aov_output)


    tw_afex_data <- cbind(id = 1:nrow(tw_data), tw_data)
    tw_afex_aov <- afex::aov_ez(
      data = tw_afex_data
      , id = "id"
      , dv = "Alertness"
      , between = c("Gender", "Dosage")
    )

    tw_afex_aov_output <- apa_print(tw_afex_aov$aov)
    expect_identical(tw_afex_aov_output, tw_aov_output)

    # With intercept
    tw_afex_aov_output <- apa_print(tw_afex_aov$Anova, intercept = TRUE)

    expect_apa_results(
      tw_afex_aov_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )

    ## stat
    expect_identical(names(tw_afex_aov_output$stat), c("Intercept", "Gender", "Dosage", "Gender_Dosage"))
    expect_identical(tw_afex_aov_output$stat$Intercept, "$F(1, 12) = 121.99$, $\\mathit{MSE} = 25.94$, $p < .001$")

    ## est
    expect_identical(names(tw_afex_aov_output$est), c("Intercept", "Gender", "Dosage", "Gender_Dosage"))
    expect_identical(tw_afex_aov_output$est$Intercept, "$\\hat{\\eta}^2_G = .910$")

    ## full
    expect_identical(names(tw_afex_aov_output$full), c("Intercept", "Gender", "Dosage", "Gender_Dosage"))
    expect_identical(tw_afex_aov_output$full$Intercept, "$F(1, 12) = 121.99$, $\\mathit{MSE} = 25.94$, $p < .001$, $\\hat{\\eta}^2_G = .910$")

    ## table
    expect_identical(nrow(tw_afex_aov_output$table), 4L)
    expect_identical(
      object = tw_afex_aov_output$table$term
      , expected = structure(
        c("Intercept", "Gender", "Dosage", "Gender $\\times$ Dosage")
        , label = "Effect"
        , class = c("tiny_labelled", "character")
      )
    )

    tw_afex_aov_output2 <- apa_print(tw_afex_aov, intercept = TRUE)
    expect_identical(tw_afex_aov_output2, tw_afex_aov_output)

    # Observed effects
    tw_rm_aov_output <- apa_print(tw_aov, observed = "Gender")

    expect_identical(tw_rm_aov_output$est$Gender, "$\\hat{\\eta}^2_G = .197$")
    expect_identical(tw_rm_aov_output$est$Dosage, "$\\hat{\\eta}^2_G = .013$")
    expect_identical(tw_rm_aov_output$est$Gender_Dosage, "$\\hat{\\eta}^2_G = .000$")
  }
)


test_that(
  "One-way repeated-measures ANOVA"
  , {
    options(papaja.estimate_anova = "ges")
    load("data/rm_data.rdata")
    rm_aov <- aov(Recall ~ Valence + Error(Subject/Valence), rm_data)
    rm_aov_output <- apa_print(rm_aov)

    expect_apa_results(
      rm_aov_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )

    # stat
    expect_identical(names(rm_aov_output$stat), "Valence")
    expect_identical(rm_aov_output$stat$Valence, "$F(2, 8) = 189.11$, $\\mathit{MSE} = 5.37$, $p < .001$")

    # est
    expect_identical(names(rm_aov_output$est), "Valence")
    expect_identical(rm_aov_output$est$Valence, "$\\hat{\\eta}^2_G = .932$")

    # full
    expect_identical(names(rm_aov_output$full), "Valence")
    expect_identical(rm_aov_output$full$Valence, "$F(2, 8) = 189.11$, $\\mathit{MSE} = 5.37$, $p < .001$, $\\hat{\\eta}^2_G = .932$")

    # Other classes
    rm_aov_summary_output <- apa_print(summary(rm_aov))
    expect_identical(rm_aov_summary_output, rm_aov_output)

    rm_afex_aov <- afex::aov_ez(
      data = rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = "Valence"
    )

    rm_afex_aov_output <- apa_print(rm_afex_aov$aov)
    expect_identical(rm_afex_aov_output, rm_aov_output)

    rm_afex_aov_output <- apa_print(rm_afex_aov$Anova, correction = "none", intercept = FALSE)
    expect_identical(rm_afex_aov_output, rm_aov_output)

    rm_afex_aov_output <- apa_print(rm_afex_aov, correction = "none", intercept = FALSE)
    expect_identical(rm_afex_aov_output, rm_aov_output)


    # DF corrections
    rm_afex_anova.mlm_output <- apa_print(rm_afex_aov$Anova, correction = "GG")
    expect_apa_results(
      rm_afex_anova.mlm_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}^{\\mathrm{GG}}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{GG}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )
    expect_identical(rm_afex_anova.mlm_output$full$Valence, "$F(1.15, 4.60) = 189.11$, $\\mathit{MSE} = 9.34$, $p < .001$, $\\hat{\\eta}^2_G = .932$")

    rm_afex_anova.mlm_output <- apa_print(rm_afex_aov$Anova, correction = "HF")
    expect_apa_results(
      rm_afex_anova.mlm_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}^{\\mathrm{HF}}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{HF}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )
    expect_identical(rm_afex_anova.mlm_output$full$Valence, "$F(1.32, 5.26) = 189.11$, $\\mathit{MSE} = 8.16$, $p < .001$, $\\hat{\\eta}^2_G = .932$")

    rm_afex_anova.mlm_output <- apa_print(rm_afex_aov$Anova, correction = "none")
    expect_identical(rm_afex_anova.mlm_output$full$Valence, rm_aov_output$full$Valence)
  }
)


test_that(
  "Two-way repeated-measures ANOVA"
  , {
    load("data/tw_rm_data.rdata")
    tw_rm_aov <- aov(Recall~ (Task * Valence) + Error(Subject/(Task * Valence)), tw_rm_data)
    tw_rm_aov_output <- apa_print(tw_rm_aov)

    expect_apa_results(
      tw_rm_aov_output
      , labels = list(
        term          = "Effect"
        , estimate    = "$\\hat{\\eta}^2_G$"
        , statistic   = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , mse         = "$\\mathit{MSE}$"
        , p.value     = "$p$"
      )
    )

    # stat
    expect_identical(names(tw_rm_aov_output$stat), c("Task", "Valence", "Task_Valence"))
    expect_identical(tw_rm_aov_output$stat$Task, "$F(1, 4) = 7.35$, $\\mathit{MSE} = 4.08$, $p = .054$")
    expect_identical(tw_rm_aov_output$stat$Valence, "$F(2, 8) = 1.46$, $\\mathit{MSE} = 3.36$, $p = .288$")
    expect_identical(tw_rm_aov_output$stat$Task_Valence, "$F(2, 8) = 0.29$, $\\mathit{MSE} = 2.41$, $p = .755$")

    # est
    expect_identical(names(tw_rm_aov_output$est), c("Task", "Valence", "Task_Valence"))
    expect_identical(tw_rm_aov_output$est$Task, "$\\hat{\\eta}^2_G = .068$")
    expect_identical(tw_rm_aov_output$est$Valence, "$\\hat{\\eta}^2_G = .023$")
    expect_identical(tw_rm_aov_output$est$Task_Valence, "$\\hat{\\eta}^2_G = .003$")

    # full
    expect_identical(names(tw_rm_aov_output$full), c("Task", "Valence", "Task_Valence"))
    expect_identical(tw_rm_aov_output$full$Task, "$F(1, 4) = 7.35$, $\\mathit{MSE} = 4.08$, $p = .054$, $\\hat{\\eta}^2_G = .068$")
    expect_identical(tw_rm_aov_output$full$Valence, "$F(2, 8) = 1.46$, $\\mathit{MSE} = 3.36$, $p = .288$, $\\hat{\\eta}^2_G = .023$")
    expect_identical(tw_rm_aov_output$full$Task_Valence, "$F(2, 8) = 0.29$, $\\mathit{MSE} = 2.41$, $p = .755$, $\\hat{\\eta}^2_G = .003$")

    # Other classes
    tw_rm_aov_summary_output <- apa_print(summary(tw_rm_aov))
    expect_identical(tw_rm_aov_summary_output, tw_rm_aov_output)

    tw_rm_afex_aov <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
    ))

    tw_rm_afex_aov_output <- apa_print(tw_rm_afex_aov$aov)
    expect_identical(tw_rm_afex_aov_output, tw_rm_aov_output)

    tw_rm_afex_aov_output <- apa_print(tw_rm_afex_aov$Anova, correction = "none")
    expect_identical(tw_rm_afex_aov_output, tw_rm_aov_output)

    tw_rm_afex_aov_output <- suppressWarnings(apa_print(tw_rm_afex_aov, correction = "none"))
    expect_identical(tw_rm_afex_aov_output, tw_rm_aov_output)

    # Observed
    tw_rm_aov_output <- apa_print(tw_rm_aov, observed = "Task")

    expect_identical(tw_rm_aov_output$est$Task, "$\\hat{\\eta}^2_G = .068$")
    expect_identical(tw_rm_aov_output$est$Valence, "$\\hat{\\eta}^2_G = .022$")
    expect_identical(tw_rm_aov_output$est$Task_Valence, "$\\hat{\\eta}^2_G = .003$")
  }
)

test_that(
  "Levene test"
  , {
    load("data/ow_data.rdata")

    levene_test <- car::leveneTest(Alertness ~ Dosage, data = ow_data)
    levene_test_output <- apa_print(levene_test)

    expect_apa_results(
      levene_test_output
      , labels = list(
        statistic     = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value     = "$p$"
      )
    )
    expect_identical(levene_test_output$stat, "$F(2, 15) = 4.17$, $p = .036$")
  }
)

test_that(
  "anova(lm(...))"
  , {
    model <- anova(lm(yield ~ N, data = npk))
    apa_out <- apa_print(model)
    expect_identical(
      apa_out$table
      , expected = structure(
        list(
          term = structure("N", label = "Effect", class = c("tiny_labelled", "character"))
          , estimate = structure(".216", label = "$\\hat{\\eta}^2_G$", class = c("tiny_labelled","character"))
          , statistic = structure("6.06", label = "$F$", class = c("tiny_labelled", "character"))
          , df = structure("1", label = "$\\mathit{df}$", class = c("tiny_labelled", "character"))
          , df.residual = structure("22", label = "$\\mathit{df}_{\\mathrm{res}}$", class = c("tiny_labelled", "character"))
          , mse = structure("31.23", label = "$\\mathit{MSE}$", class = c("tiny_labelled", "character"))
          , p.value = structure(".022", label = "$p$", class = c("tiny_labelled", "character"))
        )
        , row.names = 1L
        , class = c("apa_results_table", "data.frame")
      )
    )
  }
)


test_that(
  "Warn if observed factors do not match"
  , {
    expect_warning(
      apa_print(afex::aov_4(yield~(N*P|block), data = npk, observed = "N"), observed = "P")
      , regexp = "In your call to apa_print(), you specified the model terms \"P\" as observed, whereas in your call to afex::aov_car(), you specified the model terms \"N\" as observed. Make sure that this is what you want."
      , fixed = TRUE
    )
  }
)


test_that(
  "Ensure proper sorting of terms"
  , {
    load("data/mixed_data.rdata")
    unsorted_aov <- afex::aov_4(formula = Recall ~ Gender * Dosage * (Task|Subject), data = mixed_data, fun_aggregate = mean)
    apa_out <- apa_print(unsorted_aov)

    expect_equal(
      unlabel(gsub(apa_out$table$term, pattern = " $\\times$ ", replacement = "_", fixed = TRUE))
      , names(apa_out$estimate)
    )
  }
)

test_that(
  "Suppress MSE if requested"
  , {
    npk$id <- seq_len(nrow(npk))
    out <- apa_print(
      afex::aov_4(
        formula = yield ~ N + (1 | id)
        , data = npk
        , fun_aggregate = mean
      )
      , estimate = "ges"
      , mse = FALSE
    )

    expect_identical(
      colnames(out$table)
      , c("term", "estimate", "statistic", "df", "df.residual", "p.value")
    )

    out <- apa_print(
      afex::aov_4(
        formula = yield ~ (N | block)
        , data = npk
        , fun_aggregate = mean
      )
      , estimate = "ges"
      , mse = FALSE
    )

    expect_identical(
      colnames(out$table)
      , c("term", "estimate", "statistic", "df", "df.residual", "p.value")
    )


  }
)

test_that(
  "Analysis of deviance from car"
  , {
    fit <- bwt.mu <- nnet::multinom(low ~ ., MASS::birthwt)
    car_out <- papaja::apa_print(car::Anova(fit))
    expect_apa_results(
      car_out
      , labels = list(
        term          = "Term"
        , statistic   = "$\\chi^2$"
        , df          = "$\\mathit{df}$"
        , p.value     = "$p$"
      )
    )
    # Example 1: a proportional odds model fitted to pneumo.
    set.seed(1)
    pneumo <- transform(VGAM::pneumo, let = log(exposure.time), x3 = runif(8))
    fit1 <- VGAM::vglm(cbind(normal, mild, severe) ~ let     , VGAM::propodds, pneumo)
    fit2 <- VGAM::vglm(cbind(normal, mild, severe) ~ let + x3, VGAM::propodds, pneumo)
    fit3 <- VGAM::vglm(cbind(normal, mild, severe) ~ let + x3, VGAM::cumulative, pneumo)
    car_out <- apa_print(car::Anova(fit1, type = 3))
    expect_apa_results(
      car_out
      , labels = list(
        term          = "Term"
        , statistic   = "$\\chi^2$"
        , df          = "$\\mathit{df}$"
        , p.value     = "$p$"
      )
    )
  }
)

test_that(
  "Analysis of deviance from the stats and car packages"
  , {
    # From stats::glm() examples section:
    ## Dobson (1990) Page 93: Randomized Controlled Trial :
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    treatment <- gl(3,3)
    data.frame(treatment, outcome, counts) # showing data
    glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())

    chisq_out <- apa_print(anova(glm.D93, test = "Chisq"))
    cp_out    <- apa_print(anova(glm.D93, test = "Cp"))
    lrt_out   <- apa_print(anova(glm.D93, test = "LRT"))
    rao_out   <- apa_print(anova(glm.D93, test = "Rao"))

    expect_identical(
      chisq_out$full_result
      , list(
        outcome = "$\\chi^2(2) = 5.45$, $p = .065$"
        , treatment = "$\\chi^2(2) = 0.00$, $p > .999$"
      )
    )
    expect_identical(
      cp_out$full_result
      , list(
        outcome = "$C_p = 11.13$"
        , treatment = "$C_p = 15.13$"
      )
    )
    expect_identical(
      lrt_out
      , chisq_out
    )
    expect_identical(
      rao_out$full_result
      , list(
        outcome = "$\\mathit{RS}(2) = 5.56$, $p = .062$"
        , treatment = "$\\mathit{RS}(2) = 0.00$, $p > .999$"
      )
    )

    car_lr_out   <- apa_print(car::Anova(glm.D93, type = 3, test.statistic = "LR"))
    car_wald_out <- apa_print(car::Anova(glm.D93, type = 3, test.statistic = "Wald"))
    car_f_out    <- apa_print(car::Anova(glm.D93, type = 3, test.statistic = "F"))

    expect_identical(
      car_lr_out$full_result
      , list(
        outcome = "$\\chi^2(2) = 5.45$, $p = .065$"
        , treatment = "$\\chi^2(2) = 0.00$, $p > .999$"
      )
    )
    expect_identical(
      car_wald_out$full_result
      , list(
        Intercept = "$\\chi^2(1) = 317.37$, $p < .001$"
        , outcome = "$\\chi^2(2) = 5.49$, $p = .064$"
        , treatment = "$\\chi^2(2) = 0.00$, $p > .999$"
      )
    )
    expect_identical(
      car_f_out$full_result
      , list(
        outcome     = "$F(2, 4) = 2.11$, $p = .237$"
        , treatment = "$F(2, 4) = 0.00$, $p > .999$"
      )
    )
  }
)


# restore previous options
 options(op)
