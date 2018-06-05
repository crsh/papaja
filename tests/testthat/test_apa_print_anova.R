context("apa_print() for ANOVA")

# Data and examples from http://personality-project.org/r/r.guide.html#anova

test_that(
  "One-way between ANOVA"
  , {
    load("data/ow_data.rdata")
    ow_aov <- aov(Alertness ~ Dosage, data = ow_data)
    ow_aov_output <- apa_print(ow_aov)

    expect_is(ow_aov_output, "list")
    expect_equal(names(ow_aov_output), container_names)

    # stat
    expect_is(ow_aov_output$stat, "list")
    expect_equal(names(ow_aov_output$stat), "Dosage")
    expect_is(ow_aov_output$stat$Dosage, "character")
    expect_equal(ow_aov_output$stat$Dosage, "$F(2, 15) = 8.79$, $\\mathit{MSE} = 24.25$, $p = .003$")

    # est
    expect_is(ow_aov_output$est, "list")
    expect_equal(names(ow_aov_output$est), "Dosage")
    expect_is(ow_aov_output$est$Dosage, "character")
    expect_equal(ow_aov_output$est$Dosage, "$\\hat{\\eta}^2_G = .540$")

    # full
    expect_is(ow_aov_output$full, "list")
    expect_equal(names(ow_aov_output$full), "Dosage")
    expect_is(ow_aov_output$full$Dosage, "character")
    expect_equal(ow_aov_output$full$Dosage, "$F(2, 15) = 8.79$, $\\mathit{MSE} = 24.25$, $p = .003$, $\\hat{\\eta}^2_G = .540$")

    # table
    expect_is(ow_aov_output$table, "data.frame")
    expect_equal(nrow(ow_aov_output$table), 1)
    expect_equal(colnames(ow_aov_output$table), c("Effect", "F","df1", "df2", "MSE", "p", "ges"))
    expect_equal(unname(unlist(variable_label(ow_aov_output$table))), c("Effect", "$F$","$\\mathit{df}_1$", "$\\mathit{df}_2$", "$\\mathit{MSE}$", "$p$", "$\\hat{\\eta}^2_G$"))

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

    # stat
    expect_is(ow_afex_aov_output$stat, "list")
    expect_equal(names(ow_afex_aov_output$stat), c("Intercept", "Dosage"))
    expect_is(ow_afex_aov_output$stat$Intercept, "character")
    expect_equal(ow_afex_aov_output$stat$Intercept, "$F(1, 15) = 487.23$, $\\mathit{MSE} = 24.25$, $p < .001$")

    # est
    expect_is(ow_afex_aov_output$est, "list")
    expect_equal(names(ow_afex_aov_output$est), c("Intercept", "Dosage"))
    expect_is(ow_afex_aov_output$est$Intercept, "character")
    expect_equal(ow_afex_aov_output$est$Intercept, "$\\hat{\\eta}^2_G = .970$")

    # full
    expect_is(ow_afex_aov_output$full, "list")
    expect_equal(names(ow_afex_aov_output$full), c("Intercept", "Dosage"))
    expect_is(ow_afex_aov_output$full$Intercept, "character")
    expect_equal(ow_afex_aov_output$full$Intercept, "$F(1, 15) = 487.23$, $\\mathit{MSE} = 24.25$, $p < .001$, $\\hat{\\eta}^2_G = .970$")

    # table
    expect_is(ow_afex_aov_output$table, "data.frame")
    expect_equal(nrow(ow_afex_aov_output$table), 2)
    expect_equal(colnames(ow_afex_aov_output$table), c("Effect", "F", "df1", "df2", "MSE", "p", "ges"))
    expect_equal(unname(unlist(variable_label(ow_afex_aov_output$table))), c("Effect", "$F$", "$\\mathit{df}_1$", "$\\mathit{df}_2$", "$\\mathit{MSE}$", "$p$", "$\\hat{\\eta}^2_G$"))
    expect_identical(
      ow_afex_aov_output$table$Effect
      , structure(
        c("Intercept", "Dosage")
        , class = c("papaja_labelled", "character")
        , label = "Effect"
      )
    )

    ow_afex_aov_output2 <- apa_print(ow_afex_aov, intercept = TRUE)
    expect_identical(ow_afex_aov_output2, ow_afex_aov_output)


    # Other effect sizes
    ow_aov_output <- apa_print(ow_aov, es = "pes")
    expect_equal(ow_aov_output$est$Dosage, "$\\hat{\\eta}^2_p = .540$")

    ow_aov_output <- apa_print(ow_aov, es = c("pes", "ges"))
    expect_equal(ow_aov_output$est$Dosage, "$\\hat{\\eta}^2_p = .540$, $\\hat{\\eta}^2_G = .540$")
  }
)


test_that(
  "Two-way between ANOVA"
  , {
    load("data/tw_data.rdata")
    tw_aov <- aov(Alertness ~ Gender * Dosage, tw_data)
    tw_aov_output <- apa_print(tw_aov)

    # stat
    expect_is(tw_aov_output$stat, "list")
    expect_equal(names(tw_aov_output$stat), c("Gender", "Dosage", "Gender_Dosage"))
    expect_is(tw_aov_output$stat$Gender, "character")
    expect_equal(tw_aov_output$stat$Gender, "$F(1, 12) = 2.95$, $\\mathit{MSE} = 25.94$, $p = .111$")
    expect_is(tw_aov_output$stat$Dosage, "character")
    expect_equal(tw_aov_output$stat$Dosage, "$F(1, 12) = 0.20$, $\\mathit{MSE} = 25.94$, $p = .666$")
    expect_is(tw_aov_output$stat$Gender_Dosage, "character")
    expect_equal(tw_aov_output$stat$Gender_Dosage, "$F(1, 12) = 0.00$, $\\mathit{MSE} = 25.94$, $p = .962$")

    # est
    expect_is(tw_aov_output$est, "list")
    expect_equal(names(tw_aov_output$est), c("Gender", "Dosage", "Gender_Dosage"))
    expect_is(tw_aov_output$est$Gender, "character")
    expect_equal(tw_aov_output$est$Gender, "$\\hat{\\eta}^2_G = .197$")
    expect_is(tw_aov_output$est$Dosage, "character")
    expect_equal(tw_aov_output$est$Dosage, "$\\hat{\\eta}^2_G = .016$")
    expect_is(tw_aov_output$est$Gender_Dosage, "character")
    expect_equal(tw_aov_output$est$Gender_Dosage, "$\\hat{\\eta}^2_G = .000$")

    # full
    expect_is(tw_aov_output$full, "list")
    expect_equal(names(tw_aov_output$full), c("Gender", "Dosage", "Gender_Dosage"))
    expect_is(tw_aov_output$full$Gender, "character")
    expect_equal(tw_aov_output$full$Gender, "$F(1, 12) = 2.95$, $\\mathit{MSE} = 25.94$, $p = .111$, $\\hat{\\eta}^2_G = .197$")
    expect_is(tw_aov_output$full$Dosage, "character")
    expect_equal(tw_aov_output$full$Dosage, "$F(1, 12) = 0.20$, $\\mathit{MSE} = 25.94$, $p = .666$, $\\hat{\\eta}^2_G = .016$")
    expect_is(tw_aov_output$full$Gender_Dosage, "character")
    expect_equal(tw_aov_output$full$Gender_Dosage, "$F(1, 12) = 0.00$, $\\mathit{MSE} = 25.94$, $p = .962$, $\\hat{\\eta}^2_G = .000$")

    # table
    expect_is(tw_aov_output$table, "data.frame")
    expect_equal(nrow(tw_aov_output$table), 3)
    expect_equal(colnames(tw_aov_output$table), c("Effect", "F","df1", "df2", "MSE", "p", "ges"))
    expect_equal(unname(unlist(variable_label(tw_aov_output$table))), c("Effect", "$F$","$\\mathit{df}_1$", "$\\mathit{df}_2$", "$\\mathit{MSE}$", "$p$", "$\\hat{\\eta}^2_G$"))
    expect_identical(
      tw_aov_output$table$Effect
      , expected = structure(
        c("Gender", "Dosage", "Gender $\\times$ Dosage")
        , label = "Effect"
        , class = c("papaja_labelled", "character")
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

    expect_equal(names(tw_afex_aov_output), container_names)

    ## stat
    expect_equal(names(tw_afex_aov_output$stat), c("Intercept", "Gender", "Dosage", "Gender_Dosage"))
    expect_is(tw_afex_aov_output$stat$Intercept, "character")
    expect_equal(tw_afex_aov_output$stat$Intercept, "$F(1, 12) = 121.99$, $\\mathit{MSE} = 25.94$, $p < .001$")

    ## est
    expect_equal(names(tw_afex_aov_output$est), c("Intercept", "Gender", "Dosage", "Gender_Dosage"))
    expect_is(tw_afex_aov_output$est$Intercept, "character")
    expect_equal(tw_afex_aov_output$est$Intercept, "$\\hat{\\eta}^2_G = .910$")

    ## full
    expect_is(tw_afex_aov_output$full, "list")
    expect_equal(names(tw_afex_aov_output$full), c("Intercept", "Gender", "Dosage", "Gender_Dosage"))
    expect_is(tw_afex_aov_output$full$Intercept, "character")
    expect_equal(tw_afex_aov_output$full$Intercept, "$F(1, 12) = 121.99$, $\\mathit{MSE} = 25.94$, $p < .001$, $\\hat{\\eta}^2_G = .910$")

    ## table
    expect_is(tw_afex_aov_output$table, "data.frame")
    expect_equal(nrow(tw_afex_aov_output$table), 4)
    expect_identical(
      object = tw_afex_aov_output$table$Effect
      , expected = structure(
        c("Intercept", "Gender", "Dosage", "Gender $\\times$ Dosage")
        , label = "Effect"
        , class = c("papaja_labelled", "character")
      )
    )

    tw_afex_aov_output2 <- apa_print(tw_afex_aov, intercept = TRUE)
    expect_identical(tw_afex_aov_output2, tw_afex_aov_output)

    # Observed effects
    tw_rm_aov_output <- apa_print(tw_aov, observed = "Gender")

    expect_equal(tw_rm_aov_output$est$Gender, "$\\hat{\\eta}^2_G = .197$")
    expect_equal(tw_rm_aov_output$est$Dosage, "$\\hat{\\eta}^2_G = .013$")
    expect_equal(tw_rm_aov_output$est$Gender_Dosage, "$\\hat{\\eta}^2_G = .000$")
  }
)


test_that(
  "One-way repeated-measures ANOVA"
  , {
    load("data/rm_data.rdata")
    rm_aov <- aov(Recall ~ Valence + Error(Subject/Valence), rm_data)
    rm_aov_output <- apa_print(rm_aov)

    expect_is(rm_aov_output, "list")
    expect_equal(names(rm_aov_output), container_names)

    # stat
    expect_is(rm_aov_output$stat, "list")
    expect_equal(names(rm_aov_output$stat), "Valence")
    expect_is(rm_aov_output$stat$Valence, "character")
    expect_equal(rm_aov_output$stat$Valence, "$F(2, 8) = 189.11$, $\\mathit{MSE} = 5.37$, $p < .001$")

    # est
    expect_is(rm_aov_output$est, "list")
    expect_equal(names(rm_aov_output$est), "Valence")
    expect_is(rm_aov_output$est$Valence, "character")
    expect_equal(rm_aov_output$est$Valence, "$\\hat{\\eta}^2_G = .932$")

    # full
    expect_is(rm_aov_output$full, "list")
    expect_equal(names(rm_aov_output$full), "Valence")
    expect_is(rm_aov_output$full$Valence, "character")
    expect_equal(rm_aov_output$full$Valence, "$F(2, 8) = 189.11$, $\\mathit{MSE} = 5.37$, $p < .001$, $\\hat{\\eta}^2_G = .932$")

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
    expect_equal(rm_afex_anova.mlm_output$full$Valence, "$F(1.15, 4.6) = 189.11$, $\\mathit{MSE} = 9.34$, $p < .001$, $\\hat{\\eta}^2_G = .932$")

    rm_afex_anova.mlm_output <- apa_print(rm_afex_aov$Anova, correction = "HF")
    expect_equal(rm_afex_anova.mlm_output$full$Valence, "$F(1.32, 5.26) = 189.11$, $\\mathit{MSE} = 8.16$, $p < .001$, $\\hat{\\eta}^2_G = .932$")

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

    expect_is(tw_rm_aov_output, "list")
    expect_equal(names(tw_rm_aov_output), container_names)

    # stat
    expect_is(tw_rm_aov_output$stat, "list")
    expect_equal(names(tw_rm_aov_output$stat), c("Task", "Valence", "Task_Valence"))
    expect_is(tw_rm_aov_output$stat$Task, "character")
    expect_equal(tw_rm_aov_output$stat$Task, "$F(1, 4) = 7.35$, $\\mathit{MSE} = 4.08$, $p = .054$")
    expect_is(tw_rm_aov_output$stat$Valence, "character")
    expect_equal(tw_rm_aov_output$stat$Valence, "$F(2, 8) = 1.46$, $\\mathit{MSE} = 3.36$, $p = .288$")
    expect_is(tw_rm_aov_output$stat$Task_Valence, "character")
    expect_equal(tw_rm_aov_output$stat$Task_Valence, "$F(2, 8) = 0.29$, $\\mathit{MSE} = 2.41$, $p = .755$")

    # est
    expect_is(tw_rm_aov_output$est, "list")
    expect_equal(names(tw_rm_aov_output$est), c("Task", "Valence", "Task_Valence"))
    expect_is(tw_rm_aov_output$est$Task, "character")
    expect_equal(tw_rm_aov_output$est$Task, "$\\hat{\\eta}^2_G = .068$")
    expect_is(tw_rm_aov_output$est$Valence, "character")
    expect_equal(tw_rm_aov_output$est$Valence, "$\\hat{\\eta}^2_G = .023$")
    expect_is(tw_rm_aov_output$est$Task_Valence, "character")
    expect_equal(tw_rm_aov_output$est$Task_Valence, "$\\hat{\\eta}^2_G = .003$")

    # full
    expect_is(tw_rm_aov_output$full, "list")
    expect_equal(names(tw_rm_aov_output$full), c("Task", "Valence", "Task_Valence"))
    expect_is(tw_rm_aov_output$full$Task, "character")
    expect_equal(tw_rm_aov_output$full$Task, "$F(1, 4) = 7.35$, $\\mathit{MSE} = 4.08$, $p = .054$, $\\hat{\\eta}^2_G = .068$")
    expect_is(tw_rm_aov_output$full$Valence, "character")
    expect_equal(tw_rm_aov_output$full$Valence, "$F(2, 8) = 1.46$, $\\mathit{MSE} = 3.36$, $p = .288$, $\\hat{\\eta}^2_G = .023$")
    expect_is(tw_rm_aov_output$full$Task_Valence, "character")
    expect_equal(tw_rm_aov_output$full$Task_Valence, "$F(2, 8) = 0.29$, $\\mathit{MSE} = 2.41$, $p = .755$, $\\hat{\\eta}^2_G = .003$")

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

    expect_is(tw_rm_aov_output$est$Task, "character")
    expect_equal(tw_rm_aov_output$est$Task, "$\\hat{\\eta}^2_G = .068$")
    expect_is(tw_rm_aov_output$est$Valence, "character")
    expect_equal(tw_rm_aov_output$est$Valence, "$\\hat{\\eta}^2_G = .022$")
    expect_is(tw_rm_aov_output$est$Task_Valence, "character")
    expect_equal(tw_rm_aov_output$est$Task_Valence, "$\\hat{\\eta}^2_G = .003$")
  }
)

test_that(
  "Levene test"
  , {
    levene_test <- car::leveneTest(conformity ~ fcategory * partner.status, data = carData::Moore)
    levene_test_output <- apa_print(levene_test)
    expect_is(levene_test_output, "list")
    expect_equal(names(levene_test_output), container_names)
    expect_equal(levene_test_output$stat, "$F(5, 39) = 1.47$, $p = .222$", check.attributes = FALSE)
  }
)
