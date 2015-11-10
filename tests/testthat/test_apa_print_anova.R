context("apa_print() for ANOVA")

# Data and examples from http://personality-project.org/r/r.guide.html#anova

test_that(
  "One-way between ANOVA"
  , {
    load("data/ow_data.rdata")
    ow_aov <- aov(Alertness ~ Dosage, data = ow_data)
    ow_aov_output <- apa_print(ow_aov)

    expect_that(ow_aov_output, is_a("list"))
    expect_that(length(ow_aov_output), equals(4))
    expect_that(names(ow_aov_output), equals(c("stat", "est", "full", "table")))

    # stat
    expect_that(ow_aov_output$stat, is_a("list"))
    expect_that(length(ow_aov_output$stat), equals(1))
    expect_that(names(ow_aov_output$stat), equals("Dosage"))
    expect_that(ow_aov_output$stat$Dosage, is_a("character"))
    expect_that(ow_aov_output$stat$Dosage, equals("$F(2, 15) = 8.79$, $p = .003$"))

    # est
    expect_that(ow_aov_output$est, is_a("list"))
    expect_that(length(ow_aov_output$est), equals(1))
    expect_that(names(ow_aov_output$est), equals("Dosage"))
    expect_that(ow_aov_output$est$Dosage, is_a("character"))
    expect_that(ow_aov_output$est$Dosage, equals("$\\eta^2_G = .540$"))

    # full
    expect_that(ow_aov_output$full, is_a("list"))
    expect_that(length(ow_aov_output$full), equals(1))
    expect_that(names(ow_aov_output$full), equals("Dosage"))
    expect_that(ow_aov_output$full$Dosage, is_a("character"))
    expect_that(ow_aov_output$full$Dosage, equals("$F(2, 15) = 8.79$, $p = .003$, $\\eta^2_G = .540$"))

    # table
    expect_that(ow_aov_output$table, is_a("data.frame"))
    expect_that(nrow(ow_aov_output$table), equals(1))
    expect_that(colnames(ow_aov_output$table), equals(c("Effect", "$F$", "$df_1$", "$df_2$", "$p$", "$\\eta^2_G$")))

    # Other classes
    ow_aov_summary_output <- apa_print(summary(ow_aov))
    expect_that(ow_aov_summary_output, is_identical_to(ow_aov_output))

    ow_aov_Anova_output <- apa_print(car::Anova(ow_aov))
    expect_that(ow_aov_Anova_output, is_identical_to(ow_aov_output))

    ow_afex_data <- cbind(id = 1:nrow(ow_data), ow_data)
    ow_afex_aov <- afex::aov_ez(
      data = ow_afex_data
      , id = "id"
      , dv = "Alertness"
      , between = "Dosage"
      , return = "aov"
    )

    ow_afex_aov_output <- apa_print(ow_afex_aov)
    expect_that(ow_afex_aov_output, is_identical_to(ow_aov_output))

    # Other effect sizes
    ow_aov_output <- apa_print(ow_aov, es = "pes")
    expect_that(ow_aov_output$est$Dosage, equals("$\\eta^2_p = .540$"))

    ow_aov_output <- apa_print(ow_aov, es = c("pes", "ges"))
    expect_that(ow_aov_output$est$Dosage, equals("$\\eta^2_p = .540$, $\\eta^2_G = .540$"))

    # In parentheses
    ow_aov_output <- apa_print(ow_aov, in_paren = TRUE)
    expect_that(ow_aov_output$stat$Dosage, equals("$F[2, 15] = 8.79$, $p = .003$"))
  }
)


test_that(
  "Two-way between ANOVA"
  , {
    load("data/tw_data.rdata")
    tw_aov <- aov(Alertness ~ Gender * Dosage, tw_data)
    tw_aov_output <- apa_print(tw_aov)

    expect_that(tw_aov_output, is_a("list"))
    expect_that(length(tw_aov_output), equals(4))
    expect_that(names(tw_aov_output), equals(c("stat", "est", "full", "table")))

    # stat
    expect_that(tw_aov_output$stat, is_a("list"))
    expect_that(length(tw_aov_output$stat), equals(3))
    expect_that(names(tw_aov_output$stat), equals(c("Gender", "Dosage", "Gender_Dosage")))
    expect_that(tw_aov_output$stat$Gender, is_a("character"))
    expect_that(tw_aov_output$stat$Gender, equals("$F(1, 12) = 2.95$, $p = .111$"))
    expect_that(tw_aov_output$stat$Dosage, is_a("character"))
    expect_that(tw_aov_output$stat$Dosage, equals("$F(1, 12) = 0.20$, $p = .666$"))
    expect_that(tw_aov_output$stat$Gender_Dosage, is_a("character"))
    expect_that(tw_aov_output$stat$Gender_Dosage, equals("$F(1, 12) = 0.00$, $p = .962$"))

    # est
    expect_that(tw_aov_output$est, is_a("list"))
    expect_that(length(tw_aov_output$est), equals(3))
    expect_that(names(tw_aov_output$est), equals(c("Gender", "Dosage", "Gender_Dosage")))
    expect_that(tw_aov_output$est$Gender, is_a("character"))
    expect_that(tw_aov_output$est$Gender, equals("$\\eta^2_G = .197$"))
    expect_that(tw_aov_output$est$Dosage, is_a("character"))
    expect_that(tw_aov_output$est$Dosage, equals("$\\eta^2_G = .016$"))
    expect_that(tw_aov_output$est$Gender_Dosage, is_a("character"))
    expect_that(tw_aov_output$est$Gender_Dosage, equals("$\\eta^2_G = .000$"))

    # full
    expect_that(tw_aov_output$full, is_a("list"))
    expect_that(length(tw_aov_output$full), equals(3))
    expect_that(names(tw_aov_output$full), equals(c("Gender", "Dosage", "Gender_Dosage")))
    expect_that(tw_aov_output$full$Gender, is_a("character"))
    expect_that(tw_aov_output$full$Gender, equals("$F(1, 12) = 2.95$, $p = .111$, $\\eta^2_G = .197$"))
    expect_that(tw_aov_output$full$Dosage, is_a("character"))
    expect_that(tw_aov_output$full$Dosage, equals("$F(1, 12) = 0.20$, $p = .666$, $\\eta^2_G = .016$"))
    expect_that(tw_aov_output$full$Gender_Dosage, is_a("character"))
    expect_that(tw_aov_output$full$Gender_Dosage, equals("$F(1, 12) = 0.00$, $p = .962$, $\\eta^2_G = .000$"))

    # table
    expect_that(tw_aov_output$table, is_a("data.frame"))
    expect_that(nrow(tw_aov_output$table), equals(3))
    expect_that(colnames(tw_aov_output$table), equals(c("Effect", "$F$", "$df_1$", "$df_2$", "$p$", "$\\eta^2_G$")))
    expect_that(tw_aov_output$table$Effect, equals(c("Gender", "Dosage", "Gender $\\times$ Dosage")))

    # Other classes
    tw_aov_Anova_output <- apa_print(car::Anova(tw_aov))
    expect_that(tw_aov_Anova_output, is_identical_to(tw_aov_output))

    tw_aov_summary_output <- apa_print(summary(tw_aov))
    expect_that(tw_aov_summary_output, is_identical_to(tw_aov_output))


    tw_afex_data <- cbind(id = 1:nrow(tw_data), tw_data)
    tw_afex_aov <- afex::aov_ez(
      data = tw_afex_data
      , id = "id"
      , dv = "Alertness"
      , between = c("Gender", "Dosage")
      , return = "aov"
    )

    tw_afex_aov_output <- apa_print(tw_afex_aov)
    expect_that(tw_afex_aov_output, is_identical_to(tw_aov_output))
  }
)


test_that(
  "One-way repeated-measures ANOVA"
  , {
    load("data/rm_data.rdata")
    rm_aov <- aov(Recall ~ Valence + Error(Subject/Valence), rm_data)
    rm_aov_output <- apa_print(rm_aov)

    expect_that(rm_aov_output, is_a("list"))
    expect_that(length(rm_aov_output), equals(4))
    expect_that(names(rm_aov_output), equals(c("stat", "est", "full", "table")))

    # stat
    expect_that(rm_aov_output$stat, is_a("list"))
    expect_that(length(rm_aov_output$stat), equals(1))
    expect_that(names(rm_aov_output$stat), equals("Valence"))
    expect_that(rm_aov_output$stat$Valence, is_a("character"))
    expect_that(rm_aov_output$stat$Valence, equals("$F(2, 8) = 189.11$, $p < .001$"))

    # est
    expect_that(rm_aov_output$est, is_a("list"))
    expect_that(length(rm_aov_output$est), equals(1))
    expect_that(names(rm_aov_output$est), equals("Valence"))
    expect_that(rm_aov_output$est$Valence, is_a("character"))
    expect_that(rm_aov_output$est$Valence, equals("$\\eta^2_G = .93$"))

    # full
    expect_that(rm_aov_output$full, is_a("list"))
    expect_that(length(rm_aov_output$full), equals(1))
    expect_that(names(rm_aov_output$full), equals("Valence"))
    expect_that(rm_aov_output$full$Valence, is_a("character"))
    expect_that(rm_aov_output$full$Valence, equals("$F(2, 8) = 189.11$, $p < .001$, $\\eta^2_G = .93$"))

    # Other classes
    rm_aov_summary_output <- apa_print(summary(rm_aov))
    expect_that(rm_aov_summary_output, is_identical_to(rm_aov_output))

    rm_afex_aov <- afex::aov_ez(
      data = rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = "Valence"
      , return = "aov"
    )

    rm_afex_aov_output <- apa_print(rm_afex_aov)
    expect_that(rm_afex_aov_output, is_identical_to(rm_aov_output))


    rm_afex_anova.mlm <- afex::aov_ez(
      data = rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = "Valence"
      , return = "Anova"
    )

    ## DF corrections
    rm_afex_anova.mlm_output <- apa_print(rm_afex_anova.mlm)
    expect_that(rm_afex_anova.mlm_output$full$Valence, equals("$F(1.15, 4.6) = 189.11$, $p < .001$, $\\eta^2_G = .93$"))

    rm_afex_anova.mlm_output <- apa_print(rm_afex_anova.mlm, correction = "HF")
    expect_that(rm_afex_anova.mlm_output$full$Valence, equals("$F(1.32, 5.26) = 189.11$, $p < .001$, $\\eta^2_G = .93$"))

    rm_afex_anova.mlm_output <- apa_print(rm_afex_anova.mlm, correction = "none")
    expect_that(rm_afex_anova.mlm_output$full$Valence, is_identical_to(rm_afex_anova.mlm_output$full$Valence))
  }
)


test_that(
  "Two-way repeated-measures ANOVA"
  , {
    load("data/tw_rm_data.rdata")
    tw_rm_aov <- aov(Recall~ (Task * Valence) + Error(Subject/(Task * Valence)), tw_rm_data)
    tw_rm_aov_output <- apa_print(tw_rm_aov)

    expect_that(tw_rm_aov_output, is_a("list"))
    expect_that(length(tw_rm_aov_output), equals(4))
    expect_that(names(tw_rm_aov_output), equals(c("stat", "est", "full", "table")))

    # stat
    expect_that(tw_rm_aov_output$stat, is_a("list"))
    expect_that(length(tw_rm_aov_output$stat), equals(3))
    expect_that(names(tw_rm_aov_output$stat), equals(c("Task", "Valence", "Task_Valence")))
    expect_that(tw_rm_aov_output$stat$Task, is_a("character"))
    expect_that(tw_rm_aov_output$stat$Task, equals("$F(1, 4) = 7.35$, $p = .054$"))
    expect_that(tw_rm_aov_output$stat$Valence, is_a("character"))
    expect_that(tw_rm_aov_output$stat$Valence, equals("$F(2, 8) = 1.46$, $p = .288$"))
    expect_that(tw_rm_aov_output$stat$Task_Valence, is_a("character"))
    expect_that(tw_rm_aov_output$stat$Task_Valence, equals("$F(2, 8) = 0.29$, $p = .755$"))

    # est
    expect_that(tw_rm_aov_output$est, is_a("list"))
    expect_that(length(tw_rm_aov_output$est), equals(3))
    expect_that(names(tw_rm_aov_output$est), equals(c("Task", "Valence", "Task_Valence")))
    expect_that(tw_rm_aov_output$est$Task, is_a("character"))
    expect_that(tw_rm_aov_output$est$Task, equals("$\\eta^2_G = .324$"))
    expect_that(tw_rm_aov_output$est$Valence, is_a("character"))
    expect_that(tw_rm_aov_output$est$Valence, equals("$\\eta^2_G = .136$"))
    expect_that(tw_rm_aov_output$est$Task_Valence, is_a("character"))
    expect_that(tw_rm_aov_output$est$Task_Valence, equals("$\\eta^2_G = .022$"))

    # full
    expect_that(tw_rm_aov_output$full, is_a("list"))
    expect_that(length(tw_rm_aov_output$full), equals(3))
    expect_that(names(tw_rm_aov_output$full), equals(c("Task", "Valence", "Task_Valence")))
    expect_that(tw_rm_aov_output$full$Task, is_a("character"))
    expect_that(tw_rm_aov_output$full$Task, equals("$F(1, 4) = 7.35$, $p = .054$, $\\eta^2_G = .324$"))
    expect_that(tw_rm_aov_output$full$Valence, is_a("character"))
    expect_that(tw_rm_aov_output$full$Valence, equals("$F(2, 8) = 1.46$, $p = .288$, $\\eta^2_G = .136$"))
    expect_that(tw_rm_aov_output$full$Task_Valence, is_a("character"))
    expect_that(tw_rm_aov_output$full$Task_Valence, equals("$F(2, 8) = 0.29$, $p = .755$, $\\eta^2_G = .022$"))

    # Other classes
    tw_rm_aov_summary_output <- apa_print(summary(tw_rm_aov))
    expect_that(tw_rm_aov_summary_output, is_identical_to(tw_rm_aov_output))

    tw_rm_afex_aov <- afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
      , return = "aov"
    )

    tw_rm_afex_aov_output <- apa_print(tw_rm_afex_aov)
    expect_that(tw_rm_afex_aov_output, is_identical_to(tw_rm_aov_output))

    # Observed
    tw_rm_aov_output <- apa_print(tw_rm_aov, observed = "Task")

    expect_that(tw_rm_aov_output$est$Valence, is_a("character"))
    expect_that(tw_rm_aov_output$est$Valence, equals("$\\eta^2_G = .096$"))
    expect_that(tw_rm_aov_output$est$Task_Valence, is_a("character"))
    expect_that(tw_rm_aov_output$est$Task_Valence, equals("$\\eta^2_G = .015$"))
  }
)


test_that(
  "Levene test"
  , {
    levene_test <- car::leveneTest(conformity ~ fcategory * partner.status, data = car::Moore)

    levene_test_output <- apa_print(levene_test)
    expect_that(levene_test_output, is_a("list"))
    expect_that(length(levene_test_output), equals(1))
    expect_that(levene_test_output$stat$group, equals("$F(5, 39) = 1.47$, $p = .222$"))
  }
)

# Model comparison

test_that(
  "lm model comparison"
  , {
#     baseline <- lm(formula = Fertility ~ ., data = swiss)
#     model_1 <- update(baseline, formula = . ~ . - Examination)
#     model_comp <- anova(baseline, model_1)
#
#     model_comp_output <- apa_print(model_comp)
#
#     expect_that(model_comp_output, is_a("list"))
#     expect_that(length(model_comp_output), equals(1))
#     expect_that(model_comp_output$stat$model2, equals("$F(1, 41) = 1.03$, $p = .315$"))

#     drop1(baseline, test = "F")
#     drop1(baseline, test = "Chisq")
  }
)



# library("afex")
# data(sk2011.1)
#
# a1 <- aov_ez("id", "response", sk2011.1, between = "instruction", within = c("inference", "plausibility"))
# # object of class afex_aov
#
# a1$anova_table # object of class anova and data.frame
# a1$Anova # Anova.mlm
#
# summary(a1) # Summary Anova.mlm -> Can we just use the existing methods?
#
# anova(a1, correction = "HF") # Get different es and corrections
#
# library("papaja")
# apa_print(a1$Anova, es = "pes")
# apa_print(summary(a1))
