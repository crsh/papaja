context("apa_print() for emmeans/lsmeans")

test_that(
  "ANOVA"
  , {
    emm_basis.afex_aov <- afex:::emm_basis.afex_aov

    load("data/tw_rm_data.rdata")
    tw_rm <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
    ))


    # Main effect ------------------------------------------------------

    tw_me_emm <- emmeans::emmeans(tw_rm, ~ Valence)
    tw_me_emm_output <- expect_warning(
      apa_print(tw_me_emm, conf.l = .95)
      , regexp = "Using argument 'conf.level' in calls to 'apa_print()' is deprecated. Please use 'conf.int' instead."
      , fixed = TRUE
    )

    expect_apa_results(
      tw_me_emm_output
      , labels = list(
        Valence = "Valence"
        , estimate = "$M$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value = "$p$"
      )
      , term_names = levels(tw_rm_data$Valence)
    )

    expect_apa_term(
      tw_me_emm_output
      , term = "Neg"
      , estimate = "$M = 11.00$, 95\\% CI $[6.34, 15.66]$"
      , statistic = "$t(4.63) = 6.21$, $p = .002$"
    )

    # Alternative calls
    tw_me_lsm <- emmeans::lsmeans(tw_rm, ~ Valence)
    tw_me_lsm_output <- apa_print(tw_me_lsm)
    tw_me_lsm_output2 <- apa_print(
      summary(tw_me_lsm, infer = TRUE)
      , est_name = "M"
    )
    tw_me_emm_output2 <- apa_print(
      summary(tw_me_emm, infer = TRUE)
      , est_name = "M"
    )

    expect_identical(tw_me_emm_output, tw_me_emm_output2)
    expect_identical(tw_me_lsm_output, tw_me_lsm_output2)
    expect_identical(tw_me_lsm_output, tw_me_emm_output2)


    expect_warning(
      tw_me_emm_p_output <- apa_print(
        summary(tw_me_emm, infer = c(FALSE, TRUE))
        , est_name = "M"
      )
    )
    expect_identical(
      tw_me_lsm_output$table[, c("Valence", "estimate", "statistic", "df", "p.value")]
      , tw_me_emm_p_output$table
    )

    tw_me_emm_ci_output <- apa_print(
      summary(tw_me_emm, infer = c(TRUE, FALSE))
      , est_name = "M"
    )

    expect_apa_term(
      tw_me_emm_ci_output
      , term = "Neg"
      , estimate = "$M = 11.00$, 95\\% CI $[6.34, 15.66]$"
      , statistic = NULL
    )

    expect_identical(
      tw_me_lsm_output$table[, c("Valence", "estimate", "conf.int")]
      , tw_me_emm_ci_output$table
    )

    expect_apa_results(
      apa_print(summary(tw_me_lsm, infer = c(FALSE, FALSE)))
      , labels = list(
        Valence = "Valence"
        , estimate = "$\\hat{\\theta}$"
      )
      , term_names = levels(tw_rm_data$Valence)
    )

    tw_me_emm_output3 <- apa_print(
      summary(tw_me_emm, infer = TRUE, side = ">")
    )

    # https://github.com/crsh/papaja/issues/456#issuecomment-901653372
    expect_apa_term(
      tw_me_emm_output3
      , term = "Neg"
      , estimate = "$\\hat{\\theta} = 11.00$, 95\\% CI $[7.37, \\infty]$"
      , statistic = "$t(4.63) = 6.21$, $p = .001$"
    )


    # Interaction ------------------------------------------------------
    tw_int_emm <- emmeans::emmeans(tw_rm, ~ Valence * Task)
    tw_int_emm_output <- apa_print(tw_int_emm)

    term_names <- apply(expand.grid(levels(tw_rm_data$Valence), levels(tw_rm_data$Task)), 1, paste, collapse = "_")

    expect_apa_results(
      tw_int_emm_output
      , labels = list(
        Task = "Task"
        , Valence = "Valence"
        , estimate = "$M$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
      , term_names = term_names
    )

    expect_apa_term(
      tw_int_emm_output
      , term = "Neg_Cued"
      , estimate = "$M = 11.80$, 95\\% CI $[7.17, 16.43]$"
      , statistic = "$t(5.52) = 6.37$, $p = .001$"
    )

    # Alternative calls
    tw_int_lsm <- emmeans::lsmeans(tw_rm, ~ Valence * Task)
    tw_int_lsm_output <- apa_print(tw_int_lsm)
    tw_int_lsm_output2 <- apa_print(
      summary(tw_int_lsm, infer = TRUE)
      , est_name = "M"
    )
    tw_int_emm_output2 <- apa_print(
      summary(tw_int_emm, infer = TRUE)
      , est_name = "M"
    )

    expect_identical(tw_int_emm_output, tw_int_emm_output2)
    expect_identical(tw_int_lsm_output, tw_int_lsm_output2)
    expect_identical(tw_int_lsm_output, tw_int_emm_output2)



    # Simple effects ---------------------------------------------------

    tw_se_emm <- emmeans::emmeans(tw_rm, ~ Valence | Task)
    tw_se_emm_output <- apa_print(tw_se_emm)

    expect_apa_results(
      tw_se_emm_output
      , labels = list(
        Task = "Task"
        , Valence = "Valence"
        , estimate = "$M$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
      , term_names = term_names
    )

    # Sort
    tw_int_emm_output$estimate <- tw_int_emm_output$estimate[names(tw_se_emm_output$estimate)]
    tw_int_emm_output$statistic <- tw_int_emm_output$statistic[names(tw_int_emm_output$estimate)]
    tw_int_emm_output$full_result <- tw_int_emm_output$full_result[names(tw_int_emm_output$estimate)]
    tw_int_emm_output$table <- tw_int_emm_output$table[rownames(tw_se_emm_output$table), ]

    expect_identical(tw_se_emm_output, tw_int_emm_output)


    # Complex output (four factors) ------------------------------------
    data(obk.long, package = "afex")

    fw_mixed <- suppressWarnings(afex::aov_ez(
      id = "id"
      , dv = "value"
      , data = obk.long
      , between = c("treatment", "gender")
      , within = c("phase", "hour")
      , observed = "gender"
    ))


    fw_mixed_emm <- emmeans::emmeans(fw_mixed, ~ phase * hour | treatment * gender)
    fw_mixed_emm_output <- apa_print(fw_mixed_emm)

    expect_apa_results(
      fw_mixed_emm_output
      , labels = list(
        gender = "gender"
        , treatment = "treatment"
        , hour = "hour"
        , phase = "phase"
        , estimate = "$M$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )

    # table ------------------------------------------------------------
    expect_identical(
      object = fw_mixed_emm_output$table$gender
      , expected = structure(
        c(
          "F"
          , rep("", nrow(fw_mixed_emm_output$table) / 2 - 1)
          , "M"
          , rep("", nrow(fw_mixed_emm_output$table) / 2 - 1)
        )
        , class = c("tiny_labelled", "character")
        , label = "gender"
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$treatment
      , expected = structure(
        rep(
          c(
            "Control"
            , rep("", nrow(fw_mixed_emm_output$table) / 6 - 1)
            , "A"
            , rep("", nrow(fw_mixed_emm_output$table) / 6 - 1)
            , "B"
            , rep("", nrow(fw_mixed_emm_output$table) / 6 - 1)
          )
          , 2
        )
        , class = c("tiny_labelled", "character")
        , label = "treatment"
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$hour
      , expected = structure(
        rep(
          c(
            "X1"
            , rep("", nrow(fw_mixed_emm_output$table) / 30 - 1)
            , "X2"
            , rep("", nrow(fw_mixed_emm_output$table) / 30 - 1)
            , "X3"
            , rep("", nrow(fw_mixed_emm_output$table) / 30 - 1)
            , "X4"
            , rep("", nrow(fw_mixed_emm_output$table) / 30 - 1)
            , "X5"
            , rep("", nrow(fw_mixed_emm_output$table) / 30 - 1)
          )
          , 6
        )
        , class = c("tiny_labelled", "character")
        , label = "hour"
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$phase
      , expected = structure(
        rep(
          c("Fup", "Post", "Pre")
          , 30
        )
        , class = c("tiny_labelled", "character")
        , label = "phase"
      )
    )

    # Alternative calls
    fw_mixed_lsm <- emmeans::lsmeans(fw_mixed, ~ phase * hour | treatment * gender)
    fw_mixed_lsm_output <- apa_print(fw_mixed_lsm)
    fw_mixed_lsm_output2 <- apa_print(
      summary(fw_mixed_lsm, infer = TRUE)
      , est_name = "M"
    )
    fw_mixed_emm_output2 <- apa_print(
      summary(fw_mixed_emm, infer = TRUE)
      , est_name = "M"
    )

    expect_identical(fw_mixed_lsm_output, fw_mixed_lsm_output2)
    expect_identical(fw_mixed_emm_output, fw_mixed_emm_output2)
    expect_identical(fw_mixed_lsm_output, fw_mixed_emm_output2)



    # Pairs ------------------------------------------------------------
    tw_me_pairs_emm <- pairs(tw_me_emm)
    tw_me_pairs_emm_output <- apa_print(tw_me_pairs_emm)

    expect_apa_results(
      tw_me_pairs_emm_output
      , labels = list(
        contrast = "Contrast"
        , estimate = "$\\Delta M$"
        , conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Tukey(3)}$"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Tukey(3)}$"
      )
      , term_names = c("Neg_Neu", "Neg_Pos", "Neu_Pos")
    )

    expect_apa_term(
      tw_me_pairs_emm_output
      , term = "Neg_Neu"
      , estimate = "$\\Delta M = -1.10$, $95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Tukey(3)}$ $[-3.44, 1.24]$"
      , statistic = "$t(8) = -1.34$, $p_\\mathrm{\\scriptsize Tukey(3)} = .413$"
    )

    ## Custom contrast names
    tw_me_pairs_emm_output <- apa_print(
      tw_me_pairs_emm
      , contrast_names = letters[1:3]
    )

    expect_identical(
      object = tw_me_pairs_emm_output$table$contrast
      , expected = structure(
        letters[1:3]
        , label = "Contrast"
        , class = c("tiny_labelled", "character")
      )
    )

    # Custom contrasts
    tw_se_contrast_emm <- emmeans::contrast(
        tw_se_emm
        , method = list("Positive - Negative" = c(1, 0, -1))
      )

    tw_se_contrast_emm_output <- apa_print(tw_se_contrast_emm)
    tw_se_contrast_emm_output2 <- apa_print(
      summary(tw_se_contrast_emm, infer = TRUE)
      , est_name = "\\Delta M"
    )

    expect_identical(
      tw_se_contrast_emm_output
      , tw_se_contrast_emm_output2
    )

    # Simple contrasts
    simple_pairs <- pairs(
      emmeans::emmeans(fw_mixed, ~ gender | treatment)
    )

    tw_between_emm <- emmeans::emmeans(fw_mixed, ~ treatment * gender)
    simple_pairs2 <- pairs(
      tw_between_emm
      , simple = "gender"
    )

    simple_pairs_output <- apa_print(simple_pairs)
    simple_pairs2_output <- apa_print(simple_pairs2)


    expect_apa_results(
      simple_pairs_output
      , term_names = c("Control_F_M", "A_F_M", "B_F_M")
    )

    expect_identical(simple_pairs_output, simple_pairs2_output)

    simple_contrasts <- emmeans::contrast(
      tw_between_emm
      , "consec"
      , simple = "each"
      , combine = TRUE
      , adjust = "none"
    )
    simple_contrasts_output <- apa_print(simple_contrasts)

    simple_contrasts2 <- emmeans::contrast(
      tw_between_emm
      , "consec"
      , simple = "each"
      , adjust = "none"
    )

    simple_contrasts2_output1 <- apa_print(simple_contrasts2$`simple contrasts for gender`)
    simple_contrasts2_output2 <- apa_print(simple_contrasts2$`simple contrasts for treatment`)

    expect_identical(
      simple_contrasts_output$full_result[grepl("M_F$", names(simple_contrasts_output$full_result))]
      , simple_contrasts2_output1$full_result
    )

    expect_identical(
      simple_contrasts_output$full_result[grepl("^(F|M)_", names(simple_contrasts_output$full_result))]
      , simple_contrasts2_output2$full_result
    )
  }
)

test_that(
  "Joint tests"
  , {
    emm_basis.afex_aov <- afex:::emm_basis.afex_aov

    load("data/tw_rm_data.rdata")

    # Joint tests
    tw_rm <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
      , anova_table = list(correction = "none")
    ))

    tw_rm_output <- suppressWarnings(apa_print(tw_rm))

    tw_rm_emm <- emmeans::emmeans(tw_rm$aov, ~ Task * Valence)

    ## All terms
    emm_aov <- emmeans::joint_tests(tw_rm_emm)
    emm_aov_output <- apa_print(emm_aov)

    expect_apa_results(
      emm_aov_output
      , labels = list(
        term = "Effect"
        , statistic = "$F$"
        , df = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value = "$p$"
      )
      , term_names = names(tw_rm_output$estimate)
    )

    expect_apa_term(
      emm_aov_output
      , term = "Task"
      , estimate = NULL
      , statistic = tw_rm_output$statistic$Task
    )

    ## Split by
    emm_split_aov <- emmeans::joint_tests(tw_rm_emm, by = "Task")
    emm_split_aov_output <- apa_print(emm_split_aov)

    expect_apa_results(
      emm_split_aov_output
      , labels = list(
        Task = "Task"
        , term = "Effect"
        , statistic = "$F$"
        , df = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value = "$p$"
      )
      , term_names = paste("Valence", levels(tw_rm_data$Task), sep = "_")
      , table_terms = rep("Valence", 2) # Levels split by are in "Task" column
    )

    expect_apa_term(
      emm_split_aov_output
      , term = "Valence_Cued"
      , estimate = NULL
      , statistic = "$F(2, 15.58) = 1.46$, $p = .263$"
    )


    # Ensure proper sorting of terms
    load("data/mixed_data.rdata")
    unsorted_aov <- afex::aov_4(formula = Recall ~ Gender * Dosage * (Task * Valence |Subject), data = mixed_data, fun_aggregate = mean)

    unsorted_emm <- emmeans::joint_tests(unsorted_aov, by = "Gender")
    apa_out <- apa_print(unsorted_emm)

    expect_apa_results(
      apa_out
      , labels = list(
        Gender = "Gender"
        , term = "Effect"
        , statistic = "$F$"
        , df = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value = "$p$"
      )
      , term_names = papaja:::sanitize_terms(paste(unlabel(gsub(apa_out$table$term, pattern =  " $\\times$ ", replacement = "_", fixed = TRUE)), apa_out$table$Gender, sep = "_"))
      , table_terms = beautify_terms(data.frame(unsorted_emm)$model.term)
    )
  }
)

test_that(
  "Estimate name guessing"
  , {
    # ANOVA
    library("emmeans")
    emm_basis.afex_aov <- afex:::emm_basis.afex_aov

    load("data/tw_rm_data.rdata")
    tw_rm <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
    ))

    ow_me_lsm <- lsmeans(tw_rm, ~ Valence)
    expect_identical(est_name_from_call(ow_me_lsm), "M")

    tw_int_lsm <- lsmeans(tw_rm, ~ Valence * Task)
    expect_identical(est_name_from_call(tw_int_lsm), "M")

    ow_pairs_lsm <- pairs(ow_me_lsm)
    expect_identical(est_name_from_call(ow_pairs_lsm), "\\Delta M")

    ow_pairs_lsm_2 <- contrast(ow_me_lsm, interaction = "pairwise")
    expect_identical(est_name_from_call(ow_pairs_lsm_2), "\\Delta M")


    ## Bug reported by shirdekel, #456
    tw_pairs_lsm <- pairs(tw_int_lsm)
    expect_identical(est_name_from_call(tw_pairs_lsm), "\\Delta M")

    tw_pairs_lsm_2 <- contrast(tw_int_lsm, interaction = "pairwise")
    expect_identical(est_name_from_call(tw_pairs_lsm_2), "\\Delta M")

    tw_int_emm <- emmeans(tw_rm, ~ Valence * Task)

    tw_pairs_emm <- pairs(tw_int_emm)
    expect_identical(est_name_from_call(tw_pairs_emm), "\\Delta M")

    tw_pairs_emm_2 <- contrast(tw_int_emm, interaction = "pairwise")
    expect_identical(est_name_from_call(tw_pairs_emm_2), "\\Delta M")


    ## Univeriate EMM
    afex::afex_options(emmeans_model = "univariate")

    uni_tw_me_emm <- emmeans(tw_rm, ~ Valence)
    expect_identical(est_name_from_call(uni_tw_me_emm), "M")

    uni_tw_me_consec_emm <- emmeans(tw_rm, consec ~ Valence)
    expect_identical(est_name_from_call(uni_tw_me_consec_emm$emmeans), "M")
    expect_identical(est_name_from_call(uni_tw_me_consec_emm$contrasts), "\\Delta M")

    uni_tw_pairs_emm <- pairs(emmeans(tw_rm, ~ Valence))
    expect_identical(est_name_from_call(uni_tw_pairs_emm), "\\Delta M")

    tw_me_emm <- emmeans(tw_rm, ~ Valence* Task)
    tw_pairs_contrasts_emm <- contrast(tw_me_emm, "consec", simple = "each")
    expect_identical(est_name_from_call(tw_pairs_contrasts_emm$`simple contrasts for Valence`), "\\Delta M")

    tw_pairs_contrasts_emm2 <- contrast(tw_me_emm, "consec", simple = "each", combine = TRUE)
    expect_identical(est_name_from_call(tw_pairs_contrasts_emm2), "\\Delta M")


    ## Multivariate EMM
    afex::afex_options(emmeans_model = "multivariate")

    mw_tw_me_emm <- emmeans(tw_rm, ~ Valence)
    expect_identical(est_name_from_call(uni_tw_me_emm), "M")

    mw_tw_pairs_emm <- pairs(emmeans(tw_rm, ~ Valence))
    expect_identical(est_name_from_call(mw_tw_pairs_emm), "\\Delta M")

    # GLM(M)
    ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    weight <- c(ctl, trt)
    lm.D9 <- lm(weight ~ group)

    lm_emm <- emmeans(lm.D9, pairwise~group)
    expect_identical(est_name_from_call(lm_emm$emmeans), "M")
    expect_identical(est_name_from_call(lm_emm$contrasts), "\\Delta M")


    glmm <- lme4::glmer(
      cbind(incidence, size - incidence) ~ period + (1 | herd)
      , data = lme4::cbpp
      , family = binomial(link = "logit")
    )

    glmm_emm_link <- emmeans(glmm, ~ period, type = "link")
    expect_identical(est_name_from_call(glmm_emm_link), "\\mathrm{logit}(p)")
    glmm_pairs_link <- pairs(glmm_emm_link)
    expect_identical(est_name_from_call(glmm_pairs_link), "\\log(\\mathit{OR})")

    glmm_emm_resp <- emmeans(glmm, ~ period, type = "response")
    expect_identical(est_name_from_call(glmm_emm_resp), "p")

    glmm_pairs_resp <- pairs(glmm_emm_resp)
    expect_identical(est_name_from_call(glmm_pairs_resp), "\\mathit{OR}")
    glmm_pairs_resp2 <- pairs(glmm_emm_resp, ratios = FALSE)
    expect_identical(est_name_from_call(glmm_pairs_resp2), "\\Delta \\mathrm{logit}(p)")


    glmm2 <- lme4::glmer(
      cbind(incidence, size - incidence) ~ period + (1 | herd)
      , data = lme4::cbpp
      , family = binomial(link = "probit")
    )

    glmm2_emm_link <- emmeans(glmm2, ~ period, type = "link")
    expect_identical(est_name_from_call(glmm2_emm_link), "\\Phi^{-1}(p)")
    glmm2_pairs_link <- pairs(glmm2_emm_link)
    expect_identical(est_name_from_call(glmm2_pairs_link), "\\Delta \\Phi^{-1}(p)")

    glmm2_emm_resp <- emmeans(glmm2, ~ period, type = "response")
    expect_identical(est_name_from_call(glmm2_emm_resp), "p")

    glmm2_pairs_resp <- pairs(glmm2_emm_resp)
    expect_identical(est_name_from_call(glmm2_pairs_resp), "\\Delta \\Phi^{-1}(p)")


    ## Dobson (1990) Page 93: Randomized Controlled Trial :
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    treatment <- gl(3,3)
    glm_mod <- glm(counts ~ outcome + treatment, family = poisson())

    glm_emm_link <- emmeans(glm_mod, ~ treatment, type = "link")
    expect_identical(est_name_from_call(glm_emm_link), "\\log(M)")

    glm_pairs_link <- pairs(glm_emm_link)
    expect_identical(est_name_from_call(glm_pairs_link), "\\Delta \\log(M)")

    glm_emm_resp <- emmeans(glm_mod, ~ treatment, type = "response")
    expect_identical(est_name_from_call(glm_emm_resp), "M")

    glm_pairs_resp <- pairs(glm_emm_resp)
    expect_identical(est_name_from_call(glm_pairs_resp), "M_{i}/M_{j}")

    glm_pairs_resp2 <- pairs(glm_emm_resp, ratio = FALSE)
    expect_identical(est_name_from_call(glm_pairs_link), "\\Delta \\log(M)")
  }
)

test_that(
  "Multiplicity adjustment notes"
  , {
    emm_basis.afex_aov <- afex:::emm_basis.afex_aov

    load("data/tw_rm_data.rdata")
    tw_rm <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
    ))

    ow_me_emm <- emmeans(tw_rm, ~ Valence | Task)

    ow_me_emm_bonf <- summary(ow_me_emm, infer = TRUE, adjust = "bonferroni")
    ow_me_emm_bonf_res <- apa_print(ow_me_emm_bonf)
    expect_equal(
      variable_label(ow_me_emm_bonf_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Bonferroni(3)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Bonferroni(3)}$"
      )
    )

    ow_me_emm_tukey <- pairs(ow_me_emm, infer = TRUE)
    ow_me_emm_tukey_res <- apa_print(ow_me_emm_tukey)
    expect_equal(
      variable_label(ow_me_emm_tukey_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Tukey(3)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Tukey(3)}$"
      )
    )

    ow_me_emm_holm <- contrast(ow_me_emm, infer = TRUE, adjust = "holm")
    ow_me_emm_holm_res <- apa_print(ow_me_emm_holm)
    expect_equal(
      variable_label(ow_me_emm_holm_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Bonferroni(3)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Holm(3)}$"
      )
    )

    ow_me_emm_fdr <- contrast(ow_me_emm, infer = TRUE, adjust = "fdr")
    ow_me_emm_fdr_res <- apa_print(ow_me_emm_fdr)
    expect_equal(
      variable_label(ow_me_emm_fdr_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Bonferroni(3)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize FDR(3)}$"
      )
    )

    ow_me_emm_dun <- contrast(ow_me_emm, infer = TRUE, adjust = "dunnettx")
    ow_me_emm_dun_res <- apa_print(ow_me_emm_dun)
    expect_equal(
      variable_label(ow_me_emm_dun_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Dunnett(3)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Dunnett(3)}$"
      )
    )

    ow_me_emm_scheffe <- summary(ow_me_emm, infer = TRUE, adjust = "scheffe")
    ow_me_emm_scheffe_res <- apa_print(ow_me_emm_scheffe)
    expect_equal(
      variable_label(ow_me_emm_scheffe_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Scheff\\'e(3)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Scheff\\'e(3)}$"
      )
    )

    ow_me_emm_scheffe <- emmeans(ow_me_emm, 1 ~ Valence | Task, adjust = "scheffe")
    ow_me_emm_scheffe_res <- apa_print(ow_me_emm_scheffe)
    expect_equal(
      variable_label(ow_me_emm_scheffe_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Scheff\\'e(3)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Scheff\\'e(3)}$"
      )
    )

    ow_me_emm_scheffe <- pairs(ow_me_emm, infer = TRUE, adjust = "scheffe")
    ow_me_emm_scheffe_res <- apa_print(ow_me_emm_scheffe)
    expect_equal(
      variable_label(ow_me_emm_scheffe_res$table[, c("conf.int", "adj.p.value")])
      , list(
        conf.int = "$95\\%\\ \\mathrm{CI}_\\mathrm{\\scriptsize Scheff\\'e(2)}$"
        , adj.p.value = "$p_\\mathrm{\\scriptsize Scheff\\'e(2)}$"
      )
    )


    family_mark_reference <- letters[
      rep(1:nlevels(tw_rm_data$Task), each = nlevels(tw_rm_data$Valence))
    ]

    family_marks <- .str_extract_first(
      .str_extract_first(
        ow_me_emm_bonf_res$table$conf.int
        , "[a-z]\\$$"
      )
      , "[a-z]"
    )
    family_marks <- unlabel(family_marks)

    expect_equal(
      family_marks
      , family_mark_reference
    )

    family_marks <- .str_extract_first(
      .str_extract_first(
        ow_me_emm_bonf_res$table$adj.p.value
        , "[a-z]\\$$"
      )
      , "[a-z]"
    )
    family_marks <- unlabel(family_marks)

    expect_equal(
      family_marks
      , family_mark_reference
    )

    ow_me_emm <- emmeans(tw_rm, ~ Task | Valence)
    ow_me_emm_bonf <- summary(ow_me_emm, infer = TRUE, adjust = "bonferroni")
    ow_me_emm_bonf_res <- apa_print(ow_me_emm_bonf)

    family_mark_reference <- letters[
      rep(1:nlevels(tw_rm_data$Valence), each = nlevels(tw_rm_data$Task))
    ]

    family_marks <- .str_extract_first(
      .str_extract_first(
        ow_me_emm_bonf_res$table$conf.int
        , "[a-z]\\$$"
      )
      , "[a-z]"
    )
    family_marks <- unlabel(family_marks)

    expect_equal(
      family_marks
      , family_mark_reference
    )

    family_marks <- .str_extract_first(
      .str_extract_first(
        ow_me_emm_bonf_res$table$adj.p.value
        , "[a-z]\\$$"
      )
      , "[a-z]"
    )
    family_marks <- unlabel(family_marks)

    expect_equal(
      family_marks
      , family_mark_reference
    )


    df <- data.frame(
      errors = floor(runif(n=320,min=0,max=30))
      , session=c("t1","t2","t3","t4")
    )

    glmm <- glm(errors ~ session, df, family = "poisson")
    glmm_pairs <- emmeans(glmm, pairwise~session, type="response")
    glmm_pairs_res <- apa_print(glmm_pairs$contrasts)

    expect_false(any(grepl("[a-z]", glmm_pairs_res$table$conf.int)))
    expect_false(any(grepl("[a-z]", glmm_pairs_res$table$p.value)))
  }
)

test_that(
  "Regression"
  , {
    # Typesetting of numeric predictor values
    # https://github.com/crsh/papaja/issues/445
    iris$Sepal.Length <- iris$Sepal.Length * 1000
    my_lm <- lm(
      Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length
      , data = iris
    )
    ats <- c(1000, 2000, 3000)
    my_lm_emm <- emmeans::emmeans(
      my_lm
      , ~Sepal.Length
      , at = list(Sepal.Length = ats)
    )

    my_lm_emm_output <- apa_print(my_lm_emm)

    expect_apa_results(
      my_lm_emm_output
      , labels = list(
        Sepal.Length = "Sepal.Length"
        , estimate = "$M$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
      , term_names = c("X1000", "X2000", "X3000")
    )

    expect_apa_term(
      my_lm_emm_output
      , term = "X1000"
      , estimate = "$M = 0.12$, 95\\% CI $[-0.48, 0.71]$"
      , statistic = "$t(146) = 0.39$, $p = .699$"
    )

    expect_true(
      all(my_lm_emm_output$table$Sepal.Length == printnum(ats))
    )


    # https://github.com/crsh/papaja/issues/200
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    treatment <- gl(3,3)
    glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())

    emm_glm <- emmeans::emmeans(glm.D93, pairwise~treatment)
    emm_glm_output <- apa_print(emm_glm$emmeans)

    expect_apa_results(
      emm_glm_output
      , labels = list(
        treatment = "treatment"
        , estimate = "$\\log(M)$"
        , conf.int = "95\\% CI"
        , statistic = "$z$"
        , p.value = "$p$"
      )
      , term_names = c("X1", "X2", "X3")
    )

    expect_apa_term(
      emm_glm_output
      , term = "X1"
      , estimate = "$\\log(M) = 2.80$, 95\\% CI $[2.52, 3.07]$"
      , statistic = "$z = 19.65$, $p < .001$"
    )

#     pigs_lm <- lm(log(conc) ~ source * percent, data = emmeans::pigs)
#
#     pigs_lm_emm <- emmeans::emmeans(pigs_lm, ~ source)
#     pigs_pairs_emm_output <- apa_print(pairs(pigs_lm_emm, type = "response"))
#
#     # table --------------------------------------------------------------------
#     expect_identical(
#       object = pigs_pairs_emm_output$table$estimate
#       , expected = structure(
#         c("0.77", "0.66", "0.86")
#         , label = "Ratio"
#         , class = c("tiny_labelled", "character")
#       )
#     )
#
#
#     noise.lm <- lm(noise ~ size * type * side, data = emmeans::auto.noise)
#     noise.emm <- emmeans::emmeans(noise.lm, ~ size * side * type)
#     apa_print(emmeans::contrast(noise.emm, "consec", simple = "each", combine = TRUE, adjust = "mvt")) # Table order is fucked up
#
#
#     org.int <- lm(cbind(sales1, sales2) ~ price1 * price2 + day + store, data = emmeans::oranges)
#     emmeans::emtrends(org.int, ~ variety, var = "price1", mult.name = "variety")
#     emmeans::emtrends(org.int, pairwise ~ variety, var = "price1", mult.name = "variety")
#
  }
)


test_that(
  "emtrends"
  , {
    skip("emtrends() is not yet supported.")

    library("emmeans")

    cars_lm <- lm(mpg ~ qsec, data = mtcars)
    summary(cars_lm)

    cars_em <- emtrends(cars_lm, ~ 1, var = "qsec")
    onesided <- summary(cars_em, infer = TRUE, side = ">")

    apa_print(onesided)

    # broom::tidy(summary(cars_em, infer = TRUE, side = ">"))
  }
)
