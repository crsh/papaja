context("apa_print() for emmeans/lsmeans")

# test_that(
#   "Regression"
#   , {
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
#         , class = c("papaja_labelled", "character")
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
#   }
# )

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

    tw_me_lsm <- lsmeans::lsmeans(tw_rm, ~ Valence)
    tw_me_emm <- emmeans::emmeans(tw_rm, ~ Valence)

    tw_me_lsm_output <- apa_print(tw_me_lsm)
    tw_me_emm_output <- apa_print(tw_me_emm)
    tw_me_lsm_output2 <- apa_print(
      summary(tw_me_lsm, infer = TRUE)
      , est_name = "M"
    )
    tw_me_emm_output2 <- apa_print(
      summary(tw_me_emm, infer = TRUE)
      , est_name = "M"
    )

    expect_identical(tw_me_lsm_output, tw_me_lsm_output2)
    expect_identical(tw_me_emm_output, tw_me_emm_output2)
    expect_identical(tw_me_lsm_output, tw_me_emm_output2)

    expect_is(tw_me_lsm_output, "list")
    expect_equal(names(tw_me_lsm_output), container_names)

    # table --------------------------------------------------------------------
    expect_identical(
      object = tw_me_lsm_output$table$estimate
      , expected = structure(
        c("11.00", "12.10", "12.30")
        , label = "$M$"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_lsm_output$table$ci
      , expected = structure(
        c("$[6.34$, $15.66]$",  "$[7.44$, $16.76]$", "$[7.64$, $16.96]$")
        , label = "95\\% CI"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_lsm_output$table$statistic
      , expected = structure(
        c("6.21", "6.84", "6.95")
        , label = "$t(4.63)$"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_lsm_output$table$p.value
      , expected = structure(
        c(".002", ".001", ".001")
        , label = "$p$"
        , class = c("papaja_labelled", "character")
      )
    )


    expect_warning(
      tw_me_emm_p_output <- apa_print(
        summary(tw_me_emm, infer = c(FALSE, TRUE))
        , est_name = "M"
      )
    )
    expect_identical(
      tw_me_lsm_output$table[, c("Valence", "estimate", "statistic", "p.value")]
      , tw_me_emm_p_output$table
    )

    expect_warning(
      tw_me_emm_ci_output <- apa_print(
        summary(tw_me_emm, infer = c(TRUE, FALSE))
        , est_name = "M"
      )
    )
    expect_identical(
      tw_me_lsm_output$table[, c("Valence", "estimate", "ci")]
      , tw_me_emm_ci_output$table
    )

    expect_error(apa_print(summary(tw_me_lsm, infer = c(FALSE, FALSE))))


    # Interaction
    tw_int_lsm <- lsmeans::lsmeans(tw_rm, ~ Valence * Task)
    tw_int_emm <- emmeans::emmeans(tw_rm, ~ Valence * Task)

    tw_int_lsm_output <- apa_print(tw_int_lsm)
    tw_int_emm_output <- apa_print(tw_int_emm)
    tw_int_lsm_output2 <- apa_print(
      summary(tw_int_lsm, infer = TRUE)
      , est_name = "M"
    )
    tw_int_emm_output2 <- apa_print(
      summary(tw_int_emm, infer = TRUE)
      , est_name = "M"
    )

    expect_identical(tw_int_lsm_output, tw_int_lsm_output2)
    expect_identical(tw_int_emm_output, tw_int_emm_output2)
    expect_identical(tw_int_lsm_output, tw_int_emm_output2)

    # table --------------------------------------------------------------------
    expect_identical(
      object = tw_int_emm_output$table$estimate
      , expected = structure(
        c("11.80", "13.00", "13.60", "10.20", "11.20", "11.00")
        , label = "$M$"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_int_emm_output$table$ci
      , expected = structure(
        c("$[7.17$, $16.43]$",  "$[8.37$, $17.63]$", "$[8.97$, $18.23]$", "$[5.57$, $14.83]$", "$[6.57$, $15.83]$", "$[6.37$, $15.63]$")
        , label = "95\\% CI"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_int_emm_output$table$statistic
      , expected = structure(
        c("6.37", "7.02", "7.34", "5.51", "6.05", "5.94")
        , label = "$t(5.52)$"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_int_emm_output$table$p.value
      , expected = structure(
        c(".001", ".001", "< .001", ".002", ".001", ".001")
        , label = "$p$"
        , class = c("papaja_labelled", "character")
      )
    )


    # Simple effects
    tw_se_lsm <- lsmeans::lsmeans(tw_rm, ~ Valence | Task)
    tw_se_emm <- emmeans::emmeans(tw_rm, ~ Valence | Task)

    tw_se_lsm_output <- apa_print(tw_se_lsm)
    tw_se_emm_output <- apa_print(tw_se_emm)
    tw_se_lsm_output2 <- apa_print(
      summary(tw_se_lsm, infer = TRUE)
      , est_name = "M"
    )
    tw_se_emm_output2 <- apa_print(
      summary(tw_se_emm, infer = TRUE)
      , est_name = "M"
    )

    expect_identical(tw_se_lsm_output, tw_se_lsm_output2)
    expect_identical(tw_se_emm_output, tw_se_emm_output2)
    expect_identical(tw_se_lsm_output, tw_se_emm_output2)

    # Sort
    tw_int_emm_output$estimate <- tw_int_emm_output$estimate[names(tw_se_emm_output$estimate)]
    tw_int_emm_output$statistic <- tw_int_emm_output$statistic[names(tw_int_emm_output$estimate)]
    tw_int_emm_output$full_result <- tw_int_emm_output$full_result[names(tw_int_emm_output$estimate)]
    tw_int_emm_output$table <- tw_int_emm_output$table[rownames(tw_se_emm_output$table), ]

    expect_identical(tw_se_emm_output, tw_int_emm_output)


    # Complex output
    data(obk.long, package = "afex")

    fw_mixed <- suppressWarnings(afex::aov_ez(
      id = "id"
      , dv = "value"
      , data = obk.long
      , between = c("treatment", "gender")
      , within = c("phase", "hour")
      , observed = "gender"
    ))

    fw_mixed_lsm <- lsmeans::lsmeans(fw_mixed, ~ phase * hour | treatment * gender)
    fw_mixed_emm <- emmeans::emmeans(fw_mixed, ~ phase * hour | treatment * gender)

    fw_mixed_lsm_output <- apa_print(fw_mixed_lsm)
    fw_mixed_emm_output <- apa_print(fw_mixed_emm)
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

    # table --------------------------------------------------------------------
    expect_identical(
      object = fw_mixed_emm_output$table$gender
      , expected = c(
        "F"
        , rep("", nrow(fw_mixed_emm_output$table) / 2 - 1)
        , "M"
        , rep("", nrow(fw_mixed_emm_output$table) / 2 - 1)
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$treatment
      , expected = rep(
        c(
          "control"
          , rep("", nrow(fw_mixed_emm_output$table) / 6 - 1)
          , "A"
          , rep("", nrow(fw_mixed_emm_output$table) / 6 - 1)
          , "B"
          , rep("", nrow(fw_mixed_emm_output$table) / 6 - 1)
        )
        , 2
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$hour
      , expected = rep(
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
    )

    expect_identical(
      object = fw_mixed_emm_output$table$phase
      , expected = rep(
        c("fup", "post", "pre")
        , 30
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$estimate
      , expected = structure(
        printnum(as.data.frame(summary(fw_mixed_emm, infer = c(T, T)))$emmean)
        , label = "$M$"
        , class = c("papaja_labelled", "character")
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$ci
      , expected = structure(
        unname(
          unlist(
            apply(
              as.data.frame(summary(fw_mixed_emm, infer = c(T, T)))[, c("lower.CL", "upper.CL")]
              , 1
              , papaja:::print_confint
            )
          )
        )
        , label = "95\\% CI"
        , class = c("papaja_labelled", "character")
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$statistic
      , expected = structure(
        printnum(as.data.frame(summary(fw_mixed_emm, infer = c(T, T)))$t.ratio)
        , label = "$t$"
        , class = c("papaja_labelled", "character")
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$p.value
      , expected = structure(
        printp(as.data.frame(summary(fw_mixed_emm, infer = c(T, T)))$p.value)
        , label = "$p$"
        , class = c("papaja_labelled", "character")
      )
    )


    # Pairs
    tw_me_pairs_emm <- pairs(tw_me_emm)
    tw_me_pairs_emm_output <- apa_print(tw_me_pairs_emm)

    # table --------------------------------------------------------------------
    expect_identical(
      object = tw_me_pairs_emm_output$table$estimate
      , expected = structure(
        c("-1.10", "-1.30", "-0.20")
        , label = "$\\Delta M$"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_pairs_emm_output$table$ci
      , expected = structure(
        c("$[-3.44$, $1.24]$",  "$[-3.64$, $1.04]$", "$[-2.54$, $2.14]$")
        , label = "95\\% CI"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_pairs_emm_output$table$statistic
      , expected = structure(
        c("-1.34", "-1.59", "-0.24")
        , label = "$t(8)$"
        , class = c("papaja_labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_pairs_emm_output$table$p.value
      , expected = structure(
        c(".413", ".305", ".968")
        , label = "$p$"
        , class = c("papaja_labelled", "character")
      )
    )

    ## Custom contrast names
    tw_me_pairs_emm_output <- apa_print(tw_me_pairs_emm, contrast_names = letters[1:3])
    expect_identical(
      object = tw_me_pairs_emm_output$table$contrast
      , expected = structure(
        letters[1:3]
        , label = "Contrast"
        , class = c("papaja_labelled", "character")
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

    expect_identical(tw_se_contrast_emm_output, tw_se_contrast_emm_output2)

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

    expect_equal(names(simple_pairs_output$estimate), c("control_F_M", "A_F_M", "B_F_M"))
    expect_identical(simple_pairs_output, simple_pairs2_output)

    simple_contrasts <- emmeans::contrast(tw_between_emm, "consec", simple = "each", combine = TRUE, adjust = "none")
    simple_contrasts_output <- apa_print(simple_contrasts)

    simple_contrasts2 <- emmeans::contrast(tw_between_emm, "consec", simple = "each", adjust = "none")
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
    emm_aov <- emmeans::joint_tests(tw_rm_emm)

    # emm_aov_output <- apa_print(emm_aov)

    emm_split_aov <- emmeans::joint_tests(tw_rm_emm, by = "Task")

    # emm_split_aov_output <- apa_print(emm_split_aov)
  }
)

test_that(
  "Estimate name guessing"
  , {
    emm_basis.afex_aov <- afex:::emm_basis.afex_aov

    load("data/tw_rm_data.rdata")
    tw_rm <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
    ))

    ow_me_lsm <- lsmeans::lsmeans(tw_rm, ~ Valence)
    expect_equal(est_name_from_call(ow_me_lsm), "M")

    tw_me_lsm <- lsmeans::lsmeans(tw_rm, ~ Valence * Task)
    expect_equal(est_name_from_call(tw_me_lsm), "M")

    ow_pairs_lsm <- pairs(ow_me_lsm)
    expect_equal(est_name_from_call(ow_pairs_lsm), "\\Delta M")

    afex::afex_options(emmeans_model = "univariate")

    uni_tw_me_emm <- emmeans::emmeans(tw_rm, ~ Valence)
    expect_equal(est_name_from_call(uni_tw_me_emm), "M")

    uni_tw_me_consec_emm <- emmeans::emmeans(tw_rm, consec ~ Valence)
    expect_equal(est_name_from_call(uni_tw_me_consec_emm$emmeans), "M")
    expect_equal(est_name_from_call(uni_tw_me_consec_emm$contrasts), "\\Delta M")

    uni_tw_pairs_emm <- pairs(emmeans::emmeans(tw_rm, ~ Valence))
    expect_equal(est_name_from_call(uni_tw_pairs_emm), "\\Delta M")

    tw_me_emm <- emmeans::emmeans(tw_rm, ~ Valence* Task)
    tw_pairs_contrasts_emm <- emmeans::contrast(tw_me_emm, "consec", simple = "each")
    expect_equal(est_name_from_call(tw_pairs_contrasts_emm$`simple contrasts for Valence`), "\\Delta M")

    tw_pairs_contrasts_emm2 <- emmeans::contrast(tw_me_emm, "consec", simple = "each", combine= TRUE)
    expect_equal(est_name_from_call(tw_pairs_contrasts_emm2), "\\Delta M")

    afex::afex_options(emmeans_model = "multivariate")

    mw_tw_me_emm <- emmeans::emmeans(tw_rm, ~ Valence)
    expect_equal(est_name_from_call(uni_tw_me_emm), "M")

    mw_tw_pairs_emm <- pairs(emmeans::emmeans(tw_rm, ~ Valence))
    expect_equal(est_name_from_call(mw_tw_pairs_emm), "\\Delta M")
  }
)
