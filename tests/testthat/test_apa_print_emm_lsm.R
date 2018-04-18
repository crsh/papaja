context("apa_print() for least squares means")

test_that(
  "Two-way repeated-measures ANOVA"
  , {
    emm_basis.afex_aov <- afex:::emm_basis.afex_aov

    load("data/tw_rm_data.rdata")
    tw_rm <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
    ))

    tw_me_lsm <- lsmeans::lsmeans(tw_rm$aov, ~ Valence)
    tw_me_emm <- emmeans::emmeans(tw_rm, ~ Valence)

    tw_me_lsm_output <- apa_print(tw_me_lsm)
    tw_me_emm_output <- apa_print(tw_me_emm)
    tw_me_lsm_output2 <- apa_print(summary(tw_me_lsm, infer = c(T, T)))
    tw_me_emm_output2 <- apa_print(summary(tw_me_emm, infer = c(T, T)))

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
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_lsm_output$table$ci
      , expected = structure(
        c("$[6.34$, $15.66]$",  "$[7.44$, $16.76]$", "$[7.64$, $16.96]$")
        , label = "95\\% CI"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_lsm_output$table$statistic
      , expected = structure(
        c("6.21", "6.84", "6.95")
        , label = "$t(4.63)$"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_me_lsm_output$table$p.value
      , expected = structure(
        c(".002", ".001", ".001")
        , label = "$p$"
        , class = c("labelled", "character")
      )
    )


    # apa_print(summary(tw_rm_lsm, infer = c(F, T)))
    # apa_print(summary(tw_rm_lsm, infer = c(T, F)))
    expect_error(apa_print(summary(tw_me_lsm, infer = c(F, F))))


    # Interaction
    tw_int_lsm <- lsmeans::lsmeans(tw_rm$aov, ~ Valence * Task)
    tw_int_emm <- emmeans::emmeans(tw_rm, ~ Valence * Task)

    tw_int_lsm_output <- apa_print(tw_int_lsm)
    tw_int_emm_output <- apa_print(tw_int_emm)
    tw_int_lsm_output2 <- apa_print(summary(tw_int_lsm, infer = c(T, T)))
    tw_int_emm_output2 <- apa_print(summary(tw_int_emm, infer = c(T, T)))

    expect_identical(tw_int_lsm_output, tw_int_lsm_output2)
    expect_identical(tw_int_emm_output, tw_int_emm_output2)
    expect_identical(tw_int_lsm_output, tw_int_emm_output2)

    # table --------------------------------------------------------------------
    expect_identical(
      object = tw_int_emm_output$table$estimate
      , expected = structure(
        c("11.80", "13.00", "13.60", "10.20", "11.20", "11.00")
        , label = "$M$"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_int_emm_output$table$ci
      , expected = structure(
        c("$[7.17$, $16.43]$",  "$[8.37$, $17.63]$", "$[8.97$, $18.23]$", "$[5.57$, $14.83]$", "$[6.57$, $15.83]$", "$[6.37$, $15.63]$")
        , label = "95\\% CI"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_int_emm_output$table$statistic
      , expected = structure(
        c("6.37", "7.02", "7.34", "5.51", "6.05", "5.94")
        , label = "$t$"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_int_emm_output$table$p.value
      , expected = structure(
        c(".001", ".001", "< .001", ".002", ".001", ".001")
        , label = "$p$"
        , class = c("labelled", "character")
      )
    )


    # Simple effects
    tw_se_lsm <- lsmeans::lsmeans(tw_rm$aov, ~ Valence | Task)
    tw_se_emm <- emmeans::emmeans(tw_rm, ~ Valence | Task)

    tw_se_lsm_output <- apa_print(tw_se_lsm)
    tw_se_emm_output <- apa_print(tw_se_emm)
    tw_se_lsm_output2 <- apa_print(summary(tw_se_lsm, infer = c(T, T)))
    tw_se_emm_output2 <- apa_print(summary(tw_se_emm, infer = c(T, T)))

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

    fw_mixed_lsm <- lsmeans::lsmeans(fw_mixed$aov, ~ phase * hour | treatment * gender)
    fw_mixed_emm <- emmeans::emmeans(fw_mixed, ~ phase * hour | treatment * gender)

    fw_mixed_lsm_output <- apa_print(fw_mixed_lsm)
    fw_mixed_emm_output <- apa_print(fw_mixed_emm)
    fw_mixed_lsm_output2 <- apa_print(summary(fw_mixed_lsm, infer = c(T, T)))
    fw_mixed_emm_output2 <- apa_print(summary(fw_mixed_emm, infer = c(T, T)))

    expect_identical(fw_mixed_lsm_output, fw_mixed_lsm_output2)
    expect_identical(fw_mixed_emm_output, fw_mixed_emm_output2)

    # Not identical because slightly different CI in some conditions in original objects
    # expect_identical(fw_mixed_lsm_output, fw_mixed_emm_output2)

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
        , class = c("labelled", "character")
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
        , class = c("labelled", "character")
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$statistic
      , expected = structure(
        printnum(as.data.frame(summary(fw_mixed_emm, infer = c(T, T)))$t.ratio)
        , label = "$t$"
        , class = c("labelled", "character")
      )
    )

    expect_identical(
      object = fw_mixed_emm_output$table$p.value
      , expected = structure(
        printp(as.data.frame(summary(fw_mixed_emm, infer = c(T, T)))$p.value)
        , label = "$p$"
        , class = c("labelled", "character")
      )
    )


    # Contrasts
    # data(obk.long, package = "afex")
    #
    # fw_mixed <- suppressWarnings(afex::aov_ez(
    #   id = "id"
    #   , dv = "value"
    #   , data = obk.long
    #   , between = c("treatment", "gender")
    #   , within = c("phase", "hour")
    #   , observed = "gender"
    # ))
    #
    # fw_mixed_lsm <- lsmeans::lsmeans(fw_mixed$aov, ~ phase * hour | treatment * gender)
    # fw_mixed_emm <- emmeans::emmeans(fw_mixed, ~ phase * hour | treatment * gender)
    #
    # fw_mixed_lsm_output <- apa_print(fw_mixed_lsm)
    # fw_mixed_emm_output <- apa_print(fw_mixed_emm)
    # fw_mixed_lsm_output2 <- apa_print(summary(fw_mixed_lsm, infer = c(T, T)))
    # fw_mixed_emm_output2 <- apa_print(summary(fw_mixed_emm, infer = c(T, T)))
    #
    # expect_identical(tw_me_lsm_output, tw_me_lsm_output2)
    # expect_identical(tw_me_emm_output, tw_me_emm_output2)
    # expect_identical(tw_me_lsm_output, tw_me_emm_output2)
  }
)
