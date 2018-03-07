context("apa_print() for least squares means")

test_that(
  "Two-way repeated-measures ANOVA"
  , {
    load("data/tw_rm_data.rdata")
    tw_rm <- suppressWarnings(afex::aov_ez(
      data = tw_rm_data
      , id = "Subject"
      , dv = "Recall"
      , within = c("Task", "Valence")
      , return = "aov"
    ))

    tw_rm_lsm <- lsmeans::lsmeans(tw_rm, ~ Valence)
    tw_rm_emm <- emmeans::emmeans(tw_rm, ~ Valence)

    tw_rm_lsm_output <- apa_print(tw_rm_lsm)
    tw_rm_emm_output <- apa_print(tw_rm_emm)
    tw_rm_lsm_output2 <- apa_print(summary(tw_rm_lsm, infer = c(T, T)))
    tw_rm_emm_output2 <- apa_print(summary(tw_rm_emm, infer = c(T, T)))

    expect_identical(tw_rm_lsm_output, tw_rm_lsm_output2)
    expect_identical(tw_rm_emm_output, tw_rm_emm_output2)
    expect_identical(tw_rm_lsm_output, tw_rm_emm_output2)

    expect_is(tw_rm_lsm_output, "list")
    expect_equal(names(tw_rm_lsm_output), container_names)

    # stat
    expect_is(tw_rm_lsm_output$stat, "list")
    # expect_equal(names(tw_rm_lsm_output$stat), c("Task", "Valence", "Task_Valence"))
    # expect_is(tw_rm_lsm_output$stat$Task, "character")
    # expect_equal(tw_rm_lsm_output$stat$Task, "$F(1, 4) = 7.35$, $\\mathit{MSE} = 4.08$, $p = .054$")
    # expect_is(tw_rm_lsm_output$stat$Valence, "character")
    # expect_equal(tw_rm_lsm_output$stat$Valence, "$F(2, 8) = 1.46$, $\\mathit{MSE} = 3.36$, $p = .288$")

    # est
    expect_is(tw_rm_lsm_output$est, "list")
    # expect_equal(names(tw_rm_lsm_output$est), c("Task", "Valence", "Task_Valence"))
    # expect_is(tw_rm_lsm_output$est$Task, "character")
    # expect_equal(tw_rm_lsm_output$est$Task, "$\\hat{\\eta}^2_G = .068$")
    # expect_is(tw_rm_lsm_output$est$Valence, "character")
    # expect_equal(tw_rm_lsm_output$est$Valence, "$\\hat{\\eta}^2_G = .023$")

    # full
    expect_is(tw_rm_lsm_output$full, "list")
    # expect_equal(names(tw_rm_lsm_output$full), c("Task", "Valence", "Task_Valence"))
    # expect_is(tw_rm_lsm_output$full$Task, "character")
    # expect_equal(tw_rm_lsm_output$full$Task, "$F(1, 4) = 7.35$, $\\mathit{MSE} = 4.08$, $p = .054$, $\\hat{\\eta}^2_G = .068$")
    # expect_is(tw_rm_lsm_output$full$Valence, "character")
    # expect_equal(tw_rm_lsm_output$full$Valence, "$F(2, 8) = 1.46$, $\\mathit{MSE} = 3.36$, $p = .288$, $\\hat{\\eta}^2_G = .023$")
    # expect_is(tw_rm_lsm_output$full$Task_Valence, "character")
    # expect_equal(tw_rm_lsm_output$full$Task_Valence, "$F(2, 8) = 0.29$, $\\mathit{MSE} = 2.41$, $p = .755$, $\\hat{\\eta}^2_G = .003$")

    # table --------------------------------------------------------------------
    expect_identical(
      object = tw_rm_lsm_output$table$estimate
      , expected = structure(
        c("11.00", "12.10", "12.30")
        , label = "$\\Delta M$"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_rm_lsm_output$table$ci
      , expected = structure(
        c("$[6.34$, $15.66]$",  "$[7.44$, $16.76]$", "$[7.64$, $16.96]$")
        , label = "95\\% CI"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_rm_lsm_output$table$statistic
      , expected = structure(
        c("6.21", "6.84", "6.95")
        , label = "$t(4.63)$"
        , class = c("labelled", "character")
      )
    )
    expect_identical(
      object = tw_rm_lsm_output$table$p.value
      , expected = structure(
        c(".002", ".001", ".001")
        , label = "$p$"
        , class = c("labelled", "character")
      )
    )


    # apa_print(summary(tw_rm_lsm, infer = c(F, T)))
    # apa_print(summary(tw_rm_lsm, infer = c(T, F)))
    expect_error(apa_print(summary(tw_rm_lsm, infer = c(F, F))))
  }
)
