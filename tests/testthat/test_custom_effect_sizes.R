
test_that(
  "Custom effect sizes for ANOVA"
  , {

    # Treat as between-subjects design, use Type-2 sums of squares
    aov_out <- aov(yield ~ N * P, npk)
    summary_aov <- summary(aov_out)

    ges <- function(x, generalized = TRUE, include_intercept = TRUE, ...) {
      effectsize::eta_squared(x, generalized = generalized, include_intercept = include_intercept, ...)
    }

    # 'aov/lm' class
    apa_with_function <- apa_print(aov_out, es = ges)
    apa_with_df <- apa_print(aov_out, es = ges(aov_out, generalized = TRUE, include_intercept = FALSE))
    apa_summary_aov <- apa_print(summary_aov, es = ges)

    expect_apa_results(
      apa_with_function
      , labels = list(
        term = "Effect"
        , estimate = "$\\hat{\\eta}^2_G$"
        , conf.int = "90\\% CI"
        , statistic = "$F$"
        , df = "$\\mathit{df}$"
        , df.residual ="$\\mathit{df}_{\\mathrm{res}}$"
        , p.value = "$p$"
      )
    )

    expect_identical(
      object = apa_with_df$estimate
      , expected = list(
        N = "$\\hat{\\eta}^2_G = .224$, 90\\% CI $[.017, .460]$"
        , P = "$\\hat{\\eta}^2_G = .013$, 90\\% CI $[.000, .181]$"
        , N_P = "$\\hat{\\eta}^2_G = .031$, 90\\% CI $[.000, .229]$"
      )
    )

    expect_identical(
      apa_with_function
      , apa_with_df
    )

    expect_identical(
      object = apa_summary_aov
      , expected = apa_with_df
    )


    # Treat as within-subjects design, use Type-3 sums of squares
    # 'afex_aov' class
    afex_out <- afex::aov_4(yield ~ (N * P | block), data = npk)

    afex_with_intercept <- apa_print(afex_out, es = ges, intercept = TRUE)
    afex_without_intercept <- expect_warning(
      apa_print(afex_out, es = ges(afex_out, include_intercept = FALSE), intercept = TRUE)
      , regexp = "Custom effect sizes were not available for some model terms. These have been dropped from the output object."
    )

    # aovlist/listof (be careful: within subjects, Type-2 sums of squares)
    aovlist_out    <- apa_print(afex_out$aov, es = ges)
    aovlist_out_df <- apa_print(afex_out$aov, es = ges(afex_out$aov))

    expect_identical(
      aovlist_out
      , aovlist_out_df
    )

    # Anova.mlm (Type-3 again, but only partial eta-squared available)
    Anova.mlm_out    <- expect_warning(apa_print(afex_out$Anova, es = ges))
    Anova.mlm_out_df <- expect_warning(apa_print(afex_out$Anova, es = ges(afex_out$Anova)))

    expect_identical(
      Anova.mlm_out
      , Anova.mlm_out_df
    )

    # anova from car::Anova
    car_Anova_lm <- car::Anova(lm(yield ~ N * P, npk), type = "II")

    expect_identical(
      apa_print(car_Anova_lm, es = ges)
      , apa_with_function
    )

    # anova from anova.lm()
    stats_anova_lm <- anova(lm(yield ~ N * P, npk))

    expect_identical(
      apa_print(stats_anova_lm, es = ges)
      , apa_with_function
    )

    # Non-supported output from summary()
    # summary.Anova.mlm
    expect_error(
      apa_print(summary(afex_out$Anova), es = ges)
      , regexp = "Cannot apply custom effect-size function to this class of object."
    )

  }
)

test_that(
  "Calculation of generalized eta squared"
  , {
    library("afex")
    library("effectsize")

    suppressMessages(set_sum_contrasts())
    data(obk.long)

    afex_aov_between <- aov_ez(
      "id"
      , "value"
      , obk.long
      , between = c("treatment", "gender")
      , observed = "gender"
      , fun_aggregate = mean
    )
    apa_afex <- apa_print(afex_aov_between)
    expect_identical(
      apa_afex$table$estimate
      , structure(
        c(".289", ".189", ".295")
        , label = "$\\hat{\\eta}^2_G$"
        , class = c("tiny_labelled", "character")
      )
    )
  }
)
