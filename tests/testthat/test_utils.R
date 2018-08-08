context("Utility functions")

test_that(
  "convert_stat_name()"
  , {
    chis <- c(
      convert_stat_name("X-squared")
      , convert_stat_name("Chi-squared")
      , convert_stat_name("chi-squared")
      , convert_stat_name("X^2")
    )
    expect_equal(chis, rep("\\chi^2", length(chis)))

    expect_equal(convert_stat_name("t"), "t")
    expect_equal(convert_stat_name("z"), "z")
  }
)


test_that(
  "print_confint()"
  , {
    x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
    y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

    cor_test <- cor.test(x, y)
    apa_confint <- print_confint(cor_test$conf.int, conf_level = 0.95)
    expect_is(apa_confint, "character")
    expect_equal(apa_confint, "95\\% CI $[-0.15$, $0.90]$")

    apa_confint <- print_confint(cor_test$conf.int, gt1 = FALSE)
    expect_equal(apa_confint, "95\\% CI $[-.15$, $.90]$")

    apa_confint <- print_confint(cor_test$conf.int)
    expect_equal(apa_confint, "95\\% CI $[-0.15$, $0.90]$")

    apa_confint <- print_confint(c(1, 2))
    expect_equal(apa_confint, "$[1.00$, $2.00]$")

    conf_int <- confint(lm(x ~ y))
    apa_confint <- print_confint(conf_int)

    expect_is(apa_confint, "list")
    expect_equal(length(apa_confint), nrow(conf_int))
    expect_equal(names(apa_confint), c("Intercept", "y"))
    expect_equal(apa_confint$Intercept, "95\\% CI $[19.52$, $51.78]$")
    expect_equal(apa_confint$y, "95\\% CI $[-0.95$, $7.67]$")
  }
)

test_that(
  "in_paren()"
  , {
    expect_equal(in_paren("$t(1) = 1$"), "$t[1] = 1$")
    expect_equal(in_paren("$\\chi^2(1, n = 100) = 1$"), "$\\chi^2[1, n = 100] = 1$")
    expect_equal(in_paren("95% CI $[123$, $123]$"), "95% CI $[123$, $123]$")
    expect_equal(in_paren("$F(1, 234) = 1$"), "$F[1, 234] = 1$")
  }
)

test_that(
  "determine_within_between"
  , {
    test <- determine_within_between(data = npk, id = "block", factors = c("N", "P", "K"))
    expect_equal(test, list(within = c("N", "P", "K"), between = NULL))

    data <- npk[2:16, ]
    test <- determine_within_between(data = data, id = "block", factors = c("N", "P", "K"))
    expect_identical(test, list(within = c("N", "P", "K"), between = NULL))
  }
)

test_that(
  "complete_observations"
  , {
    test <- complete_observations(data = npk, id = "block", dv = "yield", within = c("N", "P"))

    expect_identical(
      object = test
      , expected = npk
    )

    data <- npk
    data$yield[5] <- NA
    test <- papaja:::complete_observations(data = data, id = "block", dv = "yield", within = c("N", "K"))

    expect_identical(
      object = test
      , expected = structure(
        list(
          block = structure(
            c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L)
            , .Label = c("1", "3", "4", "5", "6")
            , class = "factor"
          )
          , N = structure(
            c(1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L)
            , .Label = c("0", "1"), class = "factor"
          )
          , P = structure(
            c(2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L)
            , .Label = c("0", "1")
            , class = "factor"
          )
          , K = structure(
            c(2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L)
            , .Label = c("0", "1")
            , class = "factor"
          )
          , yield = c(49.5, 62.8, 46.8, 57, 62.8, 55.8, 69.5, 55, 62, 48.8, 45.5, 44.2, 52, 51.5, 49.8, 48.8, 57.2, 59, 53.2, 56)
        )
        , .Names = c("block", "N", "P", "K", "yield")
        , row.names = c(1L, 2L, 3L, 4L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L)
        , class = "data.frame"
        , removed_cases_explicit_NA = c("2")
      )
    )
  }
)
