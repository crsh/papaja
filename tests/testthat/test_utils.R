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
