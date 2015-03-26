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
    expect_that(chis, equals(rep("\\chi^2", length(chis))))

    expect_that(convert_stat_name("t"), equals("t"))
    expect_that(convert_stat_name("z"), equals("z"))
  }
)


test_that(
  "print_confint()"
  , {
    x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
    y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

    cor_test <- cor.test(x, y)
    apa_confint <- print_confint(cor_test$conf.int, conf_level = 0.95)
    expect_that(apa_confint, is_a("character"))
    expect_that(apa_confint, equals("95\\% CI $[-0.15$, $0.90]$"))

    apa_confint <- print_confint(cor_test$conf.int, gt1 = FALSE)
    expect_that(apa_confint, equals("95\\% CI $[-.15$, $.90]$"))

    apa_confint <- print_confint(cor_test$conf.int)
    expect_that(apa_confint, equals("95\\% CI $[-0.15$, $0.90]$"))

    apa_confint <- print_confint(c(1, 2))
    expect_that(apa_confint, equals("$[1.00$, $2.00]$"))

    conf_int <- confint(lm(x ~ y))
    apa_confint <- print_confint(conf_int)

    expect_that(apa_confint, is_a("list"))
    expect_that(length(apa_confint), equals(nrow(conf_int)))
    expect_that(names(apa_confint), equals(c("Intercept", "y")))
    expect_that(apa_confint$Intercept, equals("95\\% CI $[19.52$, $51.78]$"))
    expect_that(apa_confint$y, equals("95\\% CI $[-0.95$, $7.67]$"))
  }
)
