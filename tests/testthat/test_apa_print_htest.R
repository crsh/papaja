context("apa_print.htest()")

test_that(
  "t-Test for means"
  , {
    t_test <- t.test(extra ~ group, data = sleep)
    t_test_output <- apa_print(t_test)

    expect_is(t_test_output, "list")
    expect_equal(length(t_test_output), 3)
    expect_equal(names(t_test_output), c("stat", "est", "full"))

    expect_is(t_test_output$stat, "character")
    expect_equal(t_test_output$stat, "$t(17.78) = -1.86$, $p = .079$")

    expect_is(t_test_output$est, "character")
    expect_equal(t_test_output$est, "$\\Delta M = 1.58$, 95\\% CI $[-3.37$, $0.21]$")

    expect_is(t_test_output$full, "character")
    expect_equal(t_test_output$full, "$\\Delta M = 1.58$, 95\\% CI $[-3.37$, $0.21]$, $t(17.78) = -1.86$, $p = .079$")

#     t_test <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
#     t_test_output <- apa_print(t_test)
#     expect_equal(t_test_output$stat, "$t(18) = -1.86$, $p = .079$")
#     expect_equal(t_test_output$est, "$\\Delta M = 1.58$, 95\\% CI $[-3.36$, $0.20]$")
#     expect_equal(t_test_output$full, "$\\Delta M = 1.58$, 95\\% CI $[-3.36$, $0.20]$, $t(18) = -1.86$, $p = .079$")


    t_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
    t_test_output <- apa_print(t_test)
    expect_equal(t_test_output$full, "$M_d = -1.58$, 95\\% CI $[-2.46$, $-0.70]$, $t(9) = -4.06$, $p = .003$")

    t_test <- t.test(sleep$extra, mu = 0)
    t_test_output <- apa_print(t_test)
    expect_equal(t_test_output$full, "$M = 1.54$, 95\\% CI $[0.60$, $2.48]$, $t(19) = 3.41$, $p = .003$")

    t_test_output <- apa_print(t_test, ci = matrix(c(1, 2), ncol = 2, dimnames = list(NULL, c("2.5 \\%", "97.5 \\%"))))
    expect_equal(t_test_output$est, "$M = 1.54$, 95\\% CI $[1.00$, $2.00]$")

    t_test_output <- apa_print(t_test, stat_name = "foobar")
    expect_equal(t_test_output$stat, "$foobar(19) = 3.41$, $p = .003$")

    t_test_output <- apa_print(t_test, est_name = "foobar")
    expect_equal(t_test_output$est, "$foobar = 1.54$, 95\\% CI $[0.60$, $2.48]$")

    t_test_output <- apa_print(t_test, digits = 3)
    expect_equal(t_test_output$est, "$M = 1.540$, 95\\% CI $[0.596$, $2.484]$")
  }
)

test_that(
  "Wilcoxon tests"
  , {
    wilcox_test <- suppressWarnings(wilcox.test(extra ~ group, data = sleep))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_is(wilcox_test_output, "list")
    expect_equal(length(wilcox_test_output), 1)
    expect_equal(names(wilcox_test_output), "stat")
    expect_is(wilcox_test_output$stat, "character")

    expect_equal(wilcox_test_output$stat, "$W = 25.50$, $p = .069$")

    wilcox_test <- suppressWarnings(wilcox.test(extra ~ group, data = sleep, conf.int = TRUE))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_equal(length(wilcox_test_output), 3)
    expect_equal(names(wilcox_test_output), c("stat", "est", "full"))

    expect_is(wilcox_test_output$stat, "character")
    expect_equal(wilcox_test_output$stat, "$W = 25.50$, $p = .069$")

    expect_is(wilcox_test_output$est, "character")
    expect_equal(wilcox_test_output$est, "$Mdn_d = -1.35$, 95\\% CI $[-3.60$, $0.10]$")

    expect_is(wilcox_test_output$full, "character")
    expect_equal(wilcox_test_output$full, "$Mdn_d = -1.35$, 95\\% CI $[-3.60$, $0.10]$, $W = 25.50$, $p = .069$")

    wilcox_test <- suppressWarnings(wilcox.test(extra ~ group, data = sleep, paired = TRUE))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_equal(wilcox_test_output$stat, "$V = 0.00$, $p = .009$")


    wilcox_test <- suppressWarnings(wilcox.test(sleep$extra, mu = 0, conf.int = TRUE))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_equal(wilcox_test_output$full, "$Mdn^* = 1.60$, 95\\% CI $[0.45$, $2.65]$, $V = 162.50$, $p = .007$")
  }
)

test_that(
  "Tests for correlations"
  , {
    x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
    y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

    cor_test <- cor.test(x, y)
    cor_test_output <- apa_print(cor_test)

    expect_is(cor_test_output, "list")
    expect_equal(length(cor_test_output), 3)
    expect_equal(names(cor_test_output), c("stat", "est", "full"))

    expect_is(cor_test_output$stat, "character")
    expect_equal(cor_test_output$stat, "$t(7) = 1.84$, $p = .108$")

    expect_is(cor_test_output$est, "character")
    expect_equal(cor_test_output$est, "$r = .57$, 95\\% CI $[-.15$, $.90]$")

    expect_is(cor_test_output$full, "character")
    expect_equal(cor_test_output$full, "$r = .57$, 95\\% CI $[-.15$, $.90]$, $t(7) = 1.84$, $p = .108$")

    cor_test <- cor.test(x, y, method = "spearman")
    cor_test_output <- apa_print(cor_test)

    expect_equal(cor_test_output$full, "$r_{\\mathrm{s}} = .60$, $S = 48.00$, $p = .097$")

    cor_test <- cor.test(x, y, method = "kendall")
    cor_test_output <- apa_print(cor_test)

    expect_equal(cor_test_output$full, "$\\uptau = .44$, $T = 26.00$, $p = .119$")

    cor_test <- cor.test(x, y, method = "kendall", exact = FALSE)
    cor_test_output <- apa_print(cor_test)

    expect_equal(cor_test_output$full, "$\\uptau = .44$, $z = 1.67$, $p = .095$")
  }
)

test_that(
  "Chi-squared for contingency tables"
  , {
    smokers  <- c(83, 90, 129, 70)
    patients <- c(86, 93, 136, 82)
    prop_test <- prop.test(smokers, patients)

    expect_error(apa_print(prop_test), "Please provide the sample size to report.")
    prop_test_output <- suppressWarnings(apa_print(prop_test, n = sum(patients)))

    expect_is(prop_test_output, "list")
    expect_equal(length(prop_test_output), 1)
    expect_equal(names(prop_test_output), "stat")
    expect_is(prop_test_output$stat, "character")

    expect_equal(prop_test_output$stat, "$\\chi^2(3, n = 397) = 12.60$, $p = .006$")
  }
)

test_that(
  "Bartlett test"
  , {
    bartlett_test <- bartlett.test(count ~ spray, data = InsectSprays)
    bartlett_test_output <- apa_print(bartlett_test)

    expect_is(bartlett_test_output, "list")
    expect_equal(length(bartlett_test_output), 1)
    expect_equal(names(bartlett_test_output), "stat")
    expect_is(bartlett_test_output$stat, "character")
    expect_equal(bartlett_test_output$stat, "$K^2(5) = 25.96$, $p < .001$")
  }
)

test_that(
  "Mauchly test"
  , {
    tmp <- capture.output(utils::example(SSD))
    mauchly_data <- data.frame(
      deg = gl(3, 1, 6, labels = c(0, 4, 8))
      , noise = gl(2, 3, 6, labels = c("A", "P"))
    )

    mauchly_test <- mauchly.test(mlmfit, X = ~ deg + noise, idata = mauchly_data)
    mauchly_output <- apa_print(mauchly_test)

    expect_is(mauchly_output, "list")
    expect_equal(length(mauchly_output), 1)
    expect_equal(names(mauchly_output), "stat")
    expect_is(mauchly_output$stat, "character")
    expect_equal(mauchly_output$stat, "$W = 0.89$, $p = .638$")

    mauchly_test <- mauchly.test(mlmfit, M = ~ deg + noise, X = ~ noise, idata = mauchly_data)
    mauchly_output <- apa_print(mauchly_test)

    expect_is(mauchly_output, "list")
    expect_equal(length(mauchly_output), 1)
    expect_equal(names(mauchly_output), "stat")
    expect_is(mauchly_output$stat, "character")
    expect_equal(mauchly_output$stat, "$W = 0.96$, $p = .850$")
  }
)

