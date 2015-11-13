context("apa_print.htest()")

test_that(
  "t-Test for means"
  , {
    t_test <- t.test(extra ~ group, data = sleep)
    t_test_output <- apa_print(t_test)

    expect_that(t_test_output, is_a("list"))
    expect_that(length(t_test_output), equals(3))
    expect_that(names(t_test_output), equals(c("stat", "est", "full")))
    expect_that(t_test_output$stat, is_a("character"))
    expect_that(t_test_output$est, is_a("character"))
    expect_that(t_test_output$full, is_a("character"))

    expect_that(t_test_output$stat, equals("$t(17.78) = -1.86$, $p = .079$"))
    expect_that(t_test_output$est, equals("$\\Delta M = 1.58$, 95\\% CI $[-3.37$, $0.21]$"))
    expect_that(t_test_output$full, equals("$\\Delta M = 1.58$, 95\\% CI $[-3.37$, $0.21]$, $t(17.78) = -1.86$, $p = .079$"))

    t_test_output <- apa_print(t_test, in_paren = TRUE)
    expect_that(t_test_output$stat, equals("$t[17.78] = -1.86$, $p = .079$"))

    t_test_output <- apa_print(t_test, est_name = "foobar")
    expect_that(t_test_output$est, equals("$foobar = 1.58$, 95\\% CI $[-3.37$, $0.21]$"))

    t_test <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
    t_test_output <- apa_print(t_test)
    expect_that(t_test_output$stat, equals("$t(18) = -1.86$, $p = .079$"))
    expect_that(t_test_output$est, equals("$\\Delta M = 1.58$, 95\\% CI $[-3.36$, $0.20]$"))
    expect_that(t_test_output$full, equals("$\\Delta M = 1.58$, 95\\% CI $[-3.36$, $0.20]$, $t(18) = -1.86$, $p = .079$"))


    t_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
    t_test_output <- apa_print(t_test)
    expect_that(t_test_output$stat, equals("$t(9) = -4.06$, $p = .003$"))
    expect_that(t_test_output$est, equals("$M_d = -1.58$, 95\\% CI $[-2.46$, $-0.70]$"))
    expect_that(t_test_output$full, equals("$M_d = -1.58$, 95\\% CI $[-2.46$, $-0.70]$, $t(9) = -4.06$, $p = .003$"))

    t_test <- t.test(sleep$extra, mu = 0)
    t_test_output <- apa_print(t_test)
    expect_that(t_test_output$stat, equals("$t(19) = 3.41$, $p = .003$"))
    expect_that(t_test_output$est, equals("$M = 1.54$, 95\\% CI $[0.60$, $2.48]$"))
    expect_that(t_test_output$full, equals("$M = 1.54$, 95\\% CI $[0.60$, $2.48]$, $t(19) = 3.41$, $p = .003$"))

    t_test_output <- apa_print(t_test, ci = matrix(c(1, 2), ncol = 2, dimnames = list(NULL, c("2.5 \\%", "97.5 \\%"))))
    expect_that(t_test_output$est, equals("$M = 1.54$, 95\\% CI $[1.00$, $2.00]$"))

    t_test_output <- apa_print(t_test, stat_name = "foobar")
    expect_that(t_test_output$stat, equals("$foobar(19) = 3.41$, $p = .003$"))

    t_test_output <- apa_print(t_test, est_name = "foobar")
    expect_that(t_test_output$est, equals("$foobar = 1.54$, 95\\% CI $[0.60$, $2.48]$"))

    t_test_output <- apa_print(t_test, est_name = "foobar", digits = 3)
    expect_that(t_test_output$est, equals("$foobar = 1.540$, 95\\% CI $[0.596$, $2.484]$"))
  }
)

test_that(
  "Wilcoxon tests"
  , {
    wilcox_test <- suppressWarnings(wilcox.test(extra ~ group, data = sleep))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_that(wilcox_test_output, is_a("list"))
    expect_that(length(wilcox_test_output), equals(1))
    expect_that(names(wilcox_test_output), equals("stat"))
    expect_that(wilcox_test_output$stat, is_a("character"))

    expect_that(wilcox_test_output$stat, equals("$W = 25.50$, $p = .069$"))

    wilcox_test <- suppressWarnings(wilcox.test(extra ~ group, data = sleep, conf.int = TRUE))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_that(length(wilcox_test_output), equals(3))
    expect_that(names(wilcox_test_output), equals(c("stat", "est", "full")))
    expect_that(wilcox_test_output$stat, is_a("character"))
    expect_that(wilcox_test_output$est, is_a("character"))
    expect_that(wilcox_test_output$full, is_a("character"))

    expect_that(wilcox_test_output$stat, equals("$W = 25.50$, $p = .069$"))
    expect_that(wilcox_test_output$est, equals("$Mdn_d = -1.35$, 95\\% CI $[-3.60$, $0.10]$"))
    expect_that(wilcox_test_output$full, equals("$Mdn_d = -1.35$, 95\\% CI $[-3.60$, $0.10]$, $W = 25.50$, $p = .069$"))

    wilcox_test <- suppressWarnings(wilcox.test(extra ~ group, data = sleep, paired = TRUE))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_that(wilcox_test_output$stat, equals("$V = 0.00$, $p = .009$"))

    wilcox_test <- suppressWarnings(wilcox.test(extra ~ group, data = sleep, paired = TRUE, conf.int = TRUE))
    wilcox_test_output <- apa_print(wilcox_test)
    expect_that(wilcox_test_output$stat, equals("$V = 0.00$, $p = .009$"))
    expect_that(wilcox_test_output$est, equals("$Mdn^* = -1.40$, 95\\% CI $[-2.95$, $-1.05]$"))
    expect_that(wilcox_test_output$full, equals("$Mdn^* = -1.40$, 95\\% CI $[-2.95$, $-1.05]$, $V = 0.00$, $p = .009$"))

    wilcox_test <- suppressWarnings(wilcox.test(sleep$extra, mu = 0, conf.int = TRUE))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_that(wilcox_test_output$stat, equals("$V = 162.50$, $p = .007$"))
    expect_that(wilcox_test_output$est, equals("$Mdn^* = 1.60$, 95\\% CI $[0.45$, $2.65]$"))
    expect_that(wilcox_test_output$full, equals("$Mdn^* = 1.60$, 95\\% CI $[0.45$, $2.65]$, $V = 162.50$, $p = .007$"))
  }
)

test_that(
  "Tests for correlations"
  , {
    x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
    y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

    cor_test <- cor.test(x, y)
    cor_test_output <- apa_print(cor_test)

    expect_that(cor_test_output, is_a("list"))
    expect_that(length(cor_test_output), equals(3))
    expect_that(names(cor_test_output), equals(c("stat", "est", "full")))
    expect_that(cor_test_output$stat, is_a("character"))
    expect_that(cor_test_output$est, is_a("character"))
    expect_that(cor_test_output$full, is_a("character"))

    expect_that(cor_test_output$stat, equals("$t(7) = 1.84$, $p = .108$"))
    expect_that(cor_test_output$est, equals("$r = .57$, 95\\% CI $[-.15$, $.90]$"))
    expect_that(cor_test_output$full, equals("$r = .57$, 95\\% CI $[-.15$, $.90]$, $t(7) = 1.84$, $p = .108$"))

    cor_test_output <- apa_print(cor_test, in_paren = TRUE)
    expect_that(cor_test_output$stat, equals("$t[7] = 1.84$, $p = .108$"))
    expect_that(cor_test_output$full, equals("$r = .57$, 95\\% CI $[-.15$, $.90]$, $t[7] = 1.84$, $p = .108$"))

    cor_test <- cor.test(x, y, method = "spearman")
    cor_test_output <- apa_print(cor_test)

    expect_that(cor_test_output$stat, equals("$S = 48.00$, $p = .097$"))
    expect_that(cor_test_output$est, equals("$r_{\\mathrm{s}} = .60$"))
    expect_that(cor_test_output$full, equals("$r_{\\mathrm{s}} = .60$, $S = 48.00$, $p = .097$"))

    cor_test <- cor.test(x, y, method = "kendall")
    cor_test_output <- apa_print(cor_test)

    expect_that(cor_test_output$stat, equals("$T = 26.00$, $p = .119$"))
    expect_that(cor_test_output$est, equals("$\\uptau = .44$"))
    expect_that(cor_test_output$full, equals("$\\uptau = .44$, $T = 26.00$, $p = .119$"))

    cor_test <- cor.test(x, y, method = "kendall", exact = FALSE)
    cor_test_output <- apa_print(cor_test)

    expect_that(cor_test_output$stat, equals("$z = 1.67$, $p = .095$"))
    expect_that(cor_test_output$est, equals("$\\uptau = .44$"))
    expect_that(cor_test_output$full, equals("$\\uptau = .44$, $z = 1.67$, $p = .095$"))
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

    expect_that(prop_test_output, is_a("list"))
    expect_that(length(prop_test_output), equals(1))
    expect_that(names(prop_test_output), equals("stat"))
    expect_that(prop_test_output$stat, is_a("character"))

    expect_that(prop_test_output$stat, equals("$\\chi^2(3, n = 397) = 12.60$, $p = .006$"))

    prop_test_output <- suppressWarnings(apa_print(prop_test, n = sum(patients), in_paren = TRUE))
    expect_that(prop_test_output$stat, equals("$\\chi^2[3, n = 397] = 12.60$, $p = .006$"))
  }
)

test_that(
  "Bartlett test"
  , {
    bartlett_test <- bartlett.test(count ~ spray, data = InsectSprays)

    bartlett_test_output <- apa_print(bartlett_test)
    expect_that(bartlett_test_output, is_a("list"))
    expect_that(length(bartlett_test_output), equals(1))
    expect_that(names(bartlett_test_output), equals("stat"))
    expect_that(bartlett_test_output$stat, is_a("character"))
    expect_that(bartlett_test_output$stat, equals("$K^2(5) = 25.96$, $p < .001$"))

    bartlett_test_output <- apa_print(bartlett_test, in_paren = TRUE)
    expect_that(bartlett_test_output$stat, equals("$K^2[5] = 25.96$, $p < .001$"))
  }
)



