context("apa_print.htest()")

test_that(
  "t-Test for means"
  , {
    t_test <- t.test(extra ~ group, data = sleep)
    t_test_output <- apa_print(t_test)

    expect_apa_results(
      t_test_output
      , labels = list(
        estimate = "$\\Delta M$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )

    expect_identical(t_test_output$stat, "$t(17.78) = -1.86$, $p = .079$")
    expect_identical(t_test_output$est, "$\\Delta M = -1.58$, 95\\% CI $[-3.37, 0.21]$")
    expect_identical(t_test_output$full, "$\\Delta M = -1.58$, 95\\% CI $[-3.37, 0.21]$, $t(17.78) = -1.86$, $p = .079$")


    t_test <- t.test(extra ~ group, data = sleep, conf.level = .99)
    t_test_output <- apa_print(t_test)

    expect_apa_results(
      object = t_test_output
      , labels = list(
        estimate = "$\\Delta M$"
        , conf.int = "99\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )

    expect_identical(
      object = t_test_output$full_result
      , expected = "$\\Delta M = -1.58$, 99\\% CI $[-4.03, 0.87]$, $t(17.78) = -1.86$, $p = .079$"
    )

#     t_test <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
#     t_test_output <- apa_print(t_test)
#     expect_equal(t_test_output$stat, "$t(18) = -1.86$, $p = .079$")
#     expect_equal(t_test_output$est, "$\\Delta M = 1.58$, 95\\% CI $[-3.36$, $0.20]$")
#     expect_equal(t_test_output$full, "$\\Delta M = 1.58$, 95\\% CI $[-3.36$, $0.20]$, $t(18) = -1.86$, $p = .079$")


    t_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
    t_test_output <- apa_print(t_test)
    expect_apa_results(
      t_test_output
      , labels = list(
        estimate = "$M_\\Delta$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )
    expect_identical(t_test_output$full, "$M_\\Delta = -1.58$, 95\\% CI $[-2.46, -0.70]$, $t(9) = -4.06$, $p = .003$")

    t_test <- t.test(sleep$extra, mu = 0)
    t_test_output <- apa_print(t_test)
    expect_apa_results(
      t_test_output
      , labels = list(
        estimate = "$M$"
        , conf.int = "95\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )
    expect_identical(
      t_test_output$full
      , "$M = 1.54$, 95\\% CI $[0.60, 2.48]$, $t(19) = 3.41$, $p = .003$"
    )
    # Provide a custom ci, check values and labelling
    ci <- structure(c(1.0, 2.0), "conf.level" = .7)
    t_test_output <- apa_print(t_test, conf.int = ci)
    expect_apa_results(
      t_test_output
      , labels = list(
        estimate = "$M$"
        , conf.int = "70\\% CI"
        , statistic = "$t$"
        , df = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )
    expect_identical(t_test_output$est, "$M = 1.54$, 70\\% CI $[1.00, 2.00]$")

    t_test_output <- apa_print(t_test, stat_name = "foobar")
    expect_identical(t_test_output$stat, "$foobar(19) = 3.41$, $p = .003$")

    t_test_output <- apa_print(t_test, est_name = "foobar")
    expect_identical(t_test_output$est, "$foobar = 1.54$, 95\\% CI $[0.60, 2.48]$")

    t_test_output <- apa_print(t_test, digits = 3)
    expect_identical(t_test_output$est, "$M = 1.540$, 95\\% CI $[0.596, 2.484]$")
  }
)

test_that(
  "Wilcoxon tests"
  , {
    wilcox_test <- wilcox.test(extra ~ group, data = sleep, exact = FALSE)
    wilcox_test_output <- apa_print(wilcox_test)

    expect_apa_results(
      wilcox_test_output
      , labels = list(
        statistic = "$W$"
        , p.value = "$p$"
      )
    )

    expect_identical(wilcox_test_output$stat, "$W = 25.50$, $p = .069$")

    wilcox_test <- wilcox.test(extra ~ group, data = sleep, conf.int = TRUE, exact = FALSE)
    wilcox_test_output <- apa_print(wilcox_test)

    expect_apa_results(
      wilcox_test_output
      , labels = list(
        estimate    = "$\\Delta \\mathit{Mdn}$"
        , conf.int  = "95\\% CI"
        , statistic = "$W$"
        , p.value   = "$p$"
      )
    )
    expect_identical(wilcox_test_output$est,  "$\\Delta \\mathit{Mdn} = -1.35$, 95\\% CI $[-3.60, 0.10]$")
    expect_identical(wilcox_test_output$full, "$\\Delta \\mathit{Mdn} = -1.35$, 95\\% CI $[-3.60, 0.10]$, $W = 25.50$, $p = .069$")


    wilcox_test <- wilcox.test(extra ~ group, data = sleep, paired = TRUE, exact = FALSE)
    wilcox_test_output <- apa_print(wilcox_test)

    expect_apa_results(
      wilcox_test_output
      , labels = list(
        statistic = "$V$"
        , p.value = "$p$"
      )
    )
    expect_identical(wilcox_test_output$stat, "$V = 0.00$, $p = .009$")
    expect_identical(wilcox_test_output$full, wilcox_test_output$stat)



    wilcox_test <- suppressWarnings(wilcox.test(sleep$extra, mu = 0, conf.int = TRUE, conf.level = .96))
    wilcox_test_output <- apa_print(wilcox_test)

    expect_apa_results(
      wilcox_test_output
      , labels = list(
        estimate    = "$\\mathit{Mdn}^*$"
        , conf.int  = "96\\% CI"
        , statistic = "$V$"
        , p.value   = "$p$"
      )
    )
    expect_identical(wilcox_test_output$full, "$\\mathit{Mdn}^* = 1.60$, 96\\% CI $[0.40, 2.70]$, $V = 162.50$, $p = .007$")
  }
)

test_that(
  "Tests for correlations"
  , {
    x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
    y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

    # Pearson's correlation ----
    cor_test <- cor.test(x, y)
    cor_test_output <- apa_print(cor_test)

    expect_apa_results(
      cor_test_output
      ,  labels = list(
        estimate    = "$r$"
        , conf.int  = "95\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value  = "$p$"
      )
    )

    expect_identical(cor_test_output$stat, "$t(7) = 1.84$, $p = .108$")
    expect_identical(cor_test_output$est, "$r = .57$, 95\\% CI $[-.15, .90]$")
    expect_identical(cor_test_output$full, "$r = .57$, 95\\% CI $[-.15, .90]$, $t(7) = 1.84$, $p = .108$")

    # Spearman's rank correlation ----
    cor_test <- cor.test(x, y, method = "spearman")
    cor_test_output <- apa_print(cor_test)

    expect_apa_results(
      cor_test_output
      , labels = list(
        estimate    = "$r_{\\mathrm{s}}$"
        , statistic = "$S$"
        , p.value   = "$p$"
      )
    )
    expect_identical(cor_test_output$full, "$r_{\\mathrm{s}} = .60$, $S = 48.00$, $p = .097$")

    # Kendall's tau, exact statistic ----
    cor_test <- cor.test(x, y, method = "kendall")
    cor_test_output <- apa_print(cor_test)

    expect_apa_results(
      cor_test_output
      , labels = list(
        estimate    = "$\\uptau$"
        , statistic = "$T$"
        , p.value   = "$p$"
      )
    )
    expect_identical(cor_test_output$full, "$\\uptau = .44$, $T = 26.00$, $p = .119$")

    # Kendall's tau with normal approcimation ----
    cor_test <- cor.test(x, y, method = "kendall", exact = FALSE)
    cor_test_output <- apa_print(cor_test)

    expect_apa_results(
      cor_test_output
      , labels = list(
        estimate    = "$\\uptau$"
        , statistic = "$z$"
        , p.value   = "$p$"
      )
    )
    expect_identical(cor_test_output$full, "$\\uptau = .44$, $z = 1.67$, $p = .095$")
  }
)

test_that(
  "Chi-squared for contingency tables"
  , {
    prop_test <- prop.test(5, 10)
    prop_test_output <- suppressWarnings(apa_print(prop_test, n = 10))

    expect_apa_results(
      prop_test_output
      ,  labels = list(
        estimate = "$\\hat\\pi$"
        , conf.int = "95\\% CI"
        , statistic = "$\\chi^2$"
        , df      = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )

    smokers  <- c(83, 90, 129, 70)
    patients <- c(86, 93, 136, 82)
    prop_test <- prop.test(smokers, patients)

    expect_error(apa_print(prop_test), "Please provide the sample size to report.")
    prop_test_output <- suppressWarnings(apa_print(prop_test, n = sum(patients)))

    expect_apa_results(
      prop_test_output
      ,  labels = list(
        statistic = "$\\chi^2$"
        , df      = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )
    expect_identical(prop_test_output$stat, "$\\chi^2(3, n = 397) = 12.60$, $p = .006$")

    two_sample_prop_test <- suppressWarnings(prop.test(smokers[3:4], n = patients[3:4]))
    two_sample_prop_test_output <- apa_print(two_sample_prop_test, n = sum(patients[3:4]))

    expect_apa_results(
      two_sample_prop_test_output
      , labels = list(
        estimate    = "\\Delta \\hat\\pi"
        , conf.int  = "95\\% CI"
        , statistic = "$\\chi^2$"
        , df        ="$\\mathit{df}$"
        , p.value   = "$p$"
      )
    )

    expect_identical(
      attr(two_sample_prop_test_output$table$statistic, "n")
      , printnum(as.integer(sum(patients[3:4])))
    )
  }
)

test_that(
  "Bartlett test"
  , {
    bartlett_test <- bartlett.test(count ~ spray, data = InsectSprays)
    bartlett_test_output <- apa_print(bartlett_test)

    expect_apa_results(
      bartlett_test_output
      , labels = list(
        statistic = "$K^2$"
        , df      = "$\\mathit{df}$"
        , p.value = "$p$"
      )
    )

    expect_identical(bartlett_test_output$stat, "$K^2(5) = 25.96$, $p < .001$")
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

    expect_apa_results(
      mauchly_output
      , labels = list(
        statistic = "$W$"
        , p.value = "$p$"
      )
    )
    expect_identical(mauchly_output$stat, "$W = 0.89$, $p = .638$")

    mauchly_test <- mauchly.test(mlmfit, M = ~ deg + noise, X = ~ noise, idata = mauchly_data)
    mauchly_output <- apa_print(mauchly_test)

    expect_apa_results(
      mauchly_output
      , labels = list(
        statistic = "$W$"
        , p.value = "$p$"
      )
    )
    expect_identical(mauchly_output$stat, "$W = 0.96$, $p = .850$")
  }
)

test_that(
  "One-way ANOVA"
  , {
    oneway_test <- oneway.test(extra ~ group, data = sleep)
    oneway_output <- apa_print(oneway_test)

    expect_apa_results(
      oneway_output
      , labels = list(
        statistic     = "$F$"
        , df          = "$\\mathit{df}$"
        , df.residual = "$\\mathit{df}_{\\mathrm{res}}$"
        , p.value     = "$p$"
      )
    )
    expect_identical(oneway_output$stat, "$F(1, 17.78) = 3.46$, $p = .079$")
    expect_identical(oneway_output$full, oneway_output$stat)
  }
)

# Test for issue #192, confidence interval was confused with infty and extraneous $ symbols
test_that(
  "One-sided t test (with infty in CI)"
  , {
    t_out <- t.test(formula = yield ~ N, data = npk, alternative = "greater")
    apa_out <- apa_print(t_out)

    t2 <- t.test(formula = yield ~ N, data = npk, alternative = "less")
    apa2 <- apa_print(t2)

    expect_apa_results(apa_out, col.names = c("estimate" , "conf.int", "statistic", "df", "p.value"))
    expect_apa_results(apa2, col.names = c("estimate", "conf.int", "statistic", "df", "p.value"))

    # positive infinity ----
    expect_identical(
      object = apa_out$full_result
      , expected = "$\\Delta M = -5.62$, 95\\% CI $[-9.54, \\infty]$, $t(21.88) = -2.46$, $p = .989$"
    )
    expect_identical(
      object = apa_out$estimate
      , expected = "$\\Delta M = -5.62$, 95\\% CI $[-9.54, \\infty]$"
    )
    expect_identical(
      apa_out$table$conf.int
      , expected = structure("[-9.54, $\\infty$]", label = "95\\% CI", class = c("tiny_labelled", "character"))
    )

    # negative infinity ----
    expect_identical(
      object = apa2$full_result
      , expected = "$\\Delta M = -5.62$, 95\\% CI $[-\\infty, -1.70]$, $t(21.88) = -2.46$, $p = .011$"
    )
    expect_identical(
      object = apa2$estimate
      , expected = "$\\Delta M = -5.62$, 95\\% CI $[-\\infty, -1.70]$"
    )
    expect_identical(
      apa2$table$conf.int
      , expected = structure(
        "[-$\\infty$, -1.70]"
        , label = "95\\% CI"
        , class = c("tiny_labelled", "character")
      )
    )
  }
)

test_that(
  "Degenerate htest objects"
  , {
    degenerate <- t.test(yield ~ N, npk)
    degenerate$statistic <- degenerate$estimate <- NULL

    expect_error(apa_print(degenerate, est_name = "M"), "No estimate available in results table.")
    expect_error(apa_print(degenerate, stat_name = "t"), "No statistic available in results table.")
  }
)

test_that(
  "Deprecated 'ci' argument"
  , {
    expect_warning(
      apa_print(t.test(yield ~ N, npk), ci = c(1, 2))
      , regexp = "Using argument 'ci' in calls to 'apa_print()' is deprecated. Please use 'conf.int' instead."
      , fixed = TRUE
    )
  }
)
