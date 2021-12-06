context("apa_barplot()")

test_that(
  "apa_barplot.default()"
  , {
    generic <- apa_factorial_plot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), plot = c("bars", "error_bars"), fun_aggregate = median, tendency = sum, dispersion = se, jit = .4)
    x <- apa_barplot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), fun_aggregate = median, tendency = sum, dispersion = se)

    expect_equal(
      object = x
      , expected = generic
    )
  }
)

test_that(
  "apa_barplot.afex_aov()"
  , {
    afex_aov <- afex::aov_ez(data = npk, id = "block", within = c("N", "P"), dv = "yield")
    generic_bar <- apa_factorial_plot(afex_aov, plot = c("bars", "error_bars"))
    generic_line <- apa_factorial_plot(afex_aov, plot = c("lines", "error_bars", "points"))
    generic_bee <- apa_factorial_plot(afex_aov, plot = c("swarms", "points", "error_bars"))
    bar <- apa_barplot(afex_aov)
    line <- apa_lineplot(afex_aov)
    bee <- apa_beeplot(afex_aov)

    expect_identical(
      object = bar
      , expected = generic_bar
    )
    expect_identical(
      object = line
      , expected = generic_line
    )
    expect_identical(
      object = bee
      , expected = generic_bee
    )
  }
)

context("apa_beeplot()")

test_that(
  "apa_beeplot.default()"
  , {
    generic <- apa_factorial_plot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), plot = c("swarms", "points", "error_bars"), fun_aggregate = median, tendency = sum, dispersion = se)
    x <- apa_beeplot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), fun_aggregate = median, tendency = sum, dispersion = se)

    expect_equal(
      object = x
      , expected = generic
    )
  }
)

context("apa_lineplot()")

test_that(
  "apa_lineplot.default()"
  , {
    generic <- apa_factorial_plot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), plot = c("lines", "points", "error_bars"), fun_aggregate = median, tendency = sum, dispersion = se)
    x <- apa_lineplot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), fun_aggregate = median, tendency = sum, dispersion = se)

    expect_equal(
      object = x
      , expected = generic
    )
  }
)


context("apa_factorial_plot()")

test_that(
  "apa_factorial_plot.default(): calculations, and parameter `level`"
  , {
    data <- npk
    out <- apa_factorial_plot(data = data, id = "block", dv = "yield", level = .75)
    data$block <- as.factor(data$block)
    aggregated <- aggregate(formula = yield ~ block, data = data, FUN = mean)
    variable_label(aggregated) <- c(block = "block", "yield" = "yield")

    tendency <- mean(aggregated$yield)
    dispersion <- conf_int(aggregated$yield, level = .75)

    expect_equivalent(
      object = tinylabels::unlabel(out$plots[[1]]$y[[1]][[1]])
      , expected = tendency + c(-dispersion, 0, dispersion)
    )
    expect_equivalent(
      object = out$plots[[1]]$y_agg
      , expected = aggregated
    )
  }
)




test_that(
  'apa_factorial_plot.default(): use = "complete.obs"'
  , {
    object_1 <- apa_factorial_plot(
      data = npk[2:24, ]
      , id = "block"
      , dv = "yield"
      , factors = c("N", "P")
      , dispersion = wsci
      , use = "complete.obs"
      , plot = c("lines", "points", "error_bars", "swarms", "bars")
    )

    expect_equal(
      object = attr(object_1$plots[[1L]]$y_agg, "removed_cases_implicit_NA")
      , expected = "1"
    )

    reference_object <- papaja:::default_label.data.frame(droplevels(npk[5:24, c("block", "N", "P", "yield")]))
    attr(reference_object, "removed_cases_implicit_NA") <- "1"
    expect_equal(
      object = object_1$plots[[1]]$y_agg
      , expected = reference_object
    )
  }
)

suppressWarnings(file.remove("Rplots.pdf"))
