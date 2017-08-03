context("apa_barplot()")

test_that(
  "apa_barplot.default()"
  , {
    generic <- apa_generic_plot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), plot = c("bars", "error_bars"), fun_aggregate = median, tendency = sum, dispersion = se)
    x <- apa_barplot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), fun_aggregate = median, tendency = sum, dispersion = se)

    expect_equal(
      object = x
      , expected = generic
    )
  }
)

context("apa_beeplot()")

test_that(
  "apa_beeplot.default()"
  , {
    generic <- apa_generic_plot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), plot = c("swarms", "points", "error_bars"), fun_aggregate = median, tendency = sum, dispersion = se)
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
    generic <- apa_generic_plot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), plot = c("lines", "points", "error_bars"), fun_aggregate = median, tendency = sum, dispersion = se)
    x <- apa_lineplot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), fun_aggregate = median, tendency = sum, dispersion = se)

    expect_equal(
      object = x
      , expected = generic
    )
  }
)


context("apa_generic_plot()")

test_that(
  "apa_generic_plot.default(): calculations, parameter `level`"
  , {
    out <- apa_generic_plot(data = npk, id = "block", dv = "yield", level = .75)
    npk$block <- as.factor(npk$block)
    variable_label(npk[c("block", "yield")]) <- c("block", "yield")
    aggregated <- aggregate(formula = yield ~ block, data = npk, FUN = mean)

    tendency <- mean(aggregated$yield)
    dispersion <- conf_int(aggregated$yield, level = .75)

    expect_equal(
      object = out$y[, c("tendency", "dispersion")]
      , expected = data.frame(tendency, dispersion)
    )
    expect_equal(
      object = out$data[, c("block", "yield")]
      , expected = aggregated
    )
  }
)

