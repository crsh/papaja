context("apa_barplot()")

test_that(
  "apa_barplot.default()"
  , {
    generic <- apa_factorial_plot(data = npk, id = "block", dv = "yield", factors = c("N", "P", "K"), plot = c("bars", "error_bars"), fun_aggregate = median, tendency = sum, dispersion = se)
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
    variable_label(data[, c("block", "yield")]) <- c("block" = "block", "yield" = "yield")
    aggregated <- aggregate(formula = yield ~ block, data = data, FUN = mean)

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

test_that(
  "apa_factorial_plot.default(): Inherit customisations"
  , {
    out <- apa_factorial_plot(
      data = npk
      , id = "block"
      , dv = "yield"
      , factors = c("N", "P")
      , main = expression("test"~italic(T))
      , plot = c("points", "swarms", "lines", "error_bars")
      , args_points = list(pch = c(22, 23), bg = c("#FF0000", "#00FF00"), cex = c(0.99, .98), col = c("#0F0F0F", "#F0F0F0"))
      , args_swarm = list(cex = 2)
      , args_lines = list(col = c("#FF3766", "blue"), lwd = c(1, 3), lty = c(13, 14, 15))
      , args_error_bars = list(col = "#FF6637")
    )

    expect_identical(
      object = out$args$args_swarm$cex
      , expected = c(2)
      )
    expect_identical(
      object = out$args$args_swarm$pch
      , expected = c(22, 23)
    )
    expect_identical(
      object = out$args$args_lines$col
      , expected = c("#FF3766", "blue")
    )
    expect_identical(
      object = out$args$args_lines$lwd
      , expected = c(1, 3)
    )
    expect_identical(
      object = out$args$args_error_bars$col
      , expected = "#FF6637"
    )
    expect_identical(
      object = out$args$args_title$main
      , expected = expression("test"~italic(T))
    )
    expect_identical(
      object = out$args$args_legend
      , expected = list(
        title = "P"
        , x = "topright"
        , legend = c("0", "1")
        , pch = c(22, 23)
        , lty = c(13, 14, 15)
        , bty = "n"
        , pt.bg = c("#FF0000", "#00FF00")
        , col = c("#0F0F0F", "#F0F0F0")
        , pt.cex = c(.99, .98)
      )
    )
  }
)

test_that(
  "apa_factorial_plot.default(): Four-factors case"
  , {
    load("data/mixed_data.rdata")

    object_1 <- apa_factorial_plot(
      data = mixed_data
      , id = "Subject"
      , dv = "Recall"
      , factors = c("Gender", "Dosage", "Task", "Valence")
      , plot = c("lines", "points", "error_bars")
      , dispersion = wsci
      , level = .4
      , intercept = 0
      , fun_aggregate = mean
    )
    # Test if mandatory legend is plotted:
    expect_identical(
      object = object_1$args$plotCPos$args_legend$plot
      , expected = TRUE
    )

    # Test more interesting stuff:
    # ...
    # ...
    # ...
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
      object = attributes(object_1$data)$removed_cases_implicit_NA
      , expected = "1"
    )

    reference_object <- default_label(droplevels(npk[5:24, c("block", "N", "P", "yield")]))
    attr(reference_object, "removed_cases_implicit_NA") <- "1"
    expect_equal(
      object = object_1$data
      , expected = reference_object
    )
  }
)

file.remove("Rplots.pdf")
