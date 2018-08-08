context("Calculations")

# data <- data.frame(
#   "id" = rep(1:2, each = 8)
#   , "F1" = rep(c(1, -1), each = 4)
#   , "F2" = rep(c(1, -1), each = 2)
#   , "F3" = c(1, -1)
# )
#
# data$dv <- data$F1 *rnorm(1) + data$F2 * rnorm(1) + data$F3 * rnorm(1) + rnorm(16) + rep(rnorm(2), each = 8)
#
# for (i in c("F1", "F2", "F3"))
#   data[[i]] <- as.factor(data[[i]])
#
# library(Rmisc)
# Rmisc <- summarySEwithin(data = data, measurevar = "dv",
#                 betweenvars = NULL, withinvars = c("F1", "F2", "F3"), idvar = "id",
#                 na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
# Rmisc <- Rmisc[c("F1", "F2", "F3", "ci")]
# colnames(Rmisc)[4] <- "dv"
# dput(as.matrix(Rmisc))

test_that(
  "Within-subjects confidence intervals: Three factors within-subjects design"
  , {
    data <- structure(
      list(
        id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)
        , F1 = structure(c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L), .Label = c("-1", "1"), class = "factor")
        , F2 = structure(c(2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L), .Label = c("-1", "1"), class = "factor")
        , F3 = structure(c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c("-1", "1"), class = "factor")
        , dv = c(
          2.51108865147373, 2.79300316288011, 1.24498751760904, -3.2097171749705, -0.86829135419962, -3.17641293936803, -5.09655274729662, -5.67922281667759
          , 4.99456366116516, 1.28920687940668, 2.80013312250661, -2.78384472239226, 2.36059467792542, -0.754370127863485, -0.863098014576623, -5.68140019794875
        )
      )
      , .Names = c("id", "F1", "F2", "F3", "dv")
      , row.names = c(NA, -16L)
      , class = "data.frame"
    )

    Rmisc <- structure(c("-1", "-1", "-1", "-1", "1", "1", "1", "1", "-1",
                         "-1", "1", "1", "-1", "-1", "1", "1", "-1", "1", "-1", "1", "-1",
                         "1", "-1", "1", "10.9180149", "17.8493485", " 5.5466877", "11.0265669",
                         " 8.0108066", " 0.3410632", "21.1166383", " 5.9639200"), .Dim = c(8L, 4L), .Dimnames = list(NULL, c("F1", "F2", "F3", "dv")))


    test <- wsci(data = data, id = "id", dv = "dv", factors = c("F1", "F2", "F3"), method = "Morey")

    expect_equal(as.matrix(test), Rmisc)
  }
)


# library(Rmisc)
# Rmisc <- summarySEwithin(data = data, measurevar = "dv",
#                 betweenvars = c("F1"), withinvars = c("F2", "F3"), idvar = "id",
#                 na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
# Rmisc <- Rmisc[c("F1", "F2", "F3", "ci")]
# dput(Rmisc)


test_that(
  "Within-subjects confidence intervals: Three factors mixed design"
  , {
    data <- structure(
      list(
        id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L)
        , F1 = structure(c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L), .Label = c("-1", "1"), class = "factor")
        , F2 = structure(c(2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L), .Label = c("-1", "1"), class = "factor")
        , F3 = structure(c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c("-1", "1"), class = "factor")
        , dv = c(
          2.51108865147373, 2.79300316288011, 1.24498751760904, -3.2097171749705, -0.86829135419962, -3.17641293936803, -5.09655274729662, -5.67922281667759
          , 4.99456366116516, 1.28920687940668, 2.80013312250661, -2.78384472239226, 2.36059467792542, -0.754370127863485, -0.863098014576623, -5.68140019794875
        )
      )
      , .Names = c("id", "F1", "F2", "F3", "dv")
      , row.names = c(NA, -16L)
      , class = "data.frame"
    )

    Rmisc <- structure(
      list(
        F1 = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("-1", "1"), class = "factor")
        , F2 = structure(c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), .Label = c("-1", "1"), class = "factor")
        , F3 = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), .Label = c("-1", "1"), class = "factor")
        , ci = c(18.13976813201, 12.9325356320052, 0.355856736124444, 5.56308923612926, 2.30569581586717, 5.97857379893408, 16.4616119853719, 12.788734002305)
        )
      , .Names = c("F1", "F2", "F3", "ci")
      , row.names = c(NA, -8L)
      , class = "data.frame"
    )

    test <- wsci(data = data, id = "id", dv = "dv", factors = c("F1", "F2", "F3"), method = "Morey")
    compare_data <- merge(test, Rmisc, by = c("F1", "F2", "F3"))

    expect_equal(compare_data$dv, compare_data$ci)
  }
)

test_that(
  "add_effect_sizes: Two-way between-subjects design"
  , {
    apa_variance_table <- structure(
      list(
        term = c("(Intercept)", "Gender", "Dosage", "Gender:Dosage")
        , sumsq = c(3164.0625, 76.5625, 5.0625, 0.0625)
        , df = c(1, 1, 1, 1)
        , sumsq_err = c(311.25, 311.25, 311.25, 311.25)
        , df_res = c(12, 12, 12, 12)
        , statistic = c(121.987951807229, 2.95180722891566, 0.195180722891566, 0.00240963855421686)
        , p.value = c(1.21163629179605e-07, 0.111450651130857, 0.66649556577607, 0.961656681938102)
      )
      , .Names = c("term", "sumsq", "df", "sumsq_err", "df_res", "statistic", "p.value")
      , row.names = c(NA, -4L)
      , class = c("apa_variance_table", "data.frame")
      , correction = "none"
    )

    with_intercept <- add_effect_sizes(apa_variance_table, es = c("pes", "ges", "es"), intercept = TRUE)
    without_intercept <- add_effect_sizes(apa_variance_table, es = c("pes", "ges", "es"), intercept = FALSE)[2:4, ]

    # SS from car output:
    SS_car <- c(3164.0625, 76.5625, 5.0625, 0.0625, 311.25)

    expect_equal(
      object = with_intercept$es
      # Expectation calculated via sums of squares from car
      , expected = SS_car[1:4] / sum(SS_car)
    )
    expect_equal(
      # Expectation calculated with afex
      object = with_intercept$pes
      , expected = c(0.910439708659293, 0.197421434327156, 0.016004742145821, 0.000200762899016262)
    )
    expect_equal(
      object = with_intercept$ges
      # Expectation calculated with afex
      , expected = c(0.910439708659293, 0.197421434327156, 0.016004742145821, 0.000200762899016262)
    )

    expect_equal(
      object = without_intercept$es
      # Expectation calculated via sums of squares from car
      , expected = SS_car[2:4]/sum(SS_car[2:5])
    )
    expect_equal(
      object = without_intercept$ges
      , expected = with_intercept$ges[2:4]
    )
    expect_equal(
      object = without_intercept$pes
      , expected = with_intercept$pes[2:4]
    )
  }
)


test_that(
  "add_effect_sizes: Two-way within-subjects design"
  , {
    apa_variance_table <- structure(
      list(
        sumsq = c(4177.2, 30.0000000000001, 9.80000000000002, 1.40000000000001)
        , df = c(1, 1, 1.43869284635452, 1.62877229248125)
        , sumsq_err = c(349.133333333333, 16.3333333333333, 26.8666666666667, 19.2666666666667)
        , df_res = c(4, 4, 5.7547713854181, 6.51508916992501)
        , statistic = c(47.8579339316403, 7.34693877551021, 1.4590570719603, 0.290657439446368)
        , p.value = c(0.00229109843354983, 0.053508296850918, 0.29341624721021, 0.714981769489886)
        , term = c("(Intercept)", "Task", "Valence", "Task:Valence")
      )
      , .Names = c("sumsq", "df", "sumsq_err", "df_res", "statistic", "p.value", "term")
      , row.names = c(NA, -4L)
      , class = c("apa_variance_table", "data.frame")
      , correction = "GG"
    )
    with_intercept <- add_effect_sizes(apa_variance_table, es = c("pes", "ges", "es"), intercept = TRUE)
    without_intercept <- add_effect_sizes(apa_variance_table, es = c("pes", "ges", "es"), intercept = FALSE)[2:4, ]

    # SS from car output:
    SS_car <- c(4177.2, 30.0000000000001, 9.80000000000002, 1.40000000000001)
    SS_err <- c(349.133333333333, 16.3333333333333, 26.8666666666667, 19.2666666666667)

    # Output with intercept ----
    expect_equal(
      object = with_intercept$es
      # Expectation calculated via sums of squares from car
      , expected = SS_car/sum(c(SS_car, SS_err))
    )
    expect_equal(
      object = with_intercept$ges
      # Expectation calculated with afex
      , expected = c(0.910303347280335, 0.0679347826086958, 0.0232558139534884, 0.00338983050847459)
    )
    expect_equal(
      object = with_intercept$pes
      # Expectation calculated with afex
      , expected = c(0.922866190441122, 0.64748201438849, 0.267272727272728, 0.0677419354838713)
    )

    # Output without intercept ----
    expect_equal(
      object = without_intercept$es
      # Expectation calculated via sums of squares from car
      , expected = SS_car[2:4]/sum(c(SS_car[2:4], SS_err))
    )
    expect_equal(
      object = without_intercept$ges
      , expected = with_intercept$ges[2:4]
    )
    expect_equal(
      object = without_intercept$pes
      , expected = with_intercept$pes[2:4]
    )
  }
)


test_that(
  "Within-subjects confidence intervals: Cousineau vs. Morey method"
  , {
    aggregated_data <- stats::aggregate(formula = yield ~ N + block, data = npk, FUN = mean)
    object_1 <- wsci(data = aggregated_data, id = "block", dv = "yield", factors = c("N"), method = "Cousineau", level = .98)
    object_2 <- wsci(data = aggregated_data, id = "block", dv = "yield", factors = c("N"), method = "Morey", level = .98)

    expect_identical(
      object = attributes(object_1)
      , expected = list(
          class = "data.frame"
          , names = c("N", "yield")
          , row.names = c(1L, 2L)
          , `Between-subjects factors` = "none"
          , `Within-subjects factors` = "N"
          , `Dependent variable` = "yield"
          , `Subject identifier` = "block"
          , `Confidence level` = 0.98
          , Method = "Cousineau"
      )
    )
    expect_identical(
      object = object_1$yield
      , expected = object_2$yield /sqrt(2)
    )
  }
)

test_that(
  "Within-subjects confidence intervals: Handling of implicit and explicit NAs"
  , {
    # Implicit NAs
    data <- npk
    data$yield[2] <- NA
    aggregated <- stats::aggregate(formula = yield ~ N + P + block, data = data, FUN = mean)
    expect_warning(
      wsci(data = aggregated, id = "block", dv = "yield", factors = c("N", "P"))
      , "Because of incomplete data, the following cases were removed from calculation of within-subjects confidence intervals:\nblock: 1"
    )
    # Explicit NAs
    data <- npk
    aggregated <- stats::aggregate(formula = yield ~ N + P + block, data = data, FUN = mean)
    aggregated$yield[5] <- NA
    expect_warning(
      wsci(data = aggregated, id = "block", dv = "yield", factors = c("N", "P"))
      , "Because of NAs in the dependent variable, the following cases were removed from calculation of within-subjects confidence intervals:\nblock: 2"
    )
  }
)


test_that(
  "Within-subjects confidence intervals: Throw error if data are not properly aggregated."
  , {
    expect_error(
      wsci(data = npk, id = "block", factors = "N", dv = "yield")
      , "More than one observation per cell. Ensure you aggregated multiple observations per participant/within-subjects condition combination."
    )
  }
)

test_that(
  "conf_int(): Throw message if not enough observations"
  , {
    expect_message(conf_int(1), "Less than two non-missing values in at least one cell of your design: Thus, no confidence interval can be computed.")
    expect_message(conf_int(NA), "Less than two non-missing values in at least one cell of your design: Thus, no confidence interval can be computed.")
  }
)
