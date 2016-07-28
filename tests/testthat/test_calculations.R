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
