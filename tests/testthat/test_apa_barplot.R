context("apa_barplot()")

test_that(
  "Within-subjects confidence intervals"
  , {
    ee <- apa_barplot(data = npk, dv = "yield", id = "block", factors = c("N", "P"), dispersion = wsci)$ee
    mat <- tapply(ee$yield, list(ee$N, ee$P), FUN = as.numeric)

    grand_mean <- mean(npk$yield)
    individual_means <- tapply(npk$yield, npk$block, mean)
    # standardised observations:
    for (i in 1:nrow(npk)) {
      npk[i, "standardised"] <- npk[i, "yield"] - individual_means[npk[i, "block"]] + grand_mean
    }
    CI <- tapply(npk$standardised, list(npk$N, npk$P), FUN = conf_int)
    CI <- 4/3*CI # Morey correction

    expect_equal(ee, CI)
  }
)
