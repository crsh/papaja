# Modified percentile bootstrap
# Algina, J., Keselman, H. J., & Penfield, R. D. (2007). ciidence Intervals for an Effect Size Measure in Multiple Linear Regression.
# Educational and Psychological Measurement, 67(2), 207–218. http://doi.org/10.1177/0013164406292030

# x: apa_model_comp object
# models: list of lm-objects

delta_r2_ci <- function(x, models, ci = 0.90, R = 100, ...) {
  # validate(x, check_class = "list")
  # validate(length(x), "length(x)", check_range = c(2, Inf))
  # validate(ci, check_range = c(0, 1))

  model_summaries <- lapply(models, summary)
  r2s <- sapply(model_summaries, function(x) x$r.squared)
  delta_r2s <- diff(r2s)

  percent_cis <- lapply(seq_along(delta_r2s), function(y) {

    delta_r2_samples <- boot::boot(
      get(as.character(model_summaries[[y]]$call$data))
      , function(data, i, calls) {
        bdata <- data[i, ]

        calls[[1]]$data <- bdata
        mod1 <- eval(calls[[1]])

        calls[[2]]$data <- bdata
        mod2 <- eval(calls[[2]])

        summary(mod2)$r.squared - summary(mod1)$r.squared
      }
      , calls = list(model_summaries[[y]]$call, model_summaries[[y + 1]]$call)
      , R = R
      , ...
    )

    boot_r2_ci <- boot::boot.ci(delta_r2_samples, ci = ci, type = "perc")

    # If difference is not significant set lower bound (closest to zero) == 0 (p. 210, Algina, Keselman & Penfield, 2007)
    if(x[y, "p.value"] >= (1 - ci) / 2) {
      lower_bound <- which(boot_r2_ci$percent[1, 4:5] == min(abs(boot_r2_ci$percent[1, 4:5])))
      boot_r2_ci$percent[1, 3 + lower_bound] <- 0
    }

    boot_r2_ci
  })

  percent_cis <- t(cbind(sapply(percent_cis, function(x) x$percent))) # Reformat to one CI per line
  percent_cis <- percent_cis[, 4:5, drop = FALSE] # Preserv matrix structure
  ci_levels <- c((1 - ci) / 2, (1 - ci) / 2 + ci) * 100
  colnames(percent_cis) <- paste(ci_levels, "%")
  attr(percent_cis, "ci.level") <- ci

  percent_cis
}
