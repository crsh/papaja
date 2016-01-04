# Modified percentile bootstrap
# Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence Intervals for an Effect Size Measure in Multiple Linear Regression.
# Educational and Psychological Measurement, 67(2), 207–218. http://doi.org/10.1177/0013164406292030

# x: apa_model_comp object
# models: list of lm-objects

delta_r2_ci <- function(x, models, conf = 0.95, R = 100, ...) {
  # validate(x, check_class = "list")
  # validate(length(x), "length(x)", check_range = c(2, Inf))
  # validate(conf, check_range = c(0, 1))

  model_summaries <- lapply(models, summary)
  r2s <- sapply(model_summaries, function(x) x$r.squared)
  model_hierarchy <- sort(r2s, index.return = TRUE)$ix
  delta_r2s <- diff(r2s[model_hierarchy])
  model_summaries <- model_summaries[model_hierarchy]

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

    boot_r2_ci <- boot::boot.ci(delta_r2_samples, conf = conf, type = "perc")
    if(x[y, "p.value"] >= 0.05) boot_r2_ci$percent[1, 4] <- 0 # Algina, Keselman & Penfield (2007), p. 210

    boot_r2_ci
  })

  percent_cis <- t(cbind(sapply(percent_cis, function(x) x$percent))) # Reformat to one CI per line
  percent_cis <- percent_cis[, 4:5, drop = FALSE] # Preserv matrix structure
  conf_levels <- c((1 - conf) / 2, (1 - conf) / 2 + conf) * 100
  colnames(percent_cis) <- paste(conf_levels, "%")
  attr(percent_cis, "conf.level") <- conf

  percent_cis
}
