apa_print.lm <- function(
  x
  , stat_name = NULL
  , standardized = FALSE
  , ci = 0.95
  , in_paren = FALSE
) {

  validate(x, check_class = "lm")
  if(!is.null(stat_name)) validate(stat_name, check_class = "character", check_length = 1)
  validate(standardized, check_class = "logical", check_length = 1)
  if(!is.null(ci)) {
    if(length(ci) == 1) {
      validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
      ci <- confint(x, level = ci)
    } else {
      validate(ci, check_class = "matrix")
      sapply(ci, validate, check_class = "numeric")
    }
  } else validate(ci)
  validate(in_paren, check_class = "logical", check_length = 1)

  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }


  # Model coefficients
  if(is.null(stat_name)) if(standardized) stat_name <- "b^*" else stat_name <- "b"

  summary_x <- summary(x)
  tidy_x <- broom::tidy(x)
  tidy_x <- cbind(tidy_x, ci) # Adds term rownames
  if(standardized) rownames(tidy_x) <- gsub("scale\\(", "z_", rownames(tidy_x)) # Sanitize term names
  rownames(tidy_x) <- gsub("\\(|\\)", "", rownames(tidy_x)) # Sanitize term names
  rownames(tidy_x) <- gsub("\\W", "_", rownames(tidy_x)) # Sanitize term names
  glance_x <- glance(x)

  apa_res <- list()
  apa_res$stat <- apply(tidy_x[, -1], 1, function(y) {
    p <- printp(y["p.value"])
    if(!grepl("<|>", p)) eq <- "= " else eq <- ""

    paste0("$t", op, glance_x$df.residual, cp, " = ",  printnum(y["statistic"]), "$, $p ", eq, p, "$")
  })

  conf_level <- as.numeric(gsub("[^.|\\d]", "", colnames(ci), perl = TRUE))
  conf_level <- 100 - conf_level[1] * 2

  apa_res$est <- apply(tidy_x[, -1], 1, function(y) {
    paste0(
      "$", stat_name, " = ", printnum(y["estimate"], gt1 = !standardized), "$, "
      , make_confint(y[tail(names(y), 2)], conf_level = conf_level, gt1 = !standardized)
    )
  })

  apa_res$full <- paste(apa_res$est, apa_res$stat, sep = ", ")
  names(apa_res$full) <- names(apa_res$est)

  apa_res <- lapply(apa_res, as.list)


  # Model fit
  p <- printp(glance_x$p.value)
  if(!grepl("<|>", p)) eq <- "= " else eq <- ""

  apa_res$stat$modelfit$r2 <- paste0("$F", op, summary_x$fstatistic[2], ",", glance_x$df.residual, cp, " = ", printnum(glance_x$statistic), "$, $p ", eq, p, "$") # glance_x$df

  ci_conf_level <- (100 - ((100 - conf_level) * 2))
  # Steiger (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests of Close Fit in the Analysis of Variance and Contrast Analysis.
  # Psychological Methods, 9(2), 164-182. doi: 10.1037/1082-989X.9.2.164
  # See also http://daniellakens.blogspot.de/2014/06/calculating-confidence-intervals-for.html
  gibberish <- capture.output(r2_ci <- MBESS::ci.pvaf(
    F.value = glance_x$statistic
    , df.1 = summary_x$fstatistic[2] # glance_x$df
    , df.2 = glance_x$df.residual
    , N = length(x$residuals)
    , conf.level =  ci_conf_level / 100
  ))

  apa_res$est$modelfit$r2 <- paste0("$R^2 = ", printnum(glance_x$r.squared, gt1 = FALSE), "$, ", make_confint(c(r2_ci$Lower, r2_ci$Upper), conf_level = ci_conf_level))
  apa_res$est$modelfit$r2_adj <- paste0("$R^2_{adj} = ", printnum(glance_x$adj.r.squared, gt1 = FALSE), "$")
  apa_res$est$modelfit$aic <- paste0("$AIC = ", printnum(glance_x$AIC), "$")
  apa_res$est$modelfit$bic <- paste0("$AIC = ", printnum(glance_x$BIC), "$")

  apa_res$full$modelfit$r2 <- paste(apa_res$est$modelfit$r2, apa_res$stat$modelfit$r2, sep = ", ")

  apa_res
}


apa_print.summary.lm <- function(x, ...) {
  validate(x, check_class = "summary.lm")
  stop("Please provide the fitted model object of class 'lm' returned by the function lm() instead of the summary object.")
}
