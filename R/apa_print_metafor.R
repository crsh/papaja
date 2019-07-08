apa_print.metafor <- function(
  x
  , est_name = NULL
  , ci = 0.95
  , in_paren = FALSE
  , ...
){

  validate(x, check_class = "rma")

  est_name <- "b"

#  if(is.matrix(ci)) {
#    conf_level <- as.numeric(gsub("[^.|\\d]", "", colnames(ci), perl = TRUE))
#    conf_level <- 100 - conf_level[1] * 2
#  } else {
#    conf_level <- 100 * ci
#  }

  regression_table <- arrange_regression(
    x
    , est_name = est_name
    , standardized = FALSE
    , ci = ci
    , ...
  )

  # Concatenate character strings and return as named list
  apa_res <- apa_glm_res(regression_table, in_paren = in_paren, conf_level = conf_level)
  names(apa_res$estimate) <- sanitize_terms(names(x$coefficients))
  names(apa_res$statistic) <- names(apa_res$estimate)
  names(apa_res$full_result) <- names(apa_res$estimate)

  # Model fit
  glance_x <- broom::glance(x)
  apa_res$estimate$modelfit$aic <- paste0("$\\mathrm{AIC} = ", printnum(glance_x$AIC), "$")
  apa_res$estimate$modelfit$bic <- paste0("$\\mathrm{BIC} = ", printnum(glance_x$BIC), "$")

  apa_res
}

