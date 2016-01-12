#' Format statistics model comparisons (APA 6th edition)
#'
#' This function is the workhorse of the \code{apa_print.anova} for model comparisons. It takes a \code{data.frame}
#' of class \code{apa_model_comp} and produces strings to report the results in accordance with APA manuscript
#' guidelines. \emph{This function is not exported.}
#'
#' @param x Data.frame. A \code{data.frame} of class \code{apa_variance_table} as returned by \code{\link{arrange_anova}}.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses. See details.
#' @param models List. List containing fitted \code{lm}- objects that were compared using \code{anova()}. If the list is named, element names are used as model names in the ouptut object.
#' @param ci Numeric. Confidence level for the confidence interval for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}. If \code{ci = NULL} no confidence intervals are estimated.
#' @param boot_samples Numeric. Number of bootstrap samples to estimate confidence intervals for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}; ignored if \code{ci = NULL}.
#' @return
#'    A named list containing the following components:
#'
#'    \describe{
#'      \item{\code{stat}}{A named list of character strings giving the test statistic, parameters, and \emph{p}
#'          value for each factor.}
#'      \item{\code{est}}{A named list of character strings giving the effect size estimates for each factor.} % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full}}{A named list of character strings comprised of \code{est} and \code{stat} for each factor.}
#'      \item{\code{table}}{A data.frame containing the complete ANOVA table, which can be passed to \code{\link{apa_table}}.}
#'    }
#'
#' @seealso \code{\link{arrange_anova}}, \code{\link{apa_print.aov}}
#' @examples
#'    ## From Venables and Ripley (2002) p. 165.
#'    npk_aov <- aov(yield ~ block + N * P * K, npk)
#'    anova_table <- arrange_anova(summary(npk_aov))
#'    print_anova(anova_table)


print_model_comp <- function(
  x
  , in_paren = FALSE
  , models = NULL
  , ci = NULL
  , boot_samples = 1000
) {
  validate(x, check_class = "data.frame")
  validate(x, check_class = "apa_model_comp")
  validate(in_paren, check_class = "logical", check_length = 1)
  if(!is.null(models)) validate(models, check_class = "list", check_length = nrow(x) + 1)

  if(!is.null(names(models))) {
    rownames(x) <- names(models)[-1]
  } else rownames(x) <- sanitize_terms(x$term)

  # Concatenate character strings and return as named list
  apa_res <- list()

  if(is.null(models)) { # No CI
    warning("No model objects were supplied to the parameter 'models'. Estimates cannot be calculated and are set to NULL.")
    apa_res$est <- NULL
  } else if(is.null(ci)) { # No CI
    model_summaries <- lapply(models, summary)
    r2s <- sapply(model_summaries, function(x) x$r.squared)
    delta_r2s <- diff(r2s)

    apa_res$est <- sapply(
      seq_along(delta_r2s)
      , function(y) {
        delta_r2_res <- printnum(delta_r2s[y], gt1 = FALSE, zero = FALSE)
        eq <- if(grepl(delta_r2_res, pattern = "<|>|=")) "" else " = "
        paste0("$\\Delta R^2 ", eq, delta_r2_res, "$")
      }
    )
  } else { # Bootstrap CI
    boot_r2_ci <- delta_r2_ci(x, models, ci = ci, R = boot_samples)

    model_summaries <- lapply(models, summary)
    r2s <- sapply(model_summaries, function(x) x$r.squared)
    delta_r2s <- diff(r2s)
    delta_r2_res <- printnum(delta_r2s, gt1 = FALSE, zero = FALSE)
    eq <- ifelse(grepl(delta_r2_res, pattern = "<|>|="), "", " = ")

    apa_res$est <- paste0(
      "$\\Delta R^2", eq, delta_r2_res, "$, ", ci * 100, "\\% CI "
      , apply(boot_r2_ci, 1, print_confint, gt1 = FALSE)
    )
  }

  # Rounding and filling with zeros
  x$statistic <- printnum(x$statistic, digits = 2)
  x$p.value <- printp(x$p.value)
  x[, c("df", "df_res")] <- round(x[, c("df","df_res")], digits = 2)

  # Add 'equals' where necessary
  eq <- (1:nrow(x))[!grepl(x$p.value, pattern = "<|>|=")]
  for (i in eq) {
    x$p.value[i] <- paste0("= ", x$p.value[i])
  }

  apa_res$stat <- apply(x, 1, function(y) {
    stat <- paste0("$F(", y["df"], ", ", y["df_res"], ") = ", y["statistic"], "$, $p ", y["p.value"], "$")
    if(in_paren) stat <- in_paren(stat)
    stat
  })
  names(apa_res$stat) <- x$term

  if(!is.null(apa_res$est)) {
    apa_res$full <- paste(apa_res$est, apa_res$stat, sep = ", ")
    names(apa_res$est) <- names(apa_res$stat)
    names(apa_res$full) <- names(apa_res$stat)
  } else {
    apa_res$full <- NULL
  }

  # Assemble table
  n_models <- length(models)

  model_summaries <- lapply(models, function(x) { # Merge b and 95% CI
    lm_table <- apa_print(x, ci = ci + (1 - ci) / 2)$table[, c(1:3)]
    lm_table[, 2] <- apply(cbind(paste0("$", lm_table[, 2], "$"), lm_table[, 3]), 1, paste, collapse = " ")
    lm_table[, -3]
  }
  )

  ## Merge coefficient tables
  coef_table <- Reduce(function(...) merge(..., by = "Predictor", all = TRUE), model_summaries)
  rownames(coef_table) <- coef_table$Predictor
  coef_table <- subset(coef_table, select = -Predictor)
  coef_table <- coef_table[names(sort(apply(coef_table, 1, function(x) sum(is.na(x))))), ] # Sort predictors to create steps in table
  coef_table <- coef_table[c("Intercept", rownames(coef_table)[rownames(coef_table) != "Intercept"]), ] # Make Intercept first Predictor
  coef_table[is.na(coef_table)] <- ""
  colnames(coef_table) <- paste("Model", 1:n_models)

  ## Add model fits
  model_fits <- lapply(models, broom::glance)
  model_fits <- do.call(rbind, model_fits)
  model_fits <- model_fits[, c("r.squared", "statistic", "df", "df.residual", "p.value", "AIC", "BIC")]
  model_diffs <- apply(model_fits[c("r.squared", "AIC", "BIC")], 2, diff)

  model_fits <- printnum(
    model_fits
    , margin = 2
    , gt1 = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
    , zero = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
    , digits = c(2, 2, 0, 0, 3, 2, 2)
  )
  model_fits$r.squared <- sapply(models, function(x) { # Get R^2 with CI
    r2 <- apa_print(x, ci = ci)$est$modelfit$r2
    r2 <- gsub("R\\^2 = ", "", r2)
    r2 <- gsub(", \\d\\d\\\\\\% CI", "", r2)
    r2
  })
  colnames(model_fits) <- c(paste0("$R^2$ [", ci * 100, "\\% CI]"), "$F$", "$df_1$", "$df_2$", "$p$", "$AIC$", "$BIC$")

  ## Add differences in model fits
  model_diffs <- printnum(
    model_diffs
    , margin = 2
    , gt1 = c(FALSE, TRUE, TRUE)
    , zero = c(FALSE, TRUE, TRUE)
  )
  model_diffs[, "r.squared"] <- gsub(", \\d\\d\\\\\\% CI", "", gsub("\\\\Delta R\\^2 = ", "", unlist(apa_res$est))) # Replace by previously estimate with CI
  model_diffs <- rbind("", model_diffs)
  colnames(model_diffs) <- c(paste0("$\\Delta R^2$ [", ci * 100, "\\% CI]"), "$\\Delta AIC$", "$\\Delta BIC$")

  diff_stats <- x[, c("statistic", "df", "df_res", "p.value")]
  diff_stats$p.value <- gsub("= ", "", diff_stats$p.value) # Remove 'equals' for table
  rownames(diff_stats) <- paste("Model", 2:n_models)
  colnames(diff_stats) <- c("$F$ ", "$df_1$ ", "$df_2$ ", "$p$ ") # Space enable duplicate row names
  diff_stats <- rbind("", diff_stats)

  model_stats_table <- t(cbind(model_fits, model_diffs, diff_stats))
  colnames(model_stats_table) <- paste("Model", 1:n_models)
  apa_res$table <- rbind(coef_table, model_stats_table)
  apa_res$table[is.na(apa_res$table)] <- ""

  apa_res[c("est", "stat", "full")] <- lapply(apa_res[c("est", "stat", "full")], as.list)
  apa_res
}
