#' Typeset Statistical Results from Model Comparisons
#'
#' This function is the workhorse of the [apa_print()][apa_print.list()] method
#' for model comparisons. It takes a data frame of class `apa_model_comp` and
#' produces strings to report the results in accordance with APA manuscript
#' guidelines.
#' *This function is not exported.*
#'
#' @param x A data frame of class `apa_variance_table` as returned by [arrange_anova()].
#' @param models List. List containing fitted `lm` objects that were compared using [anova()]. If the list is named, element names are used as model names in the output object.
#' @param conf.int Numeric. Confidence level for the confidence interval for \eqn{\Delta R^2} if `x` is a model comparison object of class `anova`. If `conf.int = NULL` no confidence intervals are estimated.
#' @param boot_samples Numeric. Number of bootstrap samples to estimate confidence intervals for \eqn{\Delta R^2} if `x` is a model comparison object of class `anova`; ignored if `conf.int = NULL`.
#' @param progress_bar Logical. Determines whether a progress bar is printed while bootstrapping.
#' @inheritParams glue_apa_results
#' @inheritParams apa_print.glm
#' @return
#'    A named list (with additional class `apa_results`) containing the following components:
#'
#'    \describe{
#'      \item{`statistic`}{
#'        A named list of character strings giving the test statistic, parameters, and *p* value for each factor.
#'      }
#'      \item{`estimate`}{
#'        A named list of character strings giving the effect size estimates for each factor,
#'        either in units of the analyzed scale or as standardized effect size.
#'      }
#'      \item{`full_result`}{
#'        A named list of character strings comprised of `estimate` and `statistic` for each factor.
#'      }
#'      \item{`table`}{
#'        A data.frame containing the complete comparison table, which can be passed to [apa_table()].
#'      }
#'    }
#'
#' @keywords internal
#' @seealso [arrange_anova()], [apa_print.aov()]
#' @examples
#'  \dontrun{
#'    mod1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#'    mod2 <- update(mod1, formula = . ~ . + Petal.Length)
#'    mod3 <- update(mod2, formula = . ~ . + Petal.Width)
#'
#'    # No bootstrapped Delta R^2 CI
#'    print_model_comp(list(Baseline = mod1, Length = mod2, Both = mod3), boot_samples = 0)
#'  }


print_model_comp <- function(
  x
  , models = NULL
  , conf.int = NULL
  , boot_samples = 1000
  , progress_bar = FALSE
  , in_paren = FALSE
  , observed = TRUE
) {
  validate(x, check_class = "data.frame")
  validate(x, check_class = "apa_model_comp")
  validate(in_paren, check_class = "logical", check_length = 1)
  validate(conf.int, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  if(!is.null(models)) validate(models, check_class = "list", check_length = nrow(x) + 1)

  if(!is.null(names(models))) {
    rownames(x) <- names(models)[-1]
  } else rownames(x) <- sanitize_terms(x$term)

  # Concatenate character strings and return as named list
  apa_res <- init_apa_results()

  ## est
  if(boot_samples <= 0) { # No CI
    model_summaries <- lapply(models, summary)
    r2s <- sapply(model_summaries, function(x) x$r.squared)
    delta_r2s <- diff(r2s)

    apa_res$estimate <- sapply(
      seq_along(delta_r2s)
      , function(y) {
        delta_r2_res <- apa_num(delta_r2s[y], gt1 = FALSE, zero = FALSE)
        paste0("$\\Delta R^2 ", add_equals(delta_r2_res), "$")
      }
    )
  } else { # Bootstrap CI
    boot_r2_ci <- delta_r2_ci(x, models, conf.int = conf.int, R = boot_samples, progress_bar = progress_bar)

    model_summaries <- lapply(models, summary)
    r2s <- sapply(model_summaries, function(x) x$r.squared)
    delta_r2s <- diff(r2s)
    delta_r2_res <- apa_num(delta_r2s, gt1 = FALSE, zero = FALSE)

    apa_res$estimate <- paste0(
      "$\\Delta R^2 ", add_equals(delta_r2_res), "$, ", conf.int * 100, "\\% CI "
      , apply(boot_r2_ci, 1, apa_interval, gt1 = FALSE, enclose_math = TRUE)
    )
  }

  ## stat
  ### Rounding and filling with zeros
  x$statistic <- apa_num(x$statistic)
  x$df <- apa_df(x$df)
  x$df.residual <- apa_df(x$df.residual)
  x$p.value <- apa_p(x$p.value, add_equals = TRUE)

  apa_res$statistic <- paste0("$F(", x[["df"]], ", ", x[["df.residual"]], ") = ", x[["statistic"]], "$, $p ", x[["p.value"]], "$")
  if(in_paren) apa_res$statistic <- in_paren(apa_res$statistic)
  names(apa_res$statistic) <- x$term

  ## full
  apa_res$full_result <- paste(apa_res$estimate, apa_res$statistic, sep = ", ")
  names(apa_res$estimate) <- names(apa_res$statistic)
  names(apa_res$full_result) <- names(apa_res$statistic)


  # Assemble table
  model_summaries <- lapply(models, function(x) { # Merge b and 95% CI
      lm_table <- apa_print(x, conf.int = conf.int + (1 - conf.int) / 2)$table[, c("term", "estimate", "conf.int"), drop = FALSE]
      lm_table[, "estimate"] <- apply(lm_table[, c("estimate", "conf.int"), drop = FALSE], MARGIN = 1, paste, collapse = " ")
      lm_table[, c("term", "estimate"), drop = FALSE]
    }
  )

  ## Merge coefficient tables
  coef_table <- Reduce(function(...) merge(..., by = "term", all = TRUE), model_summaries)
  rownames(coef_table) <- coef_table$term
  coef_table <- coef_table[, colnames(coef_table) != "term"]
  coef_table <- coef_table[names(sort(apply(coef_table, 1, function(x) sum(is.na(x))))), ] # Sort predictors to create steps in table
  coef_table <- coef_table[c("Intercept", rownames(coef_table)[rownames(coef_table) != "Intercept"]), ] # Make Intercept first Predictor
  coef_table[is.na(coef_table)] <- ""
  colnames(coef_table) <- names(models)

  ## Add model fits
  model_fits <- lapply(models, broom::glance)
  model_fits <- do.call(rbind, model_fits)
  model_fits <- model_fits[, c("r.squared", "statistic", "df", "df.residual", "p.value", "AIC", "BIC")]

  diff_vars <- c("r.squared", "AIC", "BIC")
  model_diffs <- apply(model_fits[, diff_vars], 2, diff)
  if(length(models) == 2) {
    model_diffs <- matrix(
      model_diffs
      , ncol = length(diff_vars)
      , byrow = TRUE
      , dimnames = list(NULL, diff_vars)
    )
  }
  model_diffs <- as.data.frame(model_diffs, stringsAsFactors = FALSE)

  model_fits <- apa_num(
    model_fits
    , margin = 2
    , gt1 = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
    , zero = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
    , digits = c(2, 2, 0, 0, 3, 2, 2)
  )

  ## This part is a disaster and needs refactoring
  model_fits$r.squared <- sapply(models, function(x) { # Get R^2 with CI
    r2 <- apa_print(x, conf.int = conf.int + (1 - conf.int) / 2, observed = observed)$estimate$modelfit$r2 # Calculate correct CI for function focusing on b CI
    r2 <- gsub("\\$R\\^2 = |\\$", "", r2)
    r2 <- gsub(", \\d\\d\\\\\\% CI", "", r2)
    # restore dollars where necessary
    r2 <- gsub(pattern = "\\infty", replacement = "$\\infty$", x = r2, fixed = TRUE)
    r2
  })

  colnames(model_fits) <- c(paste0("$R^2$ [", conf.int * 100, "\\% CI]"), "$F$", "$\\mathit{df}$", "$\\mathit{df}_{\\mathrm{res}}$", "$p$", "$\\mathrm{AIC}$", "$\\mathrm{BIC}$")

  ## Add differences in model fits
  model_diffs <- apa_num(
    model_diffs
    , margin = 2
    , gt1 = c(FALSE, TRUE, TRUE)
    , zero = c(FALSE, TRUE, TRUE)
  )
  model_diffs[, "r.squared"] <- gsub(", \\d\\d\\\\\\% CI", "", gsub("\\$\\\\Delta R\\^2 = |\\$$", "", unlist(apa_res$estimate))) # Replace by previous estimate with CI
  model_diffs <- rbind("", model_diffs)

  r2_diff_colname <- if(boot_samples <= 0) "$\\Delta R^2$" else paste0("$\\Delta R^2$ [", conf.int * 100, "\\% CI]")
  colnames(model_diffs) <- c(r2_diff_colname, "$\\Delta \\mathrm{AIC}$", "$\\Delta \\mathrm{BIC}$")

  diff_stats <- x[, c("statistic", "df", "df.residual", "p.value")]
  diff_stats$p.value <- gsub("= ", "", diff_stats$p.value) # Remove 'equals' for table
  colnames(diff_stats) <- c("$F$ ", "$\\mathit{df}$ ", "$\\mathit{df}_{\\mathrm{res}}$ ", "$p$ ") # Space enable duplicate row names
  diff_stats <- rbind("", diff_stats)

  model_stats_table <- as.data.frame(
    t(cbind(model_fits, model_diffs[, 1, drop = FALSE], diff_stats, model_diffs[, 2:3]))
    , stringsAsFactors = FALSE
    , make.names = NA
  )
  colnames(model_stats_table) <- names(models)
  apa_res$table <- rbind(coef_table, model_stats_table)
  apa_res$table[is.na(apa_res$table)] <- ""
  apa_res$table <- default_label(apa_res$table)
  class(apa_res$table) <- c("apa_results_table", "data.frame")

  apa_res[c("estimate", "statistic", "full_result")] <- lapply(apa_res[c("estimate", "statistic", "full_result")], as.list)
  apa_res
}
