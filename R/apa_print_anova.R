#' Format statistics from ANOVA (APA 6th edition)
#'
#' These methods take objects from various R functions that calculate ANOVA to create formated chraracter
#' strings to report the results in accordance with APA manuscript guidelines. \code{anova}-objects from e.g. model comparisons are currently
#' only supported for \code{lm}-objects.
#'
#' @param x Output object. See details.
#' @param es Character. The effect-size measure to be calculated; can be either \code{ges} for generalized eta-squared or \code{pes} for partial eta-squared.
#' @param observed Character. The names of the factors that are observed, (i.e., not manipulated). Necessary for calculation of generalized eta-squared; otherwise ignored.
#' @param correction Character. In the case of repeated-measures ANOVA, the type of sphericity correction to be used. Either \code{GG} for Greenhouse-Geisser or \code{HF} for Huyn-Feldt methods or \code{none} is also possible.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses. See details.
#' @param models List. List containing fitted \code{lm}- objects that were compared using \code{anova()}. If the list is named, element names are used as model names in the ouptut object.
#' @param ci Numeric. Confidence level for the confidence interval for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}. If \code{ci = NULL} no confidence intervals are estimated.
#' @param boot_samples Numeric. Number of bootstrap samples to estimate confidence intervals for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}; ignored if \code{ci = NULL}.
#' @param ... Additional arguments passed to or from other methods.
#' @details
#'    Currently, methods for the following objects are available:
#'    \itemize{
#'      \item{\code{aov}}
#'      \item{\code{summary.aov}}
#'      \item{\code{aovlist}}
#'      \item{\code{summary.aovlist}}
#'      \item{\code{anova}}
#'      \item{\code{Anova.mlm}}
#'    }
#'
#'    The factor names are sanitized to facilitate their use as list names (see Value section). Parentheses
#'    are omitted and other non-word characters are replaced by \code{_}.
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#'    As demonstrated by Algina, Keselman & Penfield (2007), asymptotic confidence intervals for \eqn{\Delta R^2}
#'    are often unreliable. Confidence intervals for model comparisons of \code{lm}-objects are, therefore, estimated
#'    using their modified percentile bootstrap method. Note that the accuracy of the confidence intervals depends on
#'    the number of predictors \eqn{p}, their distribution, and the sample size \eqn{n}:
#'
#'    \emph{"When the predictor distribution is multivariate normal, one can obtain accurate CIs for \eqn{\rho^2} with
#'    \eqn{n \geq 50} when \eqn{p = 3}. For \eqn{p = 6} and for \eqn{p = 9}, \eqn{n \geq 100} is advisable. When the
#'    predictor distribution is nonnormal in form, sample size requirements vary with type of nonnormality." (p. 939)}
#' @return
#'    \code{apa_print.aov} and related functions return a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{stat}}{A named list of character strings giving the test statistic, parameters, and \emph{p}
#'          value for each factor.}
#'      \item{\code{est}}{A named list of character strings giving the effect size estimates for each factor.} % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full}}{A named list of character strings comprised of \code{est} and \code{stat} for each factor.}
#'      \item{\code{table}}{A data.frame containing the complete ANOVA table, which can be passed to \code{\link{apa_table}}.}
#'    }
#' @references
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence Intervals for an Effect Size Measure in Multiple Linear Regression.
#'    \emph{Educational and Psychological Measurement}, 67(2), 207--218. doi:\href{http://dx.doi.org/10.1177/0013164406292030}{10.1177/0013164406292030}
#'
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2010). Confidence Intervals for Squared Semipartial Correlation Coefficients: The Effect of Nonnormality.
#'    \emph{Educational and Psychological Measurement}, 70(6), 926--940. doi:\href{http://dx.doi.org/10.1177/0013164410379335}{10.1177/0013164410379335}
#'
#'    Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. \emph{Behavior Research Methods}
#'    , 37 (3), 379--384. doi:\href{http://dx.doi.org/10.3758/BF03192707}{10.3758/BF03192707}
#'
#' @family apa_print
#' @seealso \code{\link{aov}}, \code{\link[car]{Anova}}
#' @examples
#'    ## From Venables and Ripley (2002) p. 165.
#'    npk_aov <- aov(yield ~ block + N * P * K, npk)
#'    apa_print(npk_aov)
#' @export

apa_print.aov <- function(
  x
  , es = "ges"
  , observed = NULL
  , in_paren = FALSE
  , ...
) {
  df <- arrange_anova(x)

  print_anova(df, es = es, observed = observed, in_paren = in_paren)
}


#' @rdname apa_print.aov
#' @method apa_print summary.aov
#' @export

apa_print.summary.aov <- function(
  x
  , es = "ges"
  , observed = NULL
  , in_paren = FALSE
  , ...
) {
  df <- arrange_anova(x)

  print_anova(df, es = es, observed = observed, in_paren = in_paren)
}


#' @rdname apa_print.aov
#' @method apa_print aovlist
#' @export

apa_print.aovlist <- function(
  x
  , es = "ges"
  , observed = NULL
  , in_paren = FALSE
  , ...
) {
  summary_x <- summary(x)

  apa_print.summary.aovlist(summary_x, es = es, observed = observed, in_paren = in_paren)
}


#' @rdname apa_print.aov
#' @method apa_print summary.aovlist
#' @export

apa_print.summary.aovlist <- function(
  x
  , es = "ges"
  , observed = NULL
  , in_paren = FALSE
  , ...
) {
  x <- lapply(x, arrange_anova.summary.aov)
  df <- do.call("rbind", x)
  df <- data.frame(df, row.names = NULL)

  print_anova(df, es = es, observed = observed, in_paren = in_paren)
}


#' @rdname apa_print.aov
#' @method apa_print anova
#' @export

apa_print.anova <- function(
  x
  , es = "ges"
  , observed = NULL
  , in_paren = FALSE
  , models = NULL
  , ci = 0.90
  , boot_samples = 1000
  , ...
) {
  if(!is.null(ci)) validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))

  df <- arrange_anova(x)

  if(
    any(grepl("Model 1", attr(x, "heading")) & grepl("Model 2", attr(x, "heading"))) ||
      is.null(x[["Sum Sq"]])
  ) {
    return(print_model_comp(df, in_paren = in_paren, models = models, ci = ci, boot_samples = boot_samples))
  } else {
    return(print_anova(df, es = es, observed = observed, in_paren = in_paren))
  }
}


#' @rdname apa_print.aov
#' @method apa_print Anova.mlm
#' @export

apa_print.Anova.mlm <- function(
  x
  , correction = "GG"
  , es = "ges"
  , observed = NULL
  , in_paren = FALSE
  , ...
) {
  x <- summary.Anova.mlm(x)
  #x$sphericity.tests
  tmp <- x$univariate.tests
  class(tmp) <- NULL
  t.out <- data.frame(tmp)
  colnames(t.out) <- colnames(tmp)

  if(nrow(x$sphericity.tests) > 0) {
    if (correction[1] == "GG") {
      t.out[row.names(x$pval.adjustments), "num Df"] <- t.out[row.names(x$pval.adjustments), "num Df"] * x$pval.adjustments[, "GG eps"]
      t.out[row.names(x$pval.adjustments), "den Df"] <- t.out[row.names(x$pval.adjustments), "den Df"] * x$pval.adjustments[, "GG eps"]
      t.out[row.names(x$pval.adjustments), "Pr(>F)"] <- x$pval.adjustments[,"Pr(>F[GG])"]
    } else {
      if (correction[1] == "HF") {
        if (any(x$pval.adjustments[,"HF eps"] > 1)) warning("HF eps > 1 treated as 1")
        t.out[row.names(x$pval.adjustments), "num Df"] <- t.out[row.names(x$pval.adjustments), "num Df"] * pmin(1, x$pval.adjustments[, "HF eps"])
        t.out[row.names(x$pval.adjustments), "den Df"] <- t.out[row.names(x$pval.adjustments), "den Df"] * pmin(1, x$pval.adjustments[, "HF eps"])
        t.out[row.names(x$pval.adjustments), "Pr(>F)"] <- x$pval.adjustments[,"Pr(>F[HF])"]
      } else {
        if (correction[1] == "none") {
          TRUE
        } else stop("Correction not supported. 'correction' must either be 'GG' or 'HF'.")
      }
    }
  }

  df <- as.data.frame(t.out)

  # obtain positons of statistics in data.frame
  old <- c("SS", "num Df", "Error SS", "den Df", "F", "Pr(>F)")
  nu <- c("sumsq", "df", "sumsq_err", "df_res", "statistic", "p.value")
  colnames(df) == old
  for (i in 1:length(old)){
    colnames(df)[colnames(df) == old[i]] <- nu[i]
  }

  df$term <- rownames(df)
  df <- data.frame(df, row.names = NULL)

  print_anova(df, es = es, observed = observed, in_paren = in_paren)
}



print_anova <- function(
  x
  , observed = NULL
  , es = "ges"
  , in_paren = FALSE
) {
  validate(x, check_class = "data.frame")
  if(!is.null(observed)) validate(observed, check_class = "character")
  validate(es, check_class = "character")
  validate(in_paren, check_class = "logical", check_length = 1)

  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }

  rownames(x) <- sanitize_terms(x$term)

  # Calculate generalized eta squared
  ## This code is a copy from afex by Henrik Singmann who said that it is basically a copy
  ## from ezANOVA by Mike Lawrence
  if(!is.null(observed)) {
    obs <- rep(FALSE, nrow(x))
    for(i in observed){
      if (!any(grepl(paste0("\\<", i, "\\>", collapse = "|"), rownames(x)))) stop(paste0("Observed variable not in data: ", i, collapse = " "))
      obs <- obs | grepl(paste0("\\<", i, "\\>", collapse = "|"), rownames(x))
    }
    obs_SSn1 <- sum(x$sumsq*obs)
    obs_SSn2 <- x$sumsq*obs
  } else {
    obs_SSn1 <- 0
    obs_SSn2 <- 0
  }
  x$ges <- x$sumsq / (x$sumsq + sum(unique(x$sumsq_err)) + obs_SSn1 - obs_SSn2)

  # Calculate partial eta squared
  x$pes <- x$sumsq / (x$sumsq + x$sumsq_err)

  # Rounding and filling with zeros
  x$statistic <- printnum(x$statistic, digits = 2)
  x$p.value <- printp(x$p.value)

  x[, c("df", "df_res")] <- apply(X = x[, c("df", "df_res")], MARGIN = c(1, 2), FUN = function(y){
    as.character(round(y, digits = 2))
    }
  )
  x[, c("ges","pes")] <- printnum(x[, c("ges","pes")], digits = 2, margin = 2, gt1 = FALSE)

  anova_table <- data.frame(x[, c("term", "statistic", "df", "df_res", "p.value", es)], row.names = NULL)
  anova_table[["term"]] <- prettify_terms(anova_table[["term"]])
  if("ges" %in% es) {
    es_long <-"$\\eta^2_G$"
  } else if("pes" %in% es) {
    es_long <-"$\\eta^2_p$"
  }
  colnames(anova_table) <- c("Term", "$F$", "$df_1$", "$df_2$", "$p$", es_long)

  # Add 'equals' where necessary
  eq <- (1:nrow(x))[!grepl(x$p.value, pattern = "<|>|=")]
  for (i in eq) {
    x$p.value[i] <- paste0("= ", x$p.value[i])
  }

  # Concatenate character strings and return as named list
  apa_res <- list()

  apa_res$stat <- apply(x[, -1], 1, function(y) {
    paste0("$F", op, y["df"], ", ", y["df_res"], cp, " = ", y["statistic"], "$, $p ", y["p.value"], "$")
  })

  apa_res$est <- apply(x[, -1], 1, function(y) {
    apa_est <- c()
    if("pes" %in% es) {
      apa_est <- c(apa_est, paste0("$\\eta^2_p = ", y["pes"], "$"))
    }
    if("ges" %in% es) {
      apa_est <- c(apa_est, paste0("$\\eta^2_G = ", y["ges"], "$"))
    }
    apa_est <- paste(apa_est, collapse = ", ")
  })

  apa_res$full <- paste(apa_res$stat, apa_res$est, sep = ", ")

  names(apa_res$full) <- names(apa_res$est)
  apa_res <- lapply(apa_res, as.list)
  apa_res$table <- as.data.frame(anova_table)
  apa_res
}

print_model_comp <- function(
  x
  , in_paren = FALSE
  , models = NULL
  , ci = NULL
  , boot_samples = 1000
) {
  validate(x, check_class = "data.frame")
  validate(in_paren, check_class = "logical", check_length = 1)
  if(!is.null(models)) validate(models, check_class = "list", check_length = nrow(x) + 1)

  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }

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
    model_hierarchy <- sort(r2s, index.return = TRUE)$ix
    delta_r2s <- diff(r2s[model_hierarchy])

    apa_res$est <- sapply(
      seq_along(delta_r2s)
      , function(y) {
        delta_r2_res <- printnum(delta_r2s[y], gt1 = FALSE, zero = FALSE)
        eq <- if(grepl(delta_r2_res, pattern = "<|>|=")) "" else " = "
        paste0("$\\Delta R^2 ", eq, delta_r2_res, "$")
      }
    )
  } else { # Bootstrap CI
    model_summaries <- lapply(models, summary)
    r2s <- sapply(model_summaries, function(x) x$r.squared)
    model_hierarchy <- sort(r2s, index.return = TRUE)$ix
    delta_r2s <- diff(r2s[model_hierarchy])
    model_summaries <- model_summaries[model_hierarchy]

    apa_res$est <- sapply(
      seq_along(delta_r2s)
      , function(y) {

        # Modified percentile bootstrap
        # Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence Intervals for an Effect Size Measure in Multiple Linear Regression.
        # Educational and Psychological Measurement, 67(2), 207–218. http://doi.org/10.1177/0013164406292030

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
          , R = boot_samples
        )

        boot_r2_ci <- boot::boot.ci(delta_r2_samples, conf = 0.95, type = "perc")
        if(x[y, "p.value"] >= 0.05) boot_r2_ci$percent[1, 4] <- 0 # Algina, Keselman & Penfield (2007), p. 210

        delta_r2_res <- printnum(delta_r2s[y], gt1 = FALSE, zero = FALSE)
        eq <- if(grepl(delta_r2_res, pattern = "<|>|=")) "" else " = "

        paste0(
          "$\\Delta R^2 ", eq, delta_r2_res, "$, "
          , print_confint(boot_r2_ci$percent[1, 4:5], conf_level = boot_r2_ci$percent[1, 1], gt1 = FALSE)
        )
      }
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
    paste0("$F", op, y["df"], ", ", y["df_res"], cp, " = ", y["statistic"], "$, $p ", y["p.value"], "$")
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
  model_summaries <- lapply(models, function(x) { # Merge b and 95% CI
      lm_table <- apa_print(x)$table[, c(1:3)]
      lm_table[, 2] <- apply(lm_table[, 2:3], 1, paste, collapse = " ")
      lm_table[, -3]
    }
  )

  ## Sort models
  n_terms <- sapply(model_summaries, nrow)
  model_summaries <- model_summaries[sort(n_terms, index.return = TRUE)$ix]
  n_models <- length(models)
  n_terms <- max(n_terms)
  model_summaries <- lapply(model_summaries, function(x) { # Fill data.frames with empty rows
      term_diff <- n_terms - nrow(x)
      if(term_diff > 0) {
        for(i in 1:(term_diff)) {
          x <- rbind(x, "")
        }
      }
      x
    }
  )
  coef_table <- do.call(cbind, model_summaries)[, seq(2, (2 * n_models), 2)] # Removes Term columns
  rownames(coef_table) <- model_summaries[[n_models]][, "Variable"]
  colnames(coef_table) <- paste("Model", 1:n_models)

  model_fits <- lapply(models, broom::glance)
  model_fits <- do.call(rbind, model_fits)
  model_fits <- model_fits[, c("r.squared", "statistic", "df", "df.residual", "p.value", "AIC", "BIC")]
  model_diffs <- apply(model_fits[, c("r.squared", "AIC", "BIC")], 2, diff)

  model_fits <- printnum(
    model_fits
    , margin = 2
    , gt1 = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
    , zero = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
    , digits = c(2, 2, 0, 0, 3, 2, 2)
  )
  colnames(model_fits) <- c("$R^2$", "$F$", "$df_1$", "$df_2$", "$p$", "$AIC$", "$BIC$")

  model_diffs <- printnum(
    model_diffs
    , margin = 2
    , gt1 = c(FALSE, TRUE, TRUE)
    , zero = c(FALSE, TRUE, TRUE)
  )
  model_diffs <- rbind("", model_diffs)
  colnames(model_diffs) <- c("$\\Delta R^2$", "$\\Delta AIC$", "$\\Delta BIC$")

  diff_stats <- x[, c("statistic", "df", "df_res", "p.value")]
  rownames(diff_stats) <- paste("Model", 2:n_models)
  colnames(diff_stats) <- c("$F$ ", "$df_1$ ", "$df_2$ ", "$p$ ") # Space enable duplicate row names
  diff_stats <- rbind("", diff_stats)

  model_stats_table <- t(cbind(model_fits, model_diffs, diff_stats))
  colnames(model_stats_table) <- paste("Model", 1:n_models)
  apa_res$table <- rbind(coef_table, model_stats_table)

  apa_res <- lapply(apa_res, as.list)
  apa_res
}




## Helper functions
arrange_anova <- function(x) UseMethod("arrange_anova", x)

arrange_anova.anova <- function(x) {
  object <- as.data.frame(x)
  resid_row <- apply(object, 1, function(x) any(is.na(x)))
  x <- data.frame(array(NA, dim = c(nrow(object) - 1, 7)), row.names = NULL) # Create empty object
  colnames(x) <- c("term", "sumsq", "df", "sumsq_err", "df_res", "statistic", "p.value")

  # Model comparison
  if(any(grepl("Model 1", attr(object, "heading")) & grepl("Model 2", attr(object, "heading")))) {

    x[, c("sumsq", "df", "statistic", "p.value")] <- object[!resid_row, c("Sum of Sq", "Df", "F", "Pr(>F)")]
    x$df <- abs(x$df) # Objects give difference in Df
    x$sumsq_err <- object[!resid_row, "RSS"]
    x$df_res <- object[resid_row, "Res.Df"]
    x$term <- paste0("model", 2:nrow(object))

  } else if(is.null(object[["Sum Sq"]])) {
      x <- x[, -which(colnames(x) %in% c("sumsq", "sumsq_err"))]

      x[, c("df", "statistic", "p.value")] <- object[!resid_row, c("Df", "F value", "Pr(>F)")]
      x$df_res <- object[resid_row, "Df"]
      x$term <- rownames(object)[!resid_row]

  } else { # Analysis of variance

    x[, c("sumsq", "df", "statistic", "p.value")] <- object[!resid_row, c("Sum Sq", "Df", "F value", "Pr(>F)")]
    x$sumsq_err <- object[resid_row, "Sum Sq"]
    x$df_res <- object[resid_row, "Df"]
    x$term <- rownames(object)[!resid_row]

  }

  x
}

arrange_anova.aov <- function(x) {
  tidy_x <- broom::tidy(x)
  tidy_x$sumsq_err <- tidy_x[nrow(tidy_x), "sumsq"]
  tidy_x$df_res <- tidy_x[nrow(tidy_x), "df"]
  tidy_x <- tidy_x[-nrow(tidy_x), ]
  tidy_x
}

arrange_anova.summary.aov <- function(x) {
  arranged_aov <- arrange_anova.aov(x[[1]])
  arranged_aov
}

# stolen from 'car' package
#' @method summary Anova.mlm
#' @export

summary.Anova.mlm <- function (object, test.statistic, univariate=TRUE, multivariate=TRUE, ...) {
  GG <- function(SSPE, P) { # Greenhouse-Geisser correction
    p <- nrow(SSPE)
    if (p < 2)
      return(NA)
    lambda <- eigen(SSPE %*% solve(t(P) %*% P))$values
    lambda <- lambda[lambda > 0]
    ((sum(lambda)/p)^2)/(sum(lambda^2)/p)
  }
  HF <- function(gg, error.df, p) { # Huynh-Feldt correction
    ((error.df + 1) * p * gg - 2)/(p * (error.df - p * gg))
  }
  mauchly <- function(SSD, P, df) {
    # most of this function borrowed from stats:::mauchly.test.SSD
    if (nrow(SSD) < 2)
      return(c(NA, NA))
    Tr <- function(X) sum(diag(X))
    p <- nrow(P)
    I <- diag(p)
    Psi <- t(P) %*% I %*% P
    B <- SSD
    pp <- nrow(SSD)
    U <- solve(Psi, B)
    n <- df
    logW <- log(det(U)) - pp * log(Tr(U/pp))
    rho <- 1 - (2 * pp^2 + pp + 2)/(6 * pp * n)
    w2 <- (pp + 2) * (pp - 1) * (pp - 2) * (2 * pp^3 + 6 *
                                              pp^2 + 3 * p + 2)/(288 * (n * pp * rho)^2)
    z <- -n * rho * logW
    f <- pp * (pp + 1)/2 - 1
    Pr1 <- pchisq(z, f, lower.tail = FALSE)
    Pr2 <- pchisq(z, f + 4, lower.tail = FALSE)
    pval <- Pr1 + w2 * (Pr2 - Pr1)
    c(statistic = c(W = exp(logW)), p.value = pval)
  }
  if (missing(test.statistic))
    test.statistic <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
  test.statistic <- match.arg(test.statistic, c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"), several.ok = TRUE)
  nterms <- length(object$terms)
  summary.object <- list(type=object$type, repeated=object$repeated,
                         multivariate.tests=NULL, univariate.tests=NULL,
                         pval.adjustments=NULL, sphericity.tests=NULL)
  if (multivariate){
    summary.object$multivariate.tests <- vector(nterms, mode="list")
    names(summary.object$multivariate.tests) <- object$terms
    summary.object$SSPE <- object$SSPE
    for (term in 1:nterms) {
      hyp <- list(SSPH = object$SSP[[term]],
                  SSPE = if (object$repeated) object$SSPE[[term]] else object$SSPE,
                  P = if (object$repeated) object$P[[term]] else NULL,
                  test = test.statistic, df = object$df[term],
                  df.residual = object$error.df, title = object$terms[term])
      class(hyp) <- "linearHypothesis.mlm"
      summary.object$multivariate.tests[[term]] <- hyp
    }
  }
  if (object$repeated && univariate) {
    singular <- object$singular
    error.df <- object$error.df
    table <- matrix(0, nterms, 6)
    table2 <- matrix(0, nterms, 4)
    table3 <- matrix(0, nterms, 2)
    rownames(table3) <- rownames(table2) <- rownames(table) <- object$terms
    colnames(table) <- c("SS", "num Df", "Error SS", "den Df", "F", "Pr(>F)")
    colnames(table2) <- c("GG eps", "Pr(>F[GG])", "HF eps","Pr(>F[HF])")
    colnames(table3) <- c("Test statistic", "p-value")
    if (singular)
      warning("Singular error SSP matrix:\nnon-sphericity test and corrections not available")
    for (term in 1:nterms) {
      SSP <- object$SSP[[term]]
      SSPE <- object$SSPE[[term]]
      P <- object$P[[term]]
      p <- ncol(P)
      PtPinv <- solve(t(P) %*% P)
      gg <- if (!singular) GG(SSPE, P) else NA
      table[term, "SS"] <- sum(diag(SSP %*% PtPinv))
      table[term, "Error SS"] <- sum(diag(SSPE %*% PtPinv))
      table[term, "num Df"] <- object$df[term] * p
      table[term, "den Df"] <- error.df * p
      table[term, "F"] <- (table[term, "SS"]/table[term, "num Df"])/
        (table[term, "Error SS"]/table[term, "den Df"])
      table[term, "Pr(>F)"] <- pf(table[term, "F"], table[term, "num Df"], table[term, "den Df"],
                                  lower.tail = FALSE)
      table2[term, "GG eps"] <- gg
      table2[term, "HF eps"] <- if (!singular) HF(gg, error.df, p) else NA
      table3[term, ] <- if (!singular) mauchly(SSPE, P, object$error.df) else NA
    }
    table3 <- na.omit(table3)
    if (nrow(table3) > 0) {
      table2[, "Pr(>F[GG])"] <- pf(table[, "F"], table2[, "GG eps"] *
                                     table[, "num Df"], table2[, "GG eps"] * table[, "den Df"],
                                   lower.tail = FALSE)
      table2[, "Pr(>F[HF])"] <- pf(table[, "F"], pmin(1, table2[, "HF eps"]) *
                                     table[, "num Df"], pmin(1, table2[, "HF eps"]) * table[, "den Df"],
                                   lower.tail = FALSE)
      table2 <- na.omit(table2)
      if (any(table2[, "HF eps"] > 1)) warning("HF eps > 1 treated as 1")
    }
    class(table3) <- class(table) <- "anova"
    summary.object$univariate.tests <- table
    summary.object$pval.adjustments <- table2
    summary.object$sphericity.tests <- table3
  }
  class(summary.object) <- "summary.Anova.mlm"
  summary.object
}


