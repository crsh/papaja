#' Format statistics from ANOVA (APA 6th edition)
#'
#' This function is the workhorse of the \code{apa_print}-family for ANOVA. It takes a \code{data.frame}
#' of class \code{apa_variance_table} and produces strings to report the results in accordance with APA manuscript
#' guidelines. \emph{This function is not exported.}
#'
#' @param x Data.frame. A \code{data.frame} of class \code{apa_variance_table} as returned by \code{\link{arrange_anova}}.
#' @param es Character. The effect-size measure to be calculated; can be either \code{ges} for generalized eta-squared or \code{pes} for partial eta-squared.
#' @param observed Character. The names of the factors that are observed, (i.e., not manipulated). Necessary for calculation of generalized eta-squared; otherwise ignored.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses. See details.

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


print_anova <- function(
  x
  , observed = NULL
  , es = "ges"
  , in_paren = FALSE
) {
  validate(x, check_class = "data.frame")
  validate(x, check_class = "apa_variance_table")
  if(!is.null(observed)) validate(observed, check_class = "character")
  validate(es, check_class = "character")
  if(!all(es %in% c("pes", "ges"))) stop("Requested effect size measure(s) currently not supported: ", paste(es, collapse = ", "), ".")
  validate(in_paren, check_class = "logical", check_length = 1)

  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }

  rownames(x) <- sanitize_terms(x$term)

  # Calculate generalized eta squared
  if("ges" %in% es) {
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
  }

  # Calculate partial eta squared
  if("pes" %in% es) {
    x$pes <- x$sumsq / (x$sumsq + x$sumsq_err)
  }

  # Rounding and filling with zeros
  x$statistic <- printnum(x$statistic, digits = 2)
  x$p.value <- printp(x$p.value)
  x[, c("df", "df_res")] <- apply(x[, c("df", "df_res")],  c(1, 2), function(y) as.character(round(y, digits = 2)))
  x[, es] <- printnum(x[, es], digits = 3, margin = 2, gt1 = FALSE)

  # Assemble table
  anova_table <- data.frame(x[, c("term", "statistic", "df", "df_res", "p.value", es)], row.names = NULL)
  anova_table[["term"]] <- prettify_terms(anova_table[["term"]])

  ## Define appropriate column names
  es_long <- c()
  if("pes" %in% es) {
    es_long <- c(es_long, "$\\eta^2_p$")
  }
  if("ges" %in% es) {
    es_long <- c(es_long, "$\\eta^2_G$")
  }

  correction_type <- attr(x, "correction")
  if(!is.null(correction_type) && correction_type != "none") {
    colnames(anova_table) <- c("Effect", "$F$", paste0("$df_1^{", correction_type, "}$"), paste0("$df_2^{", correction_type, "}$"), "$p$", es_long)
  } else {
    colnames(anova_table) <- c("Effect", "$F$", "$df_1$", "$df_2$", "$p$", es_long)
  }

  ## Add 'equals' where necessary
  eq <- (1:nrow(x))[!grepl(x$p.value, pattern = "<|>|=")]
  for (i in eq) {
    x$p.value[i] <- paste0("= ", x$p.value[i])
  }

  # Concatenate character strings and return as named list
  apa_res <- list()

  apa_res$stat <- apply(x, 1, function(y) {
    paste0("$F", op, y["df"], ", ", y["df_res"], cp, " = ", y["statistic"], "$, $p ", y["p.value"], "$")
  })

  apa_res$est <- apply(x, 1, function(y) {
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
