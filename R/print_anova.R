#' Format statistics from ANOVA (APA 6th edition)
#'
#' This function is the internal workhorse of the \code{apa_print}-family for ANOVA. It takes a \code{data.frame}
#' of class \code{apa_variance_table} and produces strings to report the results in accordance with APA manuscript
#' guidelines. It is not ment to be called by the user. \emph{This function is not exported.}
#'
#' @param x Data.frame. A \code{data.frame} of class \code{apa_variance_table} as returned by \code{\link{arrange_anova}}.
#' @param intercept Logical. Indicates if intercept test should be included in output.
#' @param es Character. The effect-size measure to be calculated; can be either \code{ges} for generalized eta-squared, \code{pes} for partial eta-squared or \code{es} for eta-squared.
#'   Note that eta-squared is calculated correctly if and only if the design is balanced.
#' @param mse Logical. Indicates if mean squared errors should be included in output. Default is \code{TRUE}.
#' @param observed Character. The names of the factors that are observed, (i.e., not manipulated). Necessary for calculation of generalized eta-squared; otherwise ignored.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses. See details.

#' @return
#'    A named list containing the following components:
#'
#'    \describe{
#'      \item{\code{statistic}}{A named list of character strings giving the test statistic, parameters, and \emph{p}
#'          value for each factor.}
#'      \item{\code{estimate}}{A named list of character strings giving the effect size estimates for each factor.} % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full_result}}{A named list of character strings comprised of \code{estimate} and \code{statistic} for each factor.}
#'      \item{\code{table}}{A data.frame containing the complete ANOVA table, which can be passed to \code{\link{apa_table}}.}
#'    }
#'
#' @keywords internal
#' @seealso \code{\link{arrange_anova}}, \code{\link{apa_print.aov}}
#' @examples
#'  \dontrun{
#'    ## From Venables and Ripley (2002) p. 165.
#'    npk_aov <- aov(yield ~ block + N * P * K, npk)
#'    anova_table <- arrange_anova(summary(npk_aov))
#'    print_anova(anova_table)
#'  }


print_anova <- function(
  x
  , intercept = FALSE
  , observed = NULL
  , es = "ges"
  , mse = getOption("papaja.mse")
  , in_paren = FALSE
) {

  # When processing aovlist objects, the `(Intercept)` is kept to preserve the
  # SS_error of the intercept   # term to calculate generalized eta squared
  # correctly. This term contains NAs.
  validate(x, check_class = "data.frame", check_NA = FALSE)
  validate(x, check_class = "apa_variance_table", check_NA = FALSE)
  validate(intercept, check_class = "logical", check_length = 1)

  if(!is.null(observed)) validate(observed, check_class = "character")
  if(!is.null(es)) {
    validate(es, check_class = "character")
    es <- sort(es, decreasing = TRUE)
  }
  validate(in_paren, check_class = "logical", check_length = 1)

  rownames(x) <- sanitize_terms(x$term)
  # ----------------------------------------------------------------------------
  # Add effect-size measures and mean-squared errors
  x <- add_effect_sizes(
    x = x
    , es = es
    , observed = observed
    , mse = mse
    , intercept = intercept
  )

  # Remove intercept if the user doesn't want it:
  if(!intercept) x <- x[x$term != "(Intercept)", ]

  # Rounding and filling with zeros
  x$statistic <- printnum(x$statistic, digits = 2)
  x$p.value <- printp(x$p.value)
  x[, c("df", "df_res")] <- apply(x[, c("df", "df_res")],  c(1, 2), function(y) as.character(round(y, digits = 2)))
  if(!is.null(es)) x[, es] <- printnum(x[, es, drop = FALSE], digits = 3, gt1 = FALSE)
  if(mse) x$mse <- printnum(x$mse, digits = 2)

  # Assemble table
  anova_table <- data.frame(x[, c("term", "statistic","df", "df_res", if(mse) "mse" else NULL, "p.value", es)], row.names = NULL)
  anova_table[["term"]] <- prettify_terms(anova_table[["term"]])

  ## Define appropriate column names
  es_long <- c()
  if("pes" %in% es) {
    es_long <- c(es_long, "$\\hat{\\eta}^2_p$")
  }
  if("ges" %in% es) {
    es_long <- c(es_long, "$\\hat{\\eta}^2_G$")
  }
  if("es" %in% es) {
    es_long <- c(es_long, "$\\hat{\\eta}^2$")
  }


  mse_long <- if(mse) "$\\mathit{MSE}$" else NULL
  correction_type <- attr(x, "correction")

  colnames(anova_table) <- c("Effect", "F", "df1", "df2", if(mse){"MSE"}else{NULL}, "p", es)
  class(anova_table) <- "data.frame"

  if(!is.null(correction_type) && correction_type != "none") {
    variable_label(anova_table) <- c(
      "Effect" = "Effect"
      , "F" = "$F$"
      , "df1" = paste0("$\\mathit{df}_1^{", correction_type, "}$")
      , "df2" = paste0("$\\mathit{df}_2^{", correction_type, "}$")
      , "p" = "$p$"
    )
  } else {
    variable_label(anova_table) <- c(
      "Effect" = "Effect"
      , "F" = "$F$"
      , "df1" = "$\\mathit{df}_1$"
      , "df2" = "$\\mathit{df}_2$"
      , "p" = "$p$"
    )
  }
  names(es_long) <- es
  if(length(es)>0)
    variable_label(anova_table[, es]) <- es_long
  if(mse) variable_label(anova_table$MSE) <- mse_long

  ## Add 'equals' where necessary
  eq <- (1:nrow(x))[!grepl(x$p.value, pattern = "<|>|=")]
  for (i in eq) {
    x$p.value[i] <- paste0("= ", x$p.value[i])
  }

  # Concatenate character strings and return as named list
  apa_res <- apa_print_container()

  apa_res$statistic <- apply(x, 1, function(y) {
    stat <- paste0("$F(", y["df"], ", ", y["df_res"], ") = ", y["statistic"], if(mse){ paste0("$, $\\mathit{MSE} = ", y["mse"])} else {NULL}, "$, $p ", y["p.value"], "$")
    if(in_paren) stat <- in_paren(stat)
    stat
  })

  if(!is.null(es)) {
    apa_res$estimate <- apply(x, 1, function(y) {
      apa_est <- c()
      if("pes" %in% es) {
        apa_est <- c(apa_est, paste0("$\\hat{\\eta}^2_p = ", y["pes"], "$"))
      }
      if("ges" %in% es) {
        apa_est <- c(apa_est, paste0("$\\hat{\\eta}^2_G = ", y["ges"], "$"))
      }
      if("es" %in% es) {
        apa_est <- c(apa_est, paste0("$\\hat{\\eta}^2 = ", y["es"], "$"))
      }
      apa_est <- paste(apa_est, collapse = ", ")
    })

    apa_res$full_result <- paste(apa_res$statistic, apa_res$estimate, sep = ", ")

    names(apa_res$full_result) <- names(apa_res$estimate)
    apa_res <- lapply(apa_res, as.list)
  }
  apa_res$table <- sort_terms(as.data.frame(anova_table), "Effect")
  attr(apa_res$table, "class") <- c("apa_results_table", "data.frame")
  apa_res
}
