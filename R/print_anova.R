#' Format statistics from ANOVA (APA 6th edition)
#'
#' This function is the former internal workhorse of the \code{apa_print}-family for ANOVA. It takes a \code{data.frame}
#' of class \code{apa_variance_table} and produces strings to report the results in accordance with APA manuscript
#' guidelines. It is already deprecated and will soon be defunct. \emph{This function is not exported.}
#'
#' @param x Data.frame. A \code{data.frame} of class \code{apa_variance_table} as returned by \code{\link{arrange_anova}}.
#' @param intercept Logical. Indicates if intercept test should be included in output.
#' @param es Character. The effect-size measure to be calculated; can be either \code{ges} for generalized eta-squared, \code{pes} for partial eta-squared or \code{es} for eta-squared.
#'   Note that eta-squared is calculated correctly if and only if the design is balanced.
#' @param mse Logical. Indicates if mean squared errors should be included in output. Default is \code{TRUE}.
#' @param observed Character. The names of the factors that are observed, (i.e., not manipulated). Necessary for calculation of generalized eta-squared; otherwise ignored.
#' @param in_paren Logical. Indicates if the formatted string will be reported inside parentheses. See details.

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
#' @seealso [arrange_anova()], [apa_print.aov()]
# #' @examples
# #'  \dontrun{
# #'    ## From Venables and Ripley (2002) p. 165.
# #'    npk_aov <- aov(yield ~ block + N * P * K, npk)
# #'    anova_table <- arrange_anova(summary(npk_aov))
# #'    print_anova(anova_table)
# #'  }


print_anova <- function(
  x
  , intercept = FALSE
  , observed = NULL
  , es = "ges"
  , mse = getOption("papaja.mse")
  , in_paren = FALSE
) {

  .Deprecated("print_anova() is deprecated")

  # When processing aovlist objects, the `(Intercept)` is kept to preserve the
  # SS_error of the intercept   # term to calculate generalized eta squared
  # correctly. This term contains NAs.
  validate(x, check_class = "data.frame", check_NA = FALSE)
  validate(x, check_class = "apa_variance_table", check_NA = FALSE)
  validate(intercept, check_class = "logical", check_length = 1)

  if(!is.null(observed)) validate(observed, check_class = "character")
  if(!is.null(es)) {
    validate(es, check_class = "character")
    if(length(es) > 1L) warning("Calculating more than one effect-size measure is now deprecated.")
    es <- es[[1L]]
  }
  validate(in_paren, check_class = "logical", check_length = 1)

  rownames(x) <- sanitize_terms(x$term)
  # ----------------------------------------------------------------------------
  # Add effect-size measures and mean-squared errors
  x <- add_effect_sizes(
    x = x
    , es = es
    , observed = observed
    # , mse = mse # this is how we used to add MSE before adding add_mse() as a separate function
    , intercept = intercept
  )
  if(isTRUE(mse)) x <- add_mse(x)

  # Remove intercept if the user doesn't want it:
  if(!intercept) x <- x[x$term != "(Intercept)", ]

  # Rounding and filling with zeros
  x$statistic <- apa_num(x$statistic, digits = 2)
  x$p.value <- apa_p(x$p.value)
  x$df <- apa_df(x$df)
  x$df.residual <- apa_df(x$df.residual)
  for(i in es) {x[[i]] <- apa_num(x[[i]], digits = 3, gt1 = FALSE)}
  if(mse) x$mse <- apa_num(x$mse, digits = 2)

  # Assemble table -------------------------------------------------------------
  cols <- intersect(
    c("term", es, "statistic","df", "df.residual", "mse", "p.value")
    , colnames(x)
  )
  anova_table <- data.frame(x[, cols], row.names = NULL)
  anova_table[["term"]] <- beautify_terms(anova_table[["term"]])

  correction_type <- attr(x, "correction")

  statistic <- attr(x, "statistic")
  if(is.null(statistic)) statistic <- "statistic"
  stat_label <- "$F$"
  if(is.null(anova_table$df.residual)) stat_label <- "$\\Chi^2$"

  names(es) <- es
  renamers <- c(
    term = "term"
    , statistic = "statistic"
    , df = "df"
    , df.residual = "df.residual"
    , mse = "mse"
    , p.value = "p.value"
    , es
  )

  colnames(anova_table) <- renamers[colnames(anova_table)]
  class(anova_table) <- "data.frame"

  if(!is.null(correction_type) && correction_type != "none") {
    variable_label(anova_table) <- c(
      "term"          = "Effect"
      , "statistic"   = stat_label
      , "df"          = paste0("$\\mathit{df}^{\\mathrm{", correction_type, "}}$")
      , "df.residual" = paste0("$\\mathit{df}_{\\mathrm{res}}^{\\mathrm{", correction_type, "}}$")
      , "p.value"     = "$p$"
      , "pes"         = "$\\hat{\\eta}^2_p$"
      , "ges"         = "$\\hat{\\eta}^2_G$"
      , "es"          = "$\\hat{\\eta}^2$"
      , "mse"         = "$\\mathit{MSE}$"
    )[colnames(anova_table)]
  } else {
    variable_label(anova_table) <- c(
      "term"          = "Effect"
      , "statistic"   = stat_label
      , "df"          = "$\\mathit{df}$"
      , "df.residual" = "$\\mathit{df}_{\\mathrm{res}}$"
      , "p.value"     = "$p$"
      , "pes"         = "$\\hat{\\eta}^2_p$"
      , "ges"         = "$\\hat{\\eta}^2_G$"
      , "es"          = "$\\hat{\\eta}^2$"
      , "mse"         = "$\\mathit{MSE}$"
    )[colnames(anova_table)]
  }

  # fix: rename effect-size column and
  if(!is.null(es)) {
    colnames(anova_table)[colnames(anova_table) == es] <- "estimate"
  }

  # Assemble term lists --------------------------------------------------------

  # Concatenate character strings and return as named list
  apa_res <- init_apa_results()

  apa_res$statistic <- paste0(
    "$"
    , strip_math_tags(stat_label)
    , if(!is.null(x$df)) { "(" } else { NULL }
    , x$df
    , if(!is.null(x$df.residual)) { ", " } else { NULL }
    , x$df.residual
    , if(!is.null(x$df)) { ")" } else { NULL }
    , " = "
    , x$statistic
    , if(mse) { "$, $\\mathit{MSE} = " } else { NULL }
    , if(mse) { x$mse } else { NULL }
    , "$, $p "
    , add_equals(x$p.value)
    , "$"
  )

  if(in_paren) apa_res$statistic <- in_paren(apa_res$statistic)
  names(apa_res$statistic) <- rownames(x)
  apa_res$statistic <- as.vector(apa_res$statistic, mode = "list")

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
    apa_res[] <- lapply(apa_res, as.list) # [] for preserving class
  }
  apa_res$table <- sort_terms(as.data.frame(anova_table), "term")
  class(apa_res$table) <- c("apa_results_table", "data.frame")
  apa_res
}
