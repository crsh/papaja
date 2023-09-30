#' Create Variance Table from Various ANOVA objects
#'
#' These methods take objects from various R functions that calculate ANOVA to create
#' a \code{data.frame} containing a variance table. \emph{This function is not exported
#' and will most likely be deprecated, soon.}
#'
#' @param x Output object. See details.
#' @param correction Character. For \code{summary.Anova.mlm} objects, specifies the type of
#'    sphericity correction to be used. Either \code{GG} for Greenhouse-Geisser or \code{HF}
#'    for Huyn-Feldt methods or \code{none} is also possible. Ignored for other objects.
#' @param ... Further arguments to pass to methods.
#' @details
#'    The returned \code{data.frame} can be passed to functions such as [print_anova()].
#'
#'    Currently, methods for the following objects are available:
#'
#'    - summary.aov
#'    - summary.aovlist
#'    - Anova.mlm
#'
#' @return
#'    \code{data.frame} of class \code{apa_variance_table} or \code{apa_model_comp}.
#'
#' @keywords internal
#' @seealso [print_anova()], [print_model_comp()]
# #' @examples
# #'  \dontrun{
# #'    ## From Venables and Ripley (2002) p. 165.
# #'    npk_aov <- aov(yield ~ block + N * P * K, npk)
# #'    arrange_anova(summary(npk_aov))
# #'  }


arrange_anova <- function(x, ...) {
  UseMethod("arrange_anova")
}

arrange_anova.default <- function(x, ...) {
  stop(paste0("Objects of class '", class(x), "' are currently not supported (no method defined).
              Visit https://github.com/crsh/papaja/issues to request support for this class."))
}


#' @rdname arrange_anova
#' @method arrange_anova anova

arrange_anova.anova <- function(x, ...) {

  # .Deprecated("arrange_anova.anova() is deprecated")


  object <- as.data.frame(
    x
    , stringsAsFactors = FALSE
  )
  resid_row <- apply(X = object, MARGIN = 1, FUN = anyNA)

  # --------------------------------------------------------------------------
  # - stats::anova.lm()
  # - car::leveneTest()
  # - afex::mixed(method %in% c("KR", "S", "PB"))
  # - lme4::summary.merMod()
  # - lmerTest::summary.lmerModLmerTest()

  x <- object[!resid_row, ]

  if(any(resid_row)) {
    stopifnot(sum(resid_row) == 1)
    x$df.residual <- object$df[resid_row]
    x$sumsq_err <- object$sumsq[resid_row]
  }

  if(is.null(x$term)) { # if not model comparison
    x$Effect <- rownames(object)[!resid_row]
    rownames(x) <- NULL
  }

  class(x) <- c("apa_variance_table", class(x))
  attr(x, "df_correction") <- "none"
  x
}


#' @rdname arrange_anova
#' @method arrange_anova summary.aov

arrange_anova.summary.aov <- function(x, ...) {

  variance_table <- broom::tidy(x[[1]])
  variance_table <- as.data.frame(variance_table)

  # When processing aovlist via lapply a dummy term "aovlist_residuals" preserves the SS_error of the intercept
  # term to calculate generalized eta squared correctly later on.
  # ----
  # We now changed this to the term (Intercept) -- for the sake of consistency
  # and standardized processing later on

  # When processing an aovlist, this one-row aov-object contains the residual
  # sum of squares:
  if(nrow(variance_table) == 1 && variance_table$term == "Residuals") {
    variance_table$sumsq_err <- variance_table$sumsq
    variance_table$sumsq <- NA
    variance_table$df.residual <- variance_table$df
    variance_table$df <- NA
    variance_table$meansq <- NA
    variance_table$term <- "(Intercept)"
  } else {
    variance_table$sumsq_err <- variance_table$sumsq[nrow(variance_table)]
    variance_table$df.residual <- variance_table$df[nrow(variance_table)]
    variance_table <- variance_table[-nrow(variance_table), ]
  }

  class(variance_table) <- c("apa_variance_table", class(variance_table))
  attr(variance_table, "correction") <- "none"

  variance_table
}

arrange_anova.summary.aovlist <- function(x, ...) {
  x <- lapply(x, arrange_anova.summary.aov)
  variance_table <- do.call("rbind", x)
  rownames(variance_table) <- NULL

  attr(variance_table, "correction") <- "none"

  variance_table
}


#' @rdname arrange_anova
#' @method arrange_anova summary.Anova.mlm

arrange_anova.summary.Anova.mlm <- function(x, correction = "GG", ...) {
  validate(correction, check_class = "character", check_length = 1)

  # univariate.tests is NULL if the object comes from a MANOVA
  if(is.null(x$univariate.tests)) {
    stop(
      "Anova.mlm objects from car::Manova are not supported, yet. Visit https://github.com/crsh/papaja/issues to request support for this class. "
      , "You can try using stats::manova instead if Type I oder Type II sums of squares are adequate for your analysis."
    )
  }

  variance_table <- as.data.frame(unclass(x$univariate.tests))

  # Correct degrees of freedom
  if(nrow(x$sphericity.tests) > 0) {
    if (correction[1] == "GG") {
      variance_table[row.names(x$pval.adjustments), "num Df"] <- variance_table[row.names(x$pval.adjustments), "num Df"] * x$pval.adjustments[, "GG eps"]
      variance_table[row.names(x$pval.adjustments), "den Df"] <- variance_table[row.names(x$pval.adjustments), "den Df"] * x$pval.adjustments[, "GG eps"]
      variance_table[row.names(x$pval.adjustments), "Pr(>F)"] <- x$pval.adjustments[,"Pr(>F[GG])"]
    } else {
      if (correction[1] == "HF") {
        if (any(x$pval.adjustments[, "HF eps"] > 1)) message("HF eps > 1 treated as 1.")
        variance_table[row.names(x$pval.adjustments), "num Df"] <- variance_table[row.names(x$pval.adjustments), "num Df"] * pmin(1, x$pval.adjustments[, "HF eps"])
        variance_table[row.names(x$pval.adjustments), "den Df"] <- variance_table[row.names(x$pval.adjustments), "den Df"] * pmin(1, x$pval.adjustments[, "HF eps"])
        variance_table[row.names(x$pval.adjustments), "Pr(>F)"] <- x$pval.adjustments[,"Pr(>F[HF])"]
      } else {
        if (correction[1] == "none") {
          TRUE
        } else stop("Correction not supported. 'correction' must either be 'GG' or 'HF'.")
      }
    }
  }
  variance_table$Effect <- rownames(variance_table)

  class(variance_table) <- c("apa_variance_table", class(variance_table))
  attr(variance_table, "df_correction") <- correction

  variance_table
}
