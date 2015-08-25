#' Create variance table from various ANOVA objects
#'
#' These methods take objects from various R functions that calculate ANOVA to create
#' a \code{data.frame} containing a variance table. \emph{This function is not exported.}
#'
#' @param x Output object. See details.
#' @param correction Character. For \code{summary.Anova.mlm} objects, specifies the type of
#'    sphericity correction to be used. Either \code{GG} for Greenhouse-Geisser or \code{HF}
#'    for Huyn-Feldt methods or \code{none} is also possible. Ignored for other objects.
#' @details
#'    The returned \code{data.frame} can be passed to functions such as \code{\link{print_anova}}.
#'
#'    Currently, methods for the following objects are available:
#'    \itemize{
#'      \item{\code{summary.aov}}
#'      \item{\code{summary.aovlist}}
#'      \item{\code{Anova.mlm}}
#'    }
#'
#' @return
#'    \code{data.frame} of class \code{apa_variance_table} or \code{apa_model_comp}.
#'
#' @seealso \code{\link{print_anova}}, \code{\link{print_model_comp}}
#' @examples
#'    ## From Venables and Ripley (2002) p. 165.
#'    npk_aov <- aov(yield ~ block + N * P * K, npk)
#'    arrange_anova(summary(npk_aov))

arrange_anova <- function(x, ...) UseMethod("arrange_anova", x)


#' @rdname arrange_anova
#' @method arrange_anova anova

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

    class(x) <- c("apa_model_comp", class(x))

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

    class(x) <- c("apa_variance_table", class(x))
    attr(x, "correction") <- "none"
  }

  x
}


#' @rdname arrange_anova
#' @method arrange_anova summary.aov

arrange_anova.summary.aov <- function(x) {
  variance_table <- broom::tidy(x[[1]])
  variance_table$sumsq_err <- variance_table[nrow(variance_table), "sumsq"]
  variance_table$df_res <- variance_table[nrow(variance_table), "df"]
  variance_table <- variance_table[-nrow(variance_table), ]

  class(variance_table) <- c("apa_variance_table", class(variance_table))
  attr(variance_table, "correction") <- "none"

  variance_table
}

arrange_anova.summary.aovlist <- function(x) {
  x <- lapply(x, arrange_anova.summary.aov)
  variance_table <- do.call("rbind", x)
  variance_table <- data.frame(variance_table, row.names = NULL)

  class(variance_table) <- c("apa_variance_table", class(variance_table))
  attr(variance_table, "correction") <- "none"

  variance_table
}


#' @rdname arrange_anova
#' @method arrange_anova summary.Anova.mlm

arrange_anova.summary.Anova.mlm <- function(x, correction = "GG") {
  tmp <- x$univariate.tests
  class(tmp) <- NULL
  variance_table <- data.frame(tmp)
  colnames(variance_table) <- colnames(tmp)

  # Correct degrees of freedom
  if(nrow(x$sphericity.tests) > 0) {
    if (correction[1] == "GG") {
      variance_table[row.names(x$pval.adjustments), "num Df"] <- variance_table[row.names(x$pval.adjustments), "num Df"] * x$pval.adjustments[, "GG eps"]
      variance_table[row.names(x$pval.adjustments), "den Df"] <- variance_table[row.names(x$pval.adjustments), "den Df"] * x$pval.adjustments[, "GG eps"]
      variance_table[row.names(x$pval.adjustments), "Pr(>F)"] <- x$pval.adjustments[,"Pr(>F[GG])"]
    } else {
      if (correction[1] == "HF") {
        if (any(x$pval.adjustments[,"HF eps"] > 1)) warning("HF eps > 1 treated as 1")
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

  variance_table <- as.data.frame(variance_table)

  # Obtain positons of statistics in data.frame
  old <- c("SS", "num Df", "Error SS", "den Df", "F", "Pr(>F)")
  nu <- c("sumsq", "df", "sumsq_err", "df_res", "statistic", "p.value")
  colnames(variance_table) == old
  for (i in 1:length(old)){
    colnames(variance_table)[colnames(variance_table) == old[i]] <- nu[i]
  }

  variance_table$term <- rownames(variance_table)
  variance_table <- data.frame(variance_table, row.names = NULL)

  class(variance_table) <- c("apa_variance_table", class(variance_table))
  attr(variance_table, "correction") <- correction

  variance_table
}
