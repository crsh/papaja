#' Format statistics from ANOVA (APA 6th edition)
#'
#' These methods take objects from various R functions that calculate ANOVA to create formatted character
#' strings to report the results in accordance with APA manuscript guidelines. For \code{anova}-objects from model comparisons see \code{\link{apa_print.list}}.
#'
#' @param x Output object. See details.
#' @param correction Character. In the case of repeated-measures ANOVA, the type of sphericity correction to be used (\code{GG} for Greenhouse-Geisser or \code{HF} for Huyn-Feldt methods or \code{none}). Default is \code{GG}.
#' @param intercept Logical. Indicates if intercept test should be included in output.
#' @inheritParams print_anova
#' @inheritDotParams print_anova
#' @details
#'    The factor names are sanitized to facilitate their use as list names (see Value section). Parentheses
#'    are omitted and other non-word characters are replaced by \code{_}.
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formatted string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#' @return
#'    \code{apa_print.aov} and related functions return a named list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{statistic}}{A named list of character strings giving the test statistic, parameters, and \emph{p}
#'          value for each factor.}
#'      \item{\code{estimate}}{A named list of character strings giving the effect size estimates for each factor.} % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full_result}}{A named list of character strings comprised of \code{estimate} and \code{statistic} for each factor.}
#'      \item{\code{table}}{A data.frame containing the complete ANOVA table, which can be passed to \code{\link{apa_table}}.}
#'    }
#' @references
#'    Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. \emph{Behavior Research Methods}
#'    , 37 (3), 379--384. doi:\href{https://doi.org/10.3758/BF03192707}{10.3758/BF03192707}
#'
#' @family apa_print
#' @seealso \code{\link{aov}}, \code{\link[car]{Anova}}, \code{\link{apa_print.list}}
#' @examples
#'    ## From Venables and Ripley (2002) p. 165.
#'    npk_aov <- aov(yield ~ block + N * P * K, npk)
#'    apa_print(npk_aov)
#' @export

apa_print.aov <- function(x, ...) {
  summary_x <- summary(x)

  apa_print(summary_x, ...)
}


#' @rdname apa_print.aov
#' @export

apa_print.summary.aov <- function(x, ...) {
  variance_table <- arrange_anova(x)

  print_anova(variance_table, ...)
}


#' @rdname apa_print.aov
#' @export

apa_print.aovlist <- function(x, ...) {
  summary_x <- summary(x)

  apa_print(summary_x, ...)
}


#' @rdname apa_print.aov
#' @export

apa_print.summary.aovlist <- function(x, ...) {
  variance_table <- arrange_anova(x)

  print_anova(variance_table, ...)
}


#' @rdname apa_print.aov
#' @export

apa_print.Anova.mlm <- function(x, correction = getOption("papaja.sphericity_correction"), intercept = FALSE, ...) {
  if(correction != "none") {
    summary_x <- summary(x, multivariate = FALSE) # car:::summary.Anova.mlm
  } else { # Corrections are always calculated and can throw warnings; hope I don't regret this
    summary_x <- suppressWarnings(summary(x, multivariate = FALSE)) # car:::summary.Anova.mlm
  }

  apa_print(summary_x, correction = correction, intercept = intercept, ...)
}


#' @rdname apa_print.aov
#' @export

apa_print.summary.Anova.mlm <- function(x, correction = getOption("papaja.sphericity_correction"), intercept = FALSE, ...) {
  variance_table <- arrange_anova(x, correction)

  print_anova(variance_table, intercept = intercept, ...)
}


#' @rdname apa_print.aov
#' @export

apa_print.afex_aov <- function(x, correction = getOption("papaja.sphericity_correction"), intercept = FALSE, ...) {
  validate(intercept, check_class = "logical", check_length = 1)

  afex_aov_intercept <- "(Intercept)" %in% rownames(x$anova_table)
  if(afex_aov_intercept != intercept & afex_aov_intercept) {
    warning("In your call of afex::aov_car() you requested the intercept term, but now you did not (in apa_print 'intercept = FALSE' is the default). Thus, the intercept term will be omitted; make sure this is what you want.")
  }

  if("Anova.mlm" %in% class(x$Anova)) {
    summary_x <- summary(x$Anova)
    apa_print(summary_x, correction = correction, intercept = intercept, ...)
  } else {
    apa_print(x$Anova, intercept = intercept, ...)
  }
}


#' @rdname apa_print.aov
#' @export

apa_print.anova <- function(
  x
  # , ci = 0.95
  , ...
) {
  # if(!is.null(ci)) validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  # Add method for levene test

  ellipsis <- list(...)
  variance_table <- arrange_anova(x)
  object_heading <- attr(x, "heading")

  if("apa_variance_table" %in% class(variance_table)) { # car::LeveneTest
    if(length(object_heading) == 1 && grepl("Levene", object_heading)) {
      if(!is.null(ellipsis$es)) stop("Effect sizes are not available for car::LeveneTest-objects.")
      return(print_anova(variance_table, es = NULL, mse = FALSE, ...))
    }
    return(print_anova(variance_table, ...))
  } else if("apa_model_comp" %in% class(variance_table)) {
    stop("Model comparison objects of class 'anova' are not supported. See ?apa_print.list to report model comparisons.")
  }
}
