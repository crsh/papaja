#' Format statistics (APA 6th edition)
#'
#' These methods take \code{rma} objects to create formatted character
#' strings to report the results in accordance with APA manuscript guidelines.
#'
#'
#' @return
#'    \code{apa_print.metafor} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{statistic}}{A named list of character strings giving the test statistic (z), and
#'      \emph{p} value for each term.}
#'      \item{\code{estimate}}{A named list of character strings giving the descriptive estimates and confidence intervals
#'          for each term.}
#'      \item{\code{full_result}}{A named list of character strings comprised of \code{estimate} and \code{statistic} for each term.}
#'      \item{\code{table$estimate}}{A data.frame containing the study label, estimate, standard error, test statistic for each observed study and the overall meta-analytic estimate with standard error, test statistic, and p value. This table can be passed to \code{\link{apa_table}}.}
#'    }
#'    #'      \item{\code{table$heterogeneity}}{A data.frame containing the number of observations, measure and method employed in the analysis as well as the I^2, Tau^2 Tau^2 standard error and results of Cochran test of heterogeneity. This table can be passed to \code{\link{apa_table}}.}
#'    }
#' @family apa_print
#'
apa_print.metafor <- function(
  x
  , est_name = NULL
  , ci = 0.95
  , in_paren = FALSE
  , ...
){

  if(!is(x, "rma")) stop("Object is not an rma object")
  if(!is.null(est_name)) papaja:::validate(est_name, check_class = "character", check_length = 1)
  if(is.null(est_name)) est_name <- "b"


  apa_res <- papaja:::apa_print_container()

  apa_res$estimate <- paste0("$", est_name, "$ = ", round(x$b, x$digits[["est"]]), " $95\\% CI$ = $[", round(x$ci.lb, digits = x$digits[["ci"]]), "$, $", round(x$ci.ub, digits = x$digits[["ci"]]), "]$")
  apa_res$estimate <- setNames(apa_res$estimate, row.names(x$beta))

  apa_res$statistic <- paste0("$z$ = ", round(x$zval, digits = x$digits[["test"]]), ", $p$ = ", printp(x$pval))
  apa_res$statistic <- setNames(apa_res$statistic, row.names(x$beta))

  apa_res$full_result <- paste(apa_res$estimate, apa_res$statistic)
  apa_res$full_result <- setNames(apa_res$full_result, row.names(x$beta))

  apa_res$table$estimate <- broom::tidy(x)

  apa_res$table$heterogeneity <- broom::glance(x)
  names(apa_res$table$heterogeneity) <- c("k", "measure", "method", "I$^2$", "H$^2$", "$\\tau^2$", "$\\tau^2$ se", "Cochran Q$_e$", "Cochran Q$_e$ $p$", "Cochran Q$_m$", "Cochran Q$_m$ $p$")

  apa_res
}





