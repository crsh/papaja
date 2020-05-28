#' Format statistics (APA 6th edition)
#'
#' Takes various \code{lsmeans} and \code{emmeans} objects methods to create formatted character strings to report the results in
#' accordance with APA manuscript guidelines.  \emph{These methods are not properly tested and should be
#' considered experimental.}
#'
#' @param x Object
#' @param test Function. Computes p-values (adjusted for multiple comparisons).
#' @param ci Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
#'    Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
#'    with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.
# #' @param contrast_names Character. A vector of names to identify calculated contrasts.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses.
#' @param ... Further arguments to pass to \code{\link{printnum}} to format the estimate.
#' @details
#'
#'    ADJUSTED CONFIDENCE INTERVALS
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#' @return \code{apa_print()} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{statistic}}{A character string giving the test statistic, parameters (e.g., degrees of freedom),
#'          and \emph{p} value.}
#'      \item{\code{estimate}}{A character string giving the descriptive estimates and confidence intervals if possible}
#'          % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full_result}}{A joint character string comprised of \code{est} and \code{stat}.}
#'      \item{\code{table}}{A data.frame containing the complete contrast table, which can be passed to \code{\link{apa_table}}.}
#'    }
#'
#' @family apa_print
#' @examples
#'    NULL
#' @method apa_print glht
#' @export

apa_print.glht <- function(x, test = multcomp::adjusted(), ...) {
  summary_x <- summary(x, test = test)

  apa_print(summary_x, ...)
}

#' @rdname apa_print.glht
#' @method apa_print summary.glht
#' @export

apa_print.summary.glht <- function(
  x
  , ci = 0.95
  , in_paren = FALSE
  , ...
) {
  validate(x, check_class = "summary.glht")
  validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(in_paren, check_class = "logical", check_length = 1)

  tidy_x <- broom::tidy(x)
  test_stat <- ifelse(x$df == 0, "z", paste0("t(", x$df, ")"))
  conf_level <- paste0(ci * 100, "\\% CI")

  # Assamble table
  ## Add (adjusted) confidence intervall
  multcomp_adjustment <- if(x$test$type == "none") multcomp::univariate_calpha() else multcomp::adjusted_calpha()
  print_ci <- stats::confint(x, level = ci, calpha = multcomp_adjustment)$confint
  dimnames(print_ci) <- NULL
  table_ci <- unlist(print_confint(print_ci[, -1], ...)) # Remove point estimate from matrix
  contrast_table <- cbind(estimate = tidy_x$estimate, confint = table_ci, tidy_x[, c("statistic", "p.value")])
  rownames(contrast_table) <- sanitize_terms(tidy_x$lhs)

  ## Format numbers
  contrast_table$estimate <- printnum(contrast_table$estimate, ...)
  contrast_table$statistic <- printnum(contrast_table$statistic, digits = 2)
  contrast_table$p.value <- printp(contrast_table$p.value)

  # Concatenate character strings and return as named list
  apa_res <- apa_print_container()

  apa_res$estimate <- apply(contrast_table, 1, function(y) {
    paste0("$\\Delta M = ", y["estimate"], "$, ", conf_level, " ", y["confint"])
  })

  apa_res$statistic <- apply(contrast_table, 1, function(y) {
    paste0("$", test_stat, " = ", y["statistic"], "$, $p ", add_equals(y["p.value"]), "$")
  })

  apa_res$full_result <- paste(apa_res$estimate, apa_res$stat, sep = ", ")
  names(apa_res$full_result) <- names(apa_res$estimate)

  apa_res <- lapply(apa_res, as.list)

  # Add table
  rownames(contrast_table) <- tidy_x$lhs
  colnames(contrast_table) <- c("estimate", "ci", "statistic", "p.value")
  variable_label(contrast_table) <- c(
    estimate = "$\\Delta M$"
    , ci = conf_level
    , statistic = paste0("$", test_stat, "$")
    , p.value = "$p$"
  )
  apa_res$table <- contrast_table
  attr(apa_res$table, "class") <- c("apa_results_table", "data.frame")
  apa_res
}

#' @rdname apa_print.glht
#' @export

# apa_print.emmGrid <- function(x, ...) {
#   ellipsis <- list(...)
#   if(is.null(ellipsis$infer)) ellipsis$infer <- TRUE
#   ellipsis$object <- x
#
#   summary_x <- do.call("summary", ellipsis)
#   apa_print(summary_x, ...)
# }

#' @rdname apa_print.glht
#' @export

# apa_print.lsmobj <- function(x, ...) {
#   apa_print.emmGrid(x, ...)
# }


#' @rdname apa_print.glht
#' @export

# apa_print.summary_emm <- function(
#   x
#   , contrast_names = NULL
#   , in_paren = FALSE
#   , ...
# ) {
#   if(class(x)[1] != "summary.ref.grid") validate(x, check_class = "summary_emm", check_NA = FALSE)
#   validate(in_paren, check_class = "logical", check_length = 1)
#   if(!is.null(contrast_names)) validate(contrast_names, check_class = "character")
#
#   ci <- get_lsm_conf_level(x)
#   ci_supplied <- !length(ci) == 0
#   p_supplied <- "p.value" %in% colnames(x)
#   if(!ci_supplied & !p_supplied) stop("Object 'x' includes neither confidence intervals nor test statistics (i.e., p-values). See '?lsmeans::summary' for details.")
#
#   if(!ci_supplied) {
#     warning("Object 'x' does not include confidence intervals. APA guidelines recommend to routinely report confidence intervals for all estimates. See '?lsmeans::summary' on how to add confidence intervals to your results.")
#
#     ci_colnames <- NULL
#     conf_level <- NULL
#   } else {
#     ci_colnames <- c("ll", "ul")
#     if(ci < 1) ci <- ci * 100
#     conf_level <- paste0(ci, "\\% CI")
#   }
#
#   if(!p_supplied) {
#     warning("Object 'x' does not include test statistics (i.e., p-values). See '?lsmeans::summary' on how to add test statistics to your results.")
#
#     df_colname <- NULL
#     stat_colnames <- NULL
#   } else {
#     dfdigits <- as.numeric(x$df %%1 > 0) * 2
#     contrast_df <- unique(x$df)
#     df_colname <- "df"
#     stat_colnames <- c("statistic", "p.value")
#   }
#
#   split_by <- attr(x, "by.vars")
#   x <- data.frame(x)
#   if("null" %in% colnames(x)) x <- x[, -grep("null", colnames(x))]
#   colnames(x) <- c("contrast", split_by, "estimate", "std.error", df_colname, ci_colnames, stat_colnames)
#
#   # Assamble table
#   apa_res <- apa_print_container()
#
#   if(!is.null(split_by)) {
#     contrast_list <- split(x, x[, split_by])
#     contrast_list <- lapply(contrast_list, function(x) x[, -which(colnames(x) == split_by)])
#     prep_table <- merge_tables(
#       contrast_list
#       , row_names = rep(FALSE, length(contrast_list))
#       , added_stub_head = split_by
#     )
#     contrast_table <- do.call(rbind, prep_table)
#   } else {
#     contrast_table <- x
#   }
#
#   ## Add confindence interval
#   if(ci_supplied) {
#     ci_table <- data.frame(confint = unlist(print_confint(matrix(c(contrast_table$ll, contrast_table$ul), ncol = 2), margin = 2, conf_level = NULL, ...)))
#     contrast_table <- contrast_table[, -grep("std\\.error|ll|ul", colnames(contrast_table))]
#
#     contrast_table <- cbind(
#       contrast_table[, 1:which(colnames(contrast_table) == "estimate")]
#       , ci_table
#       , contrast_table[, c(df_colname, stat_colnames)] # Will be NULL if not supplied
#     )
#   } else {
#     contrast_table <- contrast_table[, -grep("std\\.error", colnames(contrast_table))]
#   }
#
#
#   ## Add contrast names
#   rownames(contrast_table) <- if(!is.null(contrast_names)) contrast_names else contrast_table$contrast
#   contrast_table <- contrast_table[, which(colnames(contrast_table) != "contrast")]
#
#   contrast_table$estimate <- printnum(contrast_table$estimate, ...)
#   if(p_supplied) {
#     contrast_table$statistic <- printnum(contrast_table$statistic)
#     contrast_table$df <- printnum(contrast_table$df, digits = dfdigits)
#     contrast_table$p.value <- printp(contrast_table$p.value)
#   }
#
#   # Concatenate character strings and return as named list
#   if(ci_supplied) {
#     apa_res$estimate <- apply(contrast_table, 1, function(y) {
#       paste0("$\\Delta M = ", y["estimate"], "$, ", conf_level, " ", y["confint"])
#     })
#   } else {
#     apa_res$estimate <- apply(contrast_table, 1, function(y) {
#       paste0("$\\Delta M = ", y["estimate"], "$")
#     })
#   }
#
#   if(p_supplied) {
#     apa_res$statistic <- apply(contrast_table, 1, function(y) {
#       if(!grepl("<|>", y["p.value"])) eq <- "= " else eq <- ""
#
#       paste0("$t(", y["df"], ") = ", y["statistic"], "$, $p ", eq, y["p.value"], "$")
#     })
#
#     apa_res$full_result <- paste(apa_res$est, apa_res$stat, sep = ", ")
#     names(apa_res$full_result) <- names(apa_res$est)
#   }
#
#   apa_res <- lapply(apa_res, as.list)
#
#   # Add table
#   if(p_supplied) {
#     if(length(contrast_df) == 1) { # Remove df column and put df in column heading
#       df <- contrast_table$df[1]
#       contrast_table <- contrast_table[, which(colnames(contrast_table) != "df")]
#       colnames(contrast_table) <- c("estimate", "ci", "statistic", "p.value")
#       contrast_table$ci <- as.character(contrast_table$ci)
#       variable_label(contrast_table) <- c(
#         estimate = "$\\Delta M$"
#         , ci = conf_level
#         , statistic = paste0("$t(", df, ")$")
#         , p.value = "$p$"
#       )
#     } else {
#       colnames(contrast_table) <- c("estimate", "ci", "statistic", "df", "p.value")
#       contrast_table$ci <- as.character(contrast_table$ci)
#       variable_label(contrast_table) <- c(
#         split_by = split_by
#         , estimate = "$\\Delta M$"
#         , ci = conf_level
#         , estimate = "$t$"
#         , df = "$df$"
#         , p.value = "$p$"
#       )
#     }
#   } else {
#     colnames(contrast_table) <- c("estimate", "ci")
#     contrast_table$ci <- as.character(contrast_table$ci)
#     variable_label(contrast_table) <- c(
#       split_by = split_by
#       , estimate = "$\\Delta M$"
#       , ci = conf_level
#     )
#   }
#
#   apa_res$table <- contrast_table
#   attr(apa_res$table, "class") <- c("apa_results_table", "data.frame")
#
#   apa_res
# }

#' @rdname apa_print.glht
#' @method apa_print summary.ref.grid
#' @export

# apa_print.summary.ref.grid <- function(x, ...) {
#   validate(x, check_class = "summary.ref.grid", check_NA = FALSE)
#   apa_print.summary_emm(x, ...)
# }
#
#
# get_lsm_conf_level <- function(x) {
#   lsm_messages <- attr(x, "mesg")
#   conf_level_message <- lsm_messages[grepl("Confidence level", lsm_messages)]
#   as.numeric(stringr::str_extract(conf_level_message, "0\\.\\d+"))
# }
