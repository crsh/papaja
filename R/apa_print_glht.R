#' Format statistics (APA 6th edition)
#'
#' Takes \code{htest} objects from various statistical methods to create
#' formated chraracter strings to report the results in accordance with APA manuscript guidelines.
#'
#' @param x See details.
#' @param test Function.
#' @param ci Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
#'    Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
#'    with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.
#' @param ... Further arguments to pass to \code{\link{printnum}} to format the estimate.
#' @details The function should work on a wide range of \code{htest} objects. Due to the large number of functions
#'    that produce these objects and their idiosyncracies, the produced strings may sometimes be inaccurate. If you
#'    experience inaccuracies you may report these \href{https://github.com/crsh/papaja/issues}{here} (please include
#'    a reproducible example in your report!).
#'    
#'    ADJUSTED CONFIDENCE INTERVALS
#'
#'    \code{stat_name} and \code{est_name} are placed in the output string and are thus passed to pandoc or LaTeX through
#'    \pkg{kntir}. Thus, to the extent it is supported by the final document type, you can pass LaTeX-markup to format the
#'    final text (e.g., \code{\\\\tau} yields \eqn{\tau}).
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#' @return \code{apa_print()} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{stat}}{A character string giving the test statistic, parameters (e.g., degrees of freedom),
#'          and \emph{p} value.}
#'      \item{\code{est}}{A character string giving the descriptive estimates and confidence intervals if possible}
#'          % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full}}{A joint character string comprised of \code{est} and \code{stat}.}
#'    }
#'
#' @family apa_print
#' @examples
#'    NULL
#' @export

apa_print.glht <- function(x, test = adjusted(), ...) {
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
  
  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }
  
  tidy_x <- broom::tidy(x)
  test_stat <- ifelse(x$df == 0, "z", paste0("t(", x$df, ")"))
  conf_level <- paste0(ci * 100, "\\% CI")
  
  # Assamble table
  ## Add (adjusted) confidence intervall
  print_ci <- confint(x, level = ci)$confint
  dimnames(print_ci) <- NULL
  table_ci <- unlist(print_confint(print_ci[, -1], ...)) # Remove point estimate from matrix
  contrast_table <- cbind(estimate = tidy_x$estimate, confint = table_ci, tidy_x[, c("statistic", "p.value")])
  rownames(contrast_table) <- sanitize_terms(tidy_x$lhs)
  
  ## Format numbers
  contrast_table$estimate <- printnum(contrast_table$estimate, ...)
  contrast_table$statistic <- printnum(contrast_table$statistic, digits = 2)
  contrast_table$p.value <- printp(contrast_table$p.value)
  
  # Concatenate character strings and return as named list
  apa_res <- list()
  
  apa_res$est <- apply(contrast_table, 1, function(y) {
    paste0("$\\Delta M = ", y["estimate"], "$, ", conf_level, " ", y["confint"])
  })
  
  apa_res$stat <- apply(contrast_table, 1, function(y) {
    if(!grepl("<|>", y["p.value"])) eq <- "= " else eq <- ""
    
    paste0("$", test_stat, " = ", y["statistic"], "$, $p ", eq, y["p.value"], "$")
  })
  
  apa_res$full <- paste(apa_res$est, apa_res$stat, sep = ", ")
  names(apa_res$full) <- names(apa_res$est)
  
  apa_res <- lapply(apa_res, as.list)
  
  # Add table
  rownames(contrast_table) <- tidy_x$lhs
  colnames(contrast_table) <- c("$\\Delta M$", conf_level, paste0("$", test_stat, "$"), "$p$")
  apa_res$table <- contrast_table
  
  apa_res
}

#' @rdname apa_print.glht
#' @method apa_print lsmobj
#' @export

apa_print.lsmobj <- function(x, ...) {
  summary_x <- summary(x, ...)

  apa_print(summary_x, ...)
}

#' @rdname apa_print.glht
#' @method apa_print summary.ref.grid
#' @export

apa_print.summary.ref.grid <- function(
  x
  , ci = 0.95
  , in_paren = FALSE
  , contrast_names = NULL
  , ...
) {
  validate(x, check_class = "summary.ref.grid")
  validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(in_paren, check_class = "logical", check_length = 1)
  if(!is.null(contrast_names)) validate(contrast_names, check_class = "character")
  
  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }
  
  split_by <- attr(x, "by.vars")
  x <- data.frame(x)
  colnames(x) <- c("contrast", "estimate", "std.error", "df", "statistic", "p.value")
  
  # Assamble table
  if(!is.null(split_by)) {
    contrast_list <- split(x, x[, split_by])
    contrast_list <- lapply(contrast_list, function(x) x[, -which(colnames(x) == split_by)])
    prep_table <- merge_tables(
      contrast_list
      , row_names = rep(FALSE, length(contrast_list))
      , added_colnames = split_by
    )
    contrast_table <- do.call(rbind, prep_table)
  } else {
    contrast_table <- x
  }
  
  contrast_df <- unique(round(contrast_table$df, 2))

  ## Calculate confidence intervals (can't use confint() because it's a summary object)
  norm_quant <- 1 - (1 - ci) / 2
  x$ll <- x$estimate - x$std.error * qnorm(norm_quant)
  x$ul <- x$estimate + x$std.error * qnorm(norm_quant)
  
  if(ci < 1) conf_level <- ci * 100
  conf_level <- paste0(conf_level, "\\% CI")
  
  ## Add confindence interval
  table_ci <- unlist(print_confint(matrix(c(x$ll, x$ul), ncol = 2), margin = 2, conf_level = NULL, ...))
  contrast_table <- cbind(
    contrast_table[, 1:which(colnames(contrast_table) == "estimate")]
    , data.frame(confint = table_ci)
    , contrast_table[, which(colnames(contrast_table) == "df"):ncol(contrast_table)]
  )
  rownames(contrast_table) <- if(!is.null(contrast_names)) contrast_names else contrast_table$contrast
  
  contrast_table <- subset(contrast_table, select = -contrast)

  contrast_table$estimate <- printnum(contrast_table$estimate, ...)
  contrast_table$p.value <- printp(contrast_table$p.value)
  contrast_table[, c("df", "statistic")] <- printnum(contrast_table[, c("df", "statistic")], margin = 2, digits = 2)

  # Concatenate character strings and return as named list
  apa_res <- list()
  
  apa_res$est <- apply(contrast_table, 1, function(y) {
    paste0("$\\Delta M = ", y["estimate"], "$, ", conf_level, " ", y["confint"])
  })
  
  apa_res$stat <- apply(contrast_table, 1, function(y) {
    if(!grepl("<|>", y["p.value"])) eq <- "= " else eq <- ""
    
    paste0("$", test_stat, " = ", y["statistic"], "$, $p ", eq, y["p.value"], "$")
  })
  
  apa_res$full <- paste(apa_res$est, apa_res$stat, sep = ", ")
  names(apa_res$full) <- names(apa_res$est)
  
  apa_res <- lapply(apa_res, as.list)
  
  
  # Add table
  if(length(contrast_df) == 1) { # Remove df column and put df in column heading
    contrast_table <- subset(contrast_table, select = -df)
    colnames(contrast_table) <- c("$\\Delta M$", conf_level, paste0("$t(", contrast_df, ")$"), "$p$")
  } else {
    colnames(contrast_table) <- c(split_by, "$\\Delta M$", conf_level, "$t$", "$df$", "$p$")
  }
  
  apa_res$table <- contrast_table

  apa_res
}
