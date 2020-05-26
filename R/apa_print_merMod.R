#' #' Format Statistics from (Generalized) Hierarchical Linear Models (APA 6th edition)
#' #'
#' #' These methods take objects from various R functions that calculate
#' #' hierarchical linear models to create formatted character strings
#' #' to report the results in accordance with APA manuscript guidelines.
#' #'
#' #' @param x A fitted hierarchical linear model, either from [lme4::lmer()],
#' #'   [lmerTest::lmer()], or [afex::mixed()].
#' #' @param args_confint Named list. Additional arguments that are passed to [lme4::confint.merMod()].
#' #' @inheritParams print_anova
#' #' @param ... Further arguments, currently ignored.
#' #' @family apa_print
#' #' @rdname apa_print.merMod
#' #' @method apa_print merMod
#' #' @export
#' #' @md
#'
#' apa_print.merMod <- function(
#'   x
#'   , args_confint
#'   , in_paren = FALSE, ...
#' ) {
#'
#'   args <- list(...)
#'
#'   args_confint <- defaults(
#'     if(missing(args_confint)) { list() } else { args_confint }
#'     , set = list(
#'       object = x
#'       , parm = "beta_"
#'     )
#'     , set.if.null = list(
#'       level = 0.95
#'       , method = if(methods::is(x, "glmerMod")){"boot"} else {"profile"}
#'       , nsim = 2e3
#'     )
#'   )
#'
#'   x_summary <- summary(x)
#'   confidence_intervals <- as.data.frame(
#'     do.call("confint", args_confint)
#'     , stringsAsFactors = FALSE
#'   )[rownames(x_summary$coefficients), ] # ensure same arrangement as in model object
#'
#'   print_cis <- print_confint(confidence_intervals)
#'
#'   confidence_col <- paste0(
#'     "["
#'     , printnum(confidence_intervals[[1]])
#'     , ", "
#'     , printnum(confidence_intervals[[2]])
#'     , "]"
#'   )
#'
#'   isLmerTest <- methods::is(x, "lmerModLmerTest")
#'
#'   term <- prettify_terms(
#'     rownames(x_summary$coefficients)
#'   )
#'
#'   res_table <- as.data.frame(
#'     x_summary$coefficients
#'     , row.names = NULL
#'   )
#'   rownames(res_table) <- NULL
#'
#'   renamers <- c(
#'     "Estimate" = "estimate"
#'     , "Std. Error" = "std.err"
#'     , "t value" = "statistic"
#'     , df = "df"
#'     , "Pr(>|t|)" = "p.value"
#'     , "Pr(>|z|)" = "p.value"
#'   )
#'   colnames(res_table) <- renamers[colnames(res_table)]
#'   res_table <- cbind(term, res_table)
#'
#'
#'   # prepare pretty table ----
#'   res_table$estimate <- printnum(res_table$estimate)
#'   res_table$std.err <- printnum(res_table$std.err)
#'   res_table$statistic <- printnum(res_table$statistic)
#'   res_table$conf.int <- confidence_col
#'
#'   if(is.null(res_table$conf.int)) {
#'     est_cols <- c("term", "estimate", "statistic")
#'   } else {
#'     est_cols <- c("term", "estimate", "conf.int", "statistic")
#'   }
#'
#'   if(isLmerTest) {
#'     res_table$df <- print_df(res_table$df)
#'     res_table$p.value <- printp(res_table$p.value)
#'     res_table <- res_table[, c(est_cols, "df", "p.value")]
#'   } else {
#'     res_table <- res_table[, est_cols]
#'   }
#'   # p values for glmms seem to be possible, but are calculated from z values
#'
#'   variable_labels(res_table) <- c(
#'     term = "Term"
#'     , estimate = "$b$"
#'     , std.err = "$\\mathit{SE}$"
#'     , conf.int = paste0(args_confint$level * 100, "% CI")
#'     , statistic = "$t$"
#'     , df = "$\\mathit{df}$"
#'     , p.value = "$p$"
#'   )[colnames(res_table)]
#'   class(res_table) <- c("apa_results_table", "data.frame")
#'
#'   # prepare final output container ----
#'   res <- apa_print_container()
#'   res$table <- res_table
#'
#'   res$estimate <- as.list(
#'     paste0("$b = ", res_table$estimate, "$, ", unlist(print_cis))
#'   )
#'   # if(in_paren) res$estimate <- in_paren(res$estimate) # not necessary, yet
#'
#'   if(isLmerTest) {
#'     res$statistic <- as.list(paste0("$t(", res_table$df, ") = ", res_table$statistic, "$, $p ", add_equals(res_table$p.value), "$"))
#'   } else {
#'     res$statistic <- as.list(paste0("$t = ", res_table$statistic, "$"))
#'   }
#'   if(in_paren) res$statistic <- in_paren(res$statistic)
#'
#'   res$full_result <- as.list(paste0(res$estimate, ", ", res$statistic))
#'
#'   names(res$full_result) <- names(res$statistic) <- names(res$estimate) <- sanitize_terms(res_table$term)
#'
#'   # return
#'   res
#' }
#'
#' #' @rdname apa_print.merMod
#' #' @method apa_print mixed
#' #' @export
#'
#' apa_print.mixed <- function(x, ...) {
#'
#'   anova_table <- x$anova_table
#'   attr(anova_table, "method") <- attr(x, "method")
#'   apa_print(anova_table, ...)
#' }
