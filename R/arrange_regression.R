#' Create a Regression Table
#'
#' These methods take \code{glm} or \code{lm} objects to create
#' a data frame containing a regression table. \emph{This function is not exported.}
#'
#' @param x \code{glm} or \code{lm} object. See details.
#' @inheritParams apa_print.glm
#'
#' @return
#'    \code{data.frame} of class \code{apa_regression_table}.
#'
#' @keywords internal

arrange_regression <- function(x, est_name, standardized, conf.int, ...) {
  ellipsis <- list(...)
  summary_x <- summary(x)

  # Obtain the name of the test statistic
  if(class(x)[1] == "glm") {
    stat_name <- gsub(
      colnames(summary_x$coefficients)[3]
      , pattern = " value"
      , replacement = ""
    )
  } else if(class(x) == "lm") {
    stat_name <- "t"
    if(is.null(est_name)) if(standardized) est_name <- "b^*" else est_name <- "b"
    if(standardized) ellipsis$gt1 <- FALSE
  }

  if(is.matrix(conf.int)) {
    conf_level <- as.numeric(gsub("[^.|\\d]", "", colnames(conf.int), perl = TRUE))
    conf_level <- 100 - conf_level[1] * 2
  } else {
    conf_level <- 100 * conf.int
  }

  tidy_x <- broom::tidy(x)
  tidy_x <- cbind(tidy_x, conf.int) # Also adds term rownames
  rownames(tidy_x) <- sanitize_terms(tidy_x$term, standardized = standardized)

  ## Assemble regression table
  regression_table <- data.frame(tidy_x[, c("term", "estimate", "statistic", "p.value")], row.names = NULL)
  regression_table$conf.int <- apply(
    tidy_x[, utils::tail(names(tidy_x), 2)]
    , 1
    , function(y) do.call(function(...) print_interval(x = y[utils::tail(names(y), 2)], ...), ellipsis) # Don't add "x% CI" to each line
  )
  regression_table <- regression_table[, c("term", "estimate", "conf.int", "statistic", "p.value")] # Change order of columns
  regression_table$term <- beautify_terms(regression_table$term, standardized = standardized)

  regression_table$estimate <- do.call(function(...) printnum(regression_table$estimate, ...), ellipsis)
  regression_table$statistic <- printnum(regression_table$statistic, digits = 2)
  regression_table$p.value <- printp(regression_table$p.value)

  colnames(regression_table) <- c("term", "estimate", "conf.int", "statistic", "p.value")

  if(stat_name == "z") {
    test_statistic <- paste0("$", stat_name, "$")
  } else {
    test_statistic <- paste0("$", stat_name, "(", x$df.residual, ")$")
  }

  variable_label(regression_table) <- c(
    term        = "Predictor"
    , estimate  = paste0("$", est_name, "$")
    , conf.int  = paste0(conf_level, "\\% CI")
    , statistic = test_statistic
    , p.value   = "$p$"
  )

  class(regression_table) <- c("apa_regression_table", class(regression_table))

  if(class(x)[1] == "glm") {
    attr(regression_table, "family") <- x$family$family
    attr(regression_table, "link") <- x$family$link
  } else if(class(x) == "lm") {
    attr(regression_table, "family") <- "gaussian"
    attr(regression_table, "link") <- "identity"
  }

  regression_table
}
