#' Typeset Within-Subjects Confidence Intervals
#'
#' This method takes an output object from \code{\link{wsci}} and creates a
#' table and character strings to report means and within-subjects confidence
#' intervals in a table or in text.
#'
#' @param x An object of class \code{papaja_wsci}.
#' @inheritDotParams apa_num
#'
#' @evalRd apa_results_return_value()
#'
#' @method apa_print papaja_wsci
#' @export

apa_print.papaja_wsci <- function(x, ...) {

  # Note: Parameter 'in_paren' is not necessary (estimate only contains brackets, these are not changed when placed in parentheses)

  summary_wsci <- summary(x)

  res <- init_apa_results()

  res$estimate <- as.list(
    paste0(
      "$M = "
      , apa_num(summary_wsci$mean, ...)
      , "$, "
      ,
        apa_confint(
          x = summary_wsci[, c("lower_limit", "upper_limit")]
          , conf_level = attr(x, "Confidence level")
          , ...
      )
    )
  )

  factors <- setdiff(colnames(summary_wsci), c("mean", "lower_limit", "upper_limit"))

  args <- list()
  for(i in factors) {
    args[[i]] <- paste0(i, summary_wsci[[i]])
  }
  args$sep = "_"

  names(res$estimate) <- do.call("paste", args) # TODO: add names consistent with other methods (especially emm)

  # res$full_result <- res$estimate

  res$table <- summary_wsci[, factors, drop = FALSE]
  for(i in factors) {
    res$table[[i]] <- as.character(res$table[[i]], keep_label = TRUE)
  }

  res$table$estimate <- apa_num(summary_wsci$mean, ...)
  res$table$conf.int <- unlist(
    apa_interval(
      summary_wsci[, c("lower_limit", "upper_limit"), drop = FALSE]
      , interval_type = NULL # suppresses leading NA% CI
      , ...
    )
  )

  variable_labels(res$table) <- c(
    estimate = "$M$"
    , conf.int = paste0(attr(x, "Confidence level") * 100, "\\% CI")
  )

  res$table <- default_label(res$table) # add labels for factors

  class(res$table) <- c("apa_results_table", "data.frame")
  # return
  res
}

