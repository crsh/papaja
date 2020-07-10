
#' @method apa_print papaja_wsci
#' @export

apa_print.papaja_wsci <- function(x, ...) {

  summary_wsci <- summary(x)

  res <- apa_print_container()

  res$estimate <- as.list(
    paste0(
      "$M = "
      , printnum(summary_wsci$mean)
      , "$, "
      ,
        print_confint(
          x = summary_wsci[, c("lower_limit", "upper_limit")]
          , conf_level = attr(x, "Confidence level")
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
  res$table$estimate <- printnum(summary_wsci$mean)
  res$table$conf.int <- unlist(
    print_interval(
      summary_wsci[, c("lower_limit", "upper_limit"), drop = FALSE]
      , interval_type = NA # suppresses leading NA% CI
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

