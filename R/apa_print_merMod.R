
#' @export

apa_print.merMod <- function(x, ...) {

  # if(package_available("lmerTest")) {
  #   x <- lmerTest::as_lmerModLmerTest(x)
  # }
  args <- list(...)

  args_confint <- defaults(
    args$args_confint
    , set = list(
      object = x
      , parm = "beta_"
    )
    , set.if.null = list(
      level = .95
    )
  )

  x_summary <- summary(x)
  confidence_intervals <- as.data.frame(
    do.call("confint", args_confint)
    , stringsAsFactors = FALSE
  )[rownames(x_summary$coefficients), ] # ensure same arrangement as in model object

  print_cis <- print_confint(confidence_intervals)

  confidence_col <- paste0(
    "["
    , printnum(confidence_intervals[[1]])
    , ", "
    , printnum(confidence_intervals[[2]])
    , "]"
  )

  isLmerTest <- methods::is(x, "lmerModLmerTest")

  term <- prettify_terms(
    rownames(x_summary$coefficients)
  )

  res_table <- as.data.frame(
    x_summary$coefficients
    , row.names = NULL
  )
  rownames(res_table) <- NULL

  renamers <- c(
    "Estimate" = "estimate"
    , "Std. Error" = "std.err"
    , "t value" = "statistic"
    , df = "df"
    , "Pr(>|t|)" = "p.value"
  )
  colnames(res_table) <- renamers[colnames(res_table)]
  res_table <- cbind(term, res_table)


  # prepare pretty table ----
  res_table$estimate <- printnum(res_table$estimate)
  res_table$std.err <- printnum(res_table$std.err)
  res_table$statistic <- printnum(res_table$statistic)
  res_table$conf.int <- confidence_col

  if(isLmerTest) {
    res_table$df <- print_df(res_table$df)
    res_table$p.value <- printp(res_table$p.value)
    res_table <- res_table[, c("term", "estimate", "conf.int", "statistic", "df", "p.value")]
  } else {
    res_table <- res_table[, c("term", "estimate", "conf.int", "statistic")]
  }

  variable_labels(res_table) <- c(
    term = "Term"
    , estimate = "$b$"
    , std.err = "$\\mathit{SE}$"
    , conf.int = paste0(args_confint$level * 100, "% CI")
    , statistic = "$t$"
    , df = "$\\mathit{df}$"
    , p.value = "$p$"
  )[colnames(res_table)]
  class(res_table) <- c("apa_results_table", "data.frame")

  # prepare final output container ----
  res <- apa_print_container()
  res$table <- res_table

  res$estimate <- as.list(
    paste0("$b = ", res_table$estimate, "$, ", unlist(print_cis))
  )
  names(res$estimate) <- sanitize_terms(res_table$term)

  if(isLmerTest) {
    res$statistic <- as.list(paste0("$t(", res_table$df, ") = ", res_table$statistic, "$, $p ", add_equals(res_table$p.value), "$"))
  } else {
    res$statistic <- as.list(paste0("$t = ", res_table$statistic, "$"))
  }
  names(res$statistic) <- names(res$estimate)

  res$full_result <- as.list(paste0(res$estimate, ", ", res$statistic))
  names(res$full_result) <- names(res$estimate)

  # return
  res
}

