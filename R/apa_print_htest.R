apa_print.htest <- function(
  x
  , stat_name = NULL
  , n = NULL
  , standardized = FALSE
  , ci = NULL
  , in_paren = FALSE
) {

  if(!is.null(ci)) validate(ci, check_class = "numeric", check_length = 2)

  if(in_paren) {
    op <- "["; cp <- "]"
  } else {
    op <- "("; cp <- ")"
  }

  if(is.null(stat_name)) stat_name <- names(x$statistic)

  stat <- printnum(x$statistic)

  if(!is.null(x$sample.size)) n <- x$sample.size

  if(!is.null(x$parameter)) {
    # Statistic and degrees of freedom
    if(tolower(names(x$parameter)) == "df") {
      if(x$parameter %%1 == 0) printdigits <- 0 else printdigits = 2
      stat_name <- convert_stat_name(names(x$statistic))
      if(stat_name == "\\Chi^2") {
        if(is.null(x$sample.size) & is.null(n)) stop("Please provide the sample size to report.") # Demand sample size information if it's a Chi^2 test
        stat_name <- paste0(stat_name, op, printnum(x$parameter[grep("df", names(x$parameter), ignore.case = TRUE)], digits = printdigits), ", n = ", n, cp)
      } else {
        stat_name <- paste0(stat_name, op, printnum(x$parameter[grep("df", names(x$parameter), ignore.case = TRUE)], digits = printdigits), cp)
      }
    }
  }

  # p-value
  p <- printp(x$p.value)
  if(!grepl("<|>", p)) eq <- "= " else eq <- ""

  apa_stat <- list()
  apa_stat$stat <- paste0("$", stat_name, " = ", stat, "$, $p ", eq, p, "$")

  # Estimate
  est_name <- convert_stat_name(names(x$estimate))
  est_gt1 <- TRUE

  if(est_name == "\\Delta M") {
    est <- printnum(diff(x$estimate))
  } else {
    if(names(x$estimate) %in% c("cor", "rho", "tau")) est_gt1 <- FALSE
    est <- printnum(x$estimate, gt1 = est_gt1)
  }
  if(!grepl("<|>", est)) eq <- " = " else eq <- ""

  if(is.null(ci) && !is.null(x$conf.int)) { # Use CI in x
    apa_stat$est <- paste0("$", est_name, eq, est, "$, ", make_confint(x$conf.int, gt1 = est_gt1))
  } else if(!is.null(ci)) { # Use supplied CI
    apa_stat$est <- paste0("$", est_name, eq, est, "$, ", make_confint(ci, gt1 = est_gt1))
  } else if(is.null(ci) && is.null(x$conf.int)) { # No CI
    apa_stat$est <- paste0("$", est_name, eq, est, "$")
  }

  apa_stat$full <- paste(apa_stat$est, apa_stat$stat, sep = ", ")

  apa_stat
}
