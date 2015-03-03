apa_print.htest <- function(
  x
  , stat_name = NULL
  , n = NULL
  , ci = NULL
  , in_paren = FALSE
) {
  validate(x, check_class = "htest")
  if(!is.null(stat_name)) validate(stat_name, check_class = "character", check_length = 1)
  if(!is.null(n)) validate(n, check_class = "numeric", check_integer = TRUE, check_range = c(0, Inf), check_length = 1)
  if(!is.null(ci)) validate(ci, check_class = "matrix", check_length = 2)
  validate(in_paren, check_class = "logical", check_length = 1)

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
      stat_name <- convert_stat_name(stat_name)
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
  if(!is.null(names(x$estimate))) est_name <- convert_stat_name(names(x$estimate)) else est_name <- NULL
  est_gt1 <- TRUE

  if(is.null(est_name)) {
    est <- NULL
  } else if(est_name == "\\Delta M") {
    est <- printnum(diff(x$estimate))
  } else if(length(x$estimate) == 1) {
    if(names(x$estimate) %in% c("cor", "rho", "tau")) est_gt1 <- FALSE
    est <- printnum(x$estimate, gt1 = est_gt1)
  }

  if(!is.null(est)) {
    if(!grepl("<|>", est)) eq <- " = " else eq <- ""

    if(is.null(ci) && !is.null(x$conf.int)) { # Use CI in x
      apa_stat$est <- paste0("$", est_name, eq, est, "$, ", make_confint(x$conf.int, gt1 = est_gt1))
    } else if(!is.null(ci)) { # Use supplied CI
      apa_stat$est <- paste0("$", est_name, eq, est, "$, ", make_confint(ci, gt1 = est_gt1, margin = 2))
    } else if(is.null(ci) && is.null(x$conf.int)) { # No CI
      apa_stat$est <- paste0("$", est_name, eq, est, "$")
    }

    apa_stat$full <- paste(apa_stat$est, apa_stat$stat, sep = ", ")
  }

  apa_stat
}
