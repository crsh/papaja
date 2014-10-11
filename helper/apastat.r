apa.stat <- function(x, stat_name = NULL, n = NULL) {
  if(class(x) == "htest") {
    if(is.null(stat_name)) stat_name <- names(x$statistic)
    stat <- printnum(x$statistic)

    if(!is.null(x$sample.size)) n <- x$sample.size

    if(!is.null(x$parameter)) {
      if(tolower(names(x$parameter)) == "df") {
        if(length(grep("chi", stat_name, ignore.case)) != 0) {
          if(is.null(n)) stop("Please provide the sample size to report.")
          stat_name <- paste0(stat_name, "(", printnum(x$parameter$df), ", N = ", n, ")")
        } else {
          stat_name <- paste0(stat_name, "(", printnum(x$parameter$df), ")")
        }
      }
    }

    p <- printnum(x$p.value, digits = 3, gt1 = FALSE, zero = FALSE)
    apa.stat <- paste0("$", stat_name, " = ", stat, "$, $p ", p, "$"))
  } else {
    stop("No method defined for object class ", class(x), ".")
  }

  return(apa.stat)
}
