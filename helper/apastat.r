apa.stat <- function(x, stat_name = NULL, n = NULL) {
  if(class(x) == "htest") {
    stat <- substring(sprintf("%3.2f", round(x$statistic, 2)), 1)
    if(is.null(stat_name)) stat_name <- names(x$statistic)
    
    if(!is.null(x$sample.size)) n <- x$sample.size
    
    if(!is.null(x$parameter)) {
      if(tolower(names(x$parameter)) == "df") {
        if(is.null(n)) stop("Please provide the sample size to report.")
        stat_name <- paste0(stat_name, "(", x$parameter, ", N = ", n, ")")
      }
    }
    p <- x$p.value
    if(round(p, 3) == 0) p <- "< .001" else p <- paste("=", substring(sprintf("%4.3f", round(p, 3)), 2))
    
    return(paste0("$", stat_name, " = ", stat, "$, $p ", p, "$"))
  } else {
    warning("No method defined for object class ", class(x), ".")
  }
}