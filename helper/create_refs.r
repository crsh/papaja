create_package_refs <- function(file = NULL, prefix = "R-", ...) {
  require("knitr")
  package_list <- c("base", names(sessionInfo()$otherPkgs))
  if(file.exists(file)) {
    bib_file <- readLines(file)
    missing_packages <- sapply(package_list, function(x) !any(grep(paste0(prefix, x), bib_file)))
    missing_packages <- names(missing_packages[missing_packages])
  } else {
    missing_packages <- package_list
  }
  if(is.null(file)) knitr::write_bib(missing_packages, prefix = prefix, ...) else {
    sink(file, append = TRUE)
    knitr::write_bib(missing_packages, prefix = prefix, ...)
    sink()
  } 
}