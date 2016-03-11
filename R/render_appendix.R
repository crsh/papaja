#' Render appendix
#'
#' This functions renders an R Markdown to TeX fragment without preamble so it can be included as appendix.
#'
#' @param x Character. Input file name.
#' @details
#' Because pandoc currently does not support \code{includes} \code{after_body} this function has no effect
#' when rendering word documents.
#' @export

render_appendix <- function(x) {
  new_name <- NULL
  target_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  if(length(target_format) > 0 && target_format == "latex") {
    # Create TeX-file
    tmp_name <- paste0(paste(sample(c(letters, LETTERS, 0:9), 28), collapse = ""), ".md")
    knitr::knit(x, tmp_name, quiet = TRUE)
    new_name <- paste0(tools::file_path_sans_ext(x), ".tex")
    pandoc_call <- paste("pandoc -f markdown -t latex -p -o", shQuote(new_name), shQuote(tmp_name))
    system(pandoc_call)
    file.remove(tmp_name)

    # Add appendix environment
    tex <- readLines(new_name)
    tex <- c("\\begin{appendix}", tex, "\\end{appendix}")
    write(tex, file = new_name)
  }

  return(new_name)
}
