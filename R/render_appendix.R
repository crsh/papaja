#' Render appendix
#'
#' This functions renders an R Markdown document \emph{without} YAML header to a TeX fragment inside
#' an \code{appendix} environment.
#'
#' @param x Character. Input file name.
#' @param options Character. Vector of options passed to \code{\link[rmarkdown]{pandoc_convert}}.
#' @param quiet Logical. Suppresses pandoc command line output; see \code{\link[rmarkdown]{render}}.
#'    If \code{FALSE} output will be included in the document.
#' @details
#'    By default \code{x} is converted to a TeX file which can be included in an R Markdown document
#'    as \code{include}:
#'
#'    \preformatted{
#'    output:
#'      pdf_document:
#'        include:
#'        after_body: appendix.tex
#'    }
#'
#'    If \code{render_appendix} is called form an R Markdown document with a target document type other
#'    than a PDF file a Markdown document is created and a warning returned (includes are only supported
#'    in PDF documents).
#'
#'    Default chunk options and hooks are set to those used in the R Markdown document from
#'    which \code{render_appendix} is called; otherwise defaults of \code{\link[rmarkdown]{md_document}} are
#'    used. It is, therefore, recommended to include \code{render_appendix} in your parent document.
#' @export

render_appendix <- function(
  x
  , options = NULL
  , quiet = TRUE
) {
  validate(x, check_class = "character", check_length = 1)
  if(!is.null(options)) validate(options, check_class = "character")
  validate(quiet, check_class = "logical", check_length = 1)


  target_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(length(target_format) == 0) stop("render_appendix() can only be used within an R Markdown document; please include the function call in a code chunk.")

  if(target_format == "latex") {

    # Render Markdown fragment
    md_fragment <- knitr::knit_child(text = readLines(x), quiet = quiet)

    ## Remove placement options
    if(!rmarkdown::metadata$figsintext) {
      md_fragment <- gsub("(\\\\begin\\{table\\})(\\[.+?\\])", "\\1", md_fragment)
      md_fragment <- gsub("(\\\\begin\\{figure\\})(\\[.+?\\])", "\\1", md_fragment)
    }

    md_file <- paste0(tools::file_path_sans_ext(x), ".md")
    write(md_fragment, file = md_file, sep = "\n")

    new_name <- paste0(tools::file_path_sans_ext(x), ".tex")

    # Create TeX-file
    status <- rmarkdown::pandoc_convert(
      md_file
      , output = new_name
      , citeproc = TRUE
      , options = options
    )

    # Add appendix environment
    tex <- readLines(new_name)
    if(!grepl("\\\\section", tex[tex != ""][1])) tex <- c("\\section{}", tex) # Add section to start appendix
    tex <- c("\\begin{appendix}", tex, "\\end{appendix}")
    tex <- gsub("\\\\begin\\{figure\\}\\[htbp\\]", "\\\\begin{figure}", tex) # Remove placement option

    write(tex, file = new_name)
    file.remove(md_file)

    if(!is.null(status)) return(status)
  } else {
    warning(target_format, " documents currently do not support appendices via includes.")
  }

  return(invisible(0))
}
