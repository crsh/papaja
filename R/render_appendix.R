#' Render appendix
#'
#' This functions renders an R Markdown document \emph{without} YAML header to a TeX fragment inside
#' an \code{appendix} environment.
#'
#' @param x Character. Input file name.
#' @param options Character. Vector of options passed to \code{\link[rmarkdown]{pandoc_convert}}.
#' @param encoding Character. Encoding of the input file; see \code{\link{file}}.
#' @param quiet Logical. Supresses pandoc command line output; see \code{\link[rmarkdown]{render}}.
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
#'    which \code{render_appendix} is called; otherwise defaults of \code\link[rmarkdown]{md_document} are
#'    used. It is, therefore, recommended to include \code{render_appendix} in your parent document.
#' @export

render_appendix <- function(
  x
  , options = NULL
  , encoding = NULL
  , quiet = TRUE
) {
  target_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  md_format <- rmarkdown::md_document()

  if(is.null(encoding)) {
    encoding <- ifelse(length(target_format) > 0, knitr::opts_knit$get("encoding"), getOption("encoding"))
    md_format$knitr$opts_chunk <- knitr::opts_chunk$get()[names(knitr::opts_chunk$get()) != "fig.path"]
    md_format$knitr$knit_hooks <- knitr::knit_hooks$get()
  }

  md_file <- rmarkdown::render(
    x
    , output_format = md_format
    , encoding = encoding
    , quiet = quiet
  )

  if(length(target_format) == 0 || target_format == "latex") {
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

    write(tex, file = new_name)
    file.remove(md_file)

    if(!is.null(status)) return(status)
  } else {
    warning(target_format, " documents currently do not support appendices via includes.")
  }

  return(invisible(0))
}
