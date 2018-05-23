#' Render appendix
#'
#' This functions renders an R Markdown document \emph{without} YAML header to a TeX fragment inside
#' an \code{appendix} environment.
#'
#' @param x Character. Input file name.
#' @param bibliography Character. Location of bibliography file(s) to use.
#' @param csl Character. Location of CSL file to use. Defaults to APA-style.
#' @param quiet Logical. Suppresses pandoc command line output; see \code{\link[rmarkdown]{render}}.
#' @param options Character. Vector of options passed to \code{\link[rmarkdown]{pandoc_convert}}.
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
  , bibliography = rmarkdown::metadata$bibliography
  , csl = rmarkdown::metadata$csl
  , quiet = TRUE
  , options = NULL
) {
  validate(x, check_class = "character", check_length = 1)

  if(length(bibliography) > 0) {
    validate(bibliography, check_class = "character")
    bib_call <- paste0("--bibliography=", bibliography)
  }

  if(length(csl) > 0) {
    validate(csl, check_class = "character", check_length = 1)
  } else {
    csl <- system.file(
      "rmd", "apa6.csl"
      , package = "papaja"
    )
  }
  csl_call <- paste0("--csl=", csl)

  if(!is.null(options)) {
    validate(options, check_class = "character")
    options <- c(options, bib_call, csl_call)
  } else if(length(bibliography) > 0) {
    options <- c(bib_call, csl_call)
  }

  validate(quiet, check_class = "logical", check_length = 1)


  target_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(length(target_format) == 0) stop("render_appendix() can only be used within an R Markdown document; please include the function call in a code chunk.")

  if(target_format == "latex") {

    # Render Markdown fragment
    md_fragment <- knitr::knit_child(text = readLines(x), quiet = quiet)

    # # Remove placement options
    # # if (and only if) class: man and figsintext: no
    # if(!rmarkdown::metadata$figsintext && grepl("man", rmarkdown::metadata$class)) {
    #   md_fragment <- gsub("(\\\\begin\\{table\\})(\\[.+?\\])", "\\1", md_fragment)
    #   md_fragment <- gsub("(\\\\begin\\{figure\\})(\\[.+?\\])", "\\1", md_fragment)
    # }

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
    tex <- readLines(new_name, encoding = "UTF-8")
    if(!grepl("\\\\section|\\\\hypertarget", tex[tex != ""][1])) tex <- c("\\section{}", tex) # Add section to start appendix
    appendix_endfloat_fix <- ifelse(
      grepl("man", rmarkdown::metadata$classoption) || grepl("man", rmarkdown::metadata$class)
      , "\\makeatletter\n\\efloat@restorefloats\n\\makeatother"
      , ""
    )
    tex <- c("\\clearpage", appendix_endfloat_fix, "\n\n\\begin{appendix}", tex, "\\end{appendix}")

    write(tex, file = new_name)
    file.remove(md_file)

    if(!is.null(status)) return(status)
  } else {
    warning(target_format, " documents currently do not support appendices via includes.")
  }

  return(invisible(0))
}
