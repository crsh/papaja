#' Render appendix
#'
#' This functions renders an R Markdown document \emph{without} YAML header to a TeX fragment inside
#' an \code{appendix} environment.
#'
#' @param x Character. Input file name.
#' @param bibliography Character. Location of bibliography file(s) to use.
#' @param csl Character. Location of CSL file to use. Defaults to APA-style.
#' @param quiet Logical. Suppresses pandoc command line output; see \code{\link[rmarkdown]{render}}.
#'    If \code{FALSE} output will be included in the document.
#' @inheritDotParams rmarkdown::pandoc_convert
#' @details
#'    By default \code{x} is converted to a TeX file which can be included in an R Markdown document
#'    as \code{include}:
#'
#'    \preformatted{
#'    output:
#'      pdf_document:
#'        include:
#'          after_body: appendix.tex
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
  , ...
) {
  validate(x, check_class = "character", check_length = 1)
  if(!is.null(csl)) validate(csl, check_class = "character", check_length = 1)
  validate(quiet, check_class = "logical", check_length = 1)

  if(length(bibliography) > 0) {
    validate(bibliography, check_class = "character")
    bibliography <- sapply(bibliography, tools::file_path_as_absolute)
    existing_bibliographies <- bibliography[file.exists(bibliography)]
    if(length(bibliography) > length(existing_bibliographies)) warning(paste("The following bibliography files could not be located:", bibliography[!bibliography %in% existing_bibliographies], sep = "\n", collapse = "\n"))
    if(length(existing_bibliographies) > 0) {
      bib_call <- paste0("--bibliography=", existing_bibliographies)
    }
  } else {
    bib_call <- NULL
  }

  if(is.list(rmarkdown::metadata$output) && is.list(rmarkdown::metadata$output[[1]]) && !is.null(rmarkdown::metadata$output[[1]]$citation_package)) {
    citation_package_call <- paste0("--", rmarkdown::metadata$output[[1]]$citation_package)
  } else {
    citation_package_call <- NULL
  }

  if(is.null(citation_package_call)) {
    if(!is.null(bib_call) & length(csl) > 0) {
      validate(csl, check_class = "character", check_length = 1)
    } else {
      csl <- system.file(
        "rmd", "apa6.csl"
        , package = "papaja"
      )
    }

    csl_call <- paste0("--csl=", csl)
  } else {
    bib_call <- gsub("^--", "-M ", bib_call)
    csl_call <- NULL
  }

  ellipsis <- list(...)
  ellipsis$options <- c(ellipsis$options, bib_call, csl_call, citation_package_call)

  validate(quiet, check_class = "logical", check_length = 1)

  target_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(length(target_format) == 0) stop("render_appendix() can only be used within an R Markdown document; please include the function call in a code chunk.")

  if(target_format %in% c("latex", "word", "docx")) {
    # Render Markdown fragment
    md_fragment <- knitr::knit_child(text = readLines(x, encoding = "UTF-8"), quiet = quiet)
  } else {
    warning(target_format, " documents currently do not support appendices via includes.")
  }

  if(target_format == "latex") {
    # Create TeX-file
    md_file <- paste0(tools::file_path_sans_ext(tools::file_path_as_absolute(x)), ".md")
    md_connection <- file(md_file, encoding = "UTF-8")
    on.exit(closeAllConnections())

    writeLines(md_fragment, con = md_connection, sep = "\n", useBytes = TRUE)
    on.exit(file.remove(md_file))

    new_name <- paste0(tools::file_path_sans_ext(x), ".tex")

    ellipsis$input <- md_file
    ellipsis$output <- basename(new_name)
    ellipsis$citeproc <- is.null(citation_package_call)

    status <- do.call(rmarkdown::pandoc_convert, ellipsis)

    # Add appendix environment
    tex_connection <- file(new_name, encoding = "UTF-8")
    tex <- readLines(con = tex_connection)

    ## Check whether Rmd starts with heading, otherwise add empty section
    md_fragment <- unlist(strsplit(md_fragment, split = "\n"))
    if(!grepl("^#(\\b|\\s)", md_fragment[!grepl("^\\\\", md_fragment) & md_fragment != ""][1])) {
      tex <- c("\\section{}", tex)
    }

    appendix_endfloat_fix <- ifelse(
      grepl("man", rmarkdown::metadata$classoption) || grepl("man", rmarkdown::metadata$class)
      , "\\makeatletter\n\\efloat@restorefloats\n\\makeatother"
      , ""
    )
    tex <- c("\\clearpage", appendix_endfloat_fix, "\n\n\\begin{appendix}", tex, "\\end{appendix}")

    writeLines(tex, con = tex_connection, useBytes = TRUE)

    if(!is.null(status)) return(status)
  } else if(target_format %in% c("latex", "word", "docx")) {
    cat(md_fragment)
  }

  return(invisible(0))
}
