#' Revision Letter
#'
#' Template for creating a journal revision letters.
#'
#' @param ... Further arguments passed on to [bookdown::pdf_document2()] and [rmarkdown::pdf_document()].
#' @details This document format is adapted from by the
#'   [revision letter template](https://github.com/mschroen/review_response_letter)
#'   by Martin Schrön.
#'
#'   It is possible to reference sections, figures, or tables in the revised
#'   manuscript, either by their number or by page. To do so, specify a path
#'   to the revised manuscript's LaTeX file (omitting the file extension) in
#'   the YAML front matter (i.e., `manuscript-tex: file_name`) and ensure that
#'   you retain the `aux` file when rendering the revised manuscript. To do
#'   so, set the following option in a code chunk of the revised manuscript:
#'   `options(tinytex.clean = FALSE)`. To reference section, figure, or
#'   table numbers it is possible to use LaTeX (i.e., `\ref{label}`) or
#'   \pkg{bookdown} cross-referencing syntax (i.e., `\@ref(label)`). To
#'   reference the corresponding page numbers you must use the LaTeX syntax
#'   (i.e., `\pageref{label}`).
#'
#'   To quote entire paragraphs directly from the revised manuscript see
#'   [`quote_from_tex()`].
#'
#' @seealso [bookdown::pdf_document2()]], [rmarkdown::pdf_document()]
#' @inherit apa6_pdf return
#' @export


revision_letter_pdf <- function(...) {
  ellipsis <- list(...)

  if(!is.null(ellipsis$template)) ellipsis$template <- NULL

  # Get template
  template <- system.file(
    "rmarkdown", "templates", "revision_letter", "resources"
    , "revision_letter.tex"
    , package = "papaja"
  )
  if(template == "") stop("No LaTeX template file found.") else ellipsis$template <- template

  # Create format
  revision_letter_format <- do.call(bookdown::pdf_document2, ellipsis)

  # Set chunk defaults
  revision_letter_format$knitr$opts_chunk$echo <- FALSE
  revision_letter_format$knitr$opts_chunk$message <- FALSE
  revision_letter_format$knitr$opts_chunk$fig.cap <- " " # Ensures that figure environments are added
  revision_letter_format$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  revision_letter_format$knitr$knit_hooks$inline <- inline_numbers

  ## Overwrite preprocessor to set correct margin and CSL defaults
  saved_files_dir <- NULL

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  # to ensure right geometry defaults in the absence of user specified values
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- revision_letter_preprocessor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(
      (is.null(metadata$replace_ampersands) || metadata$replace_ampersands) &&
      (is.null(metadata$citeproc) || metadata$citeproc)
    ) {
      metadata$citeproc <- FALSE
      assign("front_matter", metadata, pos = parent.frame())
    }

    args
  }

  revision_letter_format$pre_processor <- pre_processor

  revision_letter_format
}


revision_letter_preprocessor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
  # Add pandoc arguments
  args <- NULL

  if((!is.list(metadata$output) ||  !is.list(rmarkdown::metadata$output[[1]]) || is.null(metadata$output[[1]]$citation_package)) &
     (is.null(metadata$citeproc) || metadata$citeproc)) {

    ## Set CSL
    args <- set_default_csl(input_file
                            , version = 6
                            , metadata = metadata)
    csl_specified <- is.null(args)

    ## Set ampersand filter
    if((is.null(metadata$replace_ampersands) || metadata$replace_ampersands)) {
      if(csl_specified) {
        args <- c(args, "--csl", metadata$csl)
      }

      args <- rmdfiltr::add_citeproc_filter(args)
      args <- rmdfiltr::add_replace_ampersands_filter(args)
    }
  }

  ## Set additional lua filters
  args <- rmdfiltr::add_wordcount_filter(args, error = FALSE)
  rc_filter <- system.file(
    "lua", "reviewer_comment.lua"
    , package = "papaja"
  )
  args <- rmdfiltr::add_custom_filter(args, filter_path = rc_filter, lua = TRUE)

  args
}


#' Quote from TeX document
#'
#' Includes a labelled quote from a LaTeX document as raw LaTeX 'asis'.
#'
#' @param x Character. One or more quote labels.
#' @param file Character. Path to LaTeX file from which to quote.
#'
#' @details Searches the LaTeX document specified in `file` for labelled
#'   quotes, i.e. paragraphs that are enclosed in `% <@~{#quote-label}` and
#'   `% ~@>` tags in LaTeX comments on separate lines. The labelled quote is
#'   then inserted as raw LaTeX block and rendered `asis`. To use labelled 
#'   quote-tags in a [`apa6_pdf()`]-document, set the YAML front matter options
#'   `quote_labels: true`.
#'
#' @return A character vector of LaTeX document text of class \code{knit_asis},
#'   see [knitr::asis_output()].
#' @export

quote_from_tex <- function(x, file = paste0(rmarkdown::metadata[["manuscript-tex"]], ".tex")) {
  label_warning <- paste0("Labelled quote(s) ", paste0("'", x, "'", collapse = ", "), " not found in ", file)

  tex <- read_and_cache_tex(file)

  if(length(x) > 1) {
    quoted_tex <- lapply(x, quote_from_tex, file = file)
  } else {
    start <- which(grepl(paste0("<@~\\{#", x, "\\}"), x = tex))

    if(length(start) == 0) {
      warning(label_warning)
      return(NULL)
    } else if(length(start) > 1) {
      stop(paste0("Each quote label can only be used once. ", paste0("'", x, "'", collapse = ", "), " was found ", length(start), " times."))
    } else {
      end <- which(grepl("~@>", x = tex[start:length(tex)]))[1] + start - 1

      quoted_tex <- paste(
        "```{=latex}"
        , "\\begin{quote}"
        , paste(tex[(start + 2):(end - 1)], collapse = "\n")
        , "\\end{quote}"
        , "```"
        , "\n"
        , sep = "\n"
      )
    }
  }
  knitr::asis_output(quoted_tex)
}

quote_from_tex_cache <- new.env(parent = emptyenv())

read_and_cache_tex <- function(x) {
  file_hash <- rlang::hash(x)

  if (!exists(file_hash, envir = quote_from_tex_cache, inherits = FALSE)) {
    tex <- readLines(x, encoding = "UTF-8")
    assign(file_hash, tex, envir = quote_from_tex_cache)
  } else {
    tex <- get(file_hash, envir = quote_from_tex_cache, inherits = FALSE)
  }

  tex
}
