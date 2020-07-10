#' Journal revision letter
#'
#' Template for creating an journal revision letters.
#'
#' @param keep_tex Logical. Logical. Keep the intermediate tex file used in the conversion to PDF.
#' @inheritDotParams bookdown::pdf_document2
#'
#' @seealso \code{\link[bookdown]{html_document2}}
#' @examples NULL
#' @export


revision_letter_pdf <- function(keep_tex = TRUE, ...) {
  validate(keep_tex, check_class = "logical", check_length = 1)

  ellipsis <- list(...)
  ellipsis$keep_tex <- keep_tex

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

    revision_letter_preprocessor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)
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
    args <- set_default_csl(input_file)
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

  args
}


#' Quote from TeX document
#'
#' Includes a labelled quote from a LaTeX document 'asis'.
#'
#' @param x Character. One or more quote labels.
#' @param file Character. Path to LaTeX file from which to quote.
#'
#' @details Searches the LaTeX document for a labelled quotes preceeded and
#'   followed by \code{\% <@~{#quote-label}} and \code{\% ~@>} tags in LaTeX
#'   comments
#'
#' @export

quote_from_tex <- function(x, file) {
  if(length(x) > 1) {
    quoted_tex <- lapply(x, quote_from_tex, file = file)
  } else if(length(x) == 0) {
    warning(paste0("Quote label(s) ", paste0("'", x, "'", collapse = ", "), " not found in ", file))
    quoted_tex <- NULL
  } else {
    tex <- readLines(file)
    start <- which(grepl(paste0("% <@~{#", x, "}"), x = tex, fixed = TRUE))
    end <- which(grepl("% ~@>", x = tex[start:length(tex)], fixed = TRUE))[1] + start - 1

    quoted_tex <- paste(
      paste("> ", tex[(start + 1)])
      , paste(tex[(start + 2):(end - 1)], collapse = "\n")
      , "\n"
      , sep = "\n"
    )
  }
  knitr::asis_output(quoted_tex)
}
