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
  template <- template <-  system.file(
    "rmarkdown", "templates", "revision_letter", "resources"
    , "revision_letter.tex"
    , package = "papaja"
  )
  if(template == "") stop("No LaTeX template file found.") else ellipsis$template <- template

  # Create format
  revision_letter_format <- do.call(bookdown::pdf_document2, ellipsis)

  revision_letter_format$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"

  ## Overwrite preprocessor to set correct margin and CSL defaults
  saved_files_dir <- NULL

  # # Preprocessor functions are adaptations from the RMarkdown package
  # # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  # # to ensure right geometry defaults in the absence of user specified values
  # pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
  #   # save files dir (for generating intermediates)
  #   saved_files_dir <<- files_dir
  #
  #   pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)
  # }
  #
  # revision_letter_format$pre_processor <- pre_processor

  revision_letter_format
}
