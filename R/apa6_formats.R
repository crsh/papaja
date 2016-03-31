#' APA article (6th edition)
#'
#' Template for creating an article according to APA guidelines (6th edition) in PDF format.
#'
#' @param fig_caption Logical. Indicates if figures are rendered with captions.
#' @param pandoc_args Additional command line options to pass to pandoc
#' @param keep_tex Logical. Keep the intermediate tex file used in the conversion to PDF.
#' @param ... Further arguments to pass to \code{\link[rmarkdown]{pdf_document}} or \code{\link[rmarkdown]{word_document}}.
#' @details
#'    When creating PDF documents the YAML option \code{class} is passed to the class options of the LaTeX apa6 document class.
#'    In this case, additional options are available. Refer to the apa6 document class
#'    \href{ftp://ftp.fu-berlin.de/tex/CTAN/macros/latex/contrib/apa6/apa6.pdf}{documentation} to find out about class options
#'    such as paper size or draft watermarks.
#'
#'    When creating PDF documents the output device for figures defaults to \code{c("pdf", "postscript", "png", "tiff")},
#'    so that each figure is saved in all four formats at a resolution of 300 dpi.
#' @seealso \code{\link[rmarkdown]{pdf_document}}, \code{\link[rmarkdown]{word_document}}
#' @examples NULL
#' @export

apa6_pdf <- function(
  fig_caption = TRUE
  , pandoc_args = NULL
  , keep_tex = TRUE
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)
  validate(keep_tex, check_class = "logical", check_length = 1)

  # Get APA6 template
    template <-  system.file(
      "rmarkdown", "templates", "apa6", "resources"
      , "apa6.tex"
      , package = "papaja"
    )
    if(template == "") stop("No LaTeX template file found.")

    # Call pdf_document() with the appropriate options
    format <- rmarkdown::pdf_document(
      template = template
      , fig_caption = fig_caption
      , keep_tex = keep_tex
      , pandoc_args = pandoc_args
      , ...
    )

  # Set chunk defaults
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$results <- "asis"
  format$knitr$opts_chunk$fig.cap <- " " # Ensures that figure environments are added
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  format$knitr$knit_hooks$inline <- inline_numbers

  format$knitr$opts_chunk$dev <- c("pdf", "postscript", "png", "tiff")
  format$knitr$opts_chunk$dpi <- 300
  format$clean_supporting <- FALSE # Always keep images files

  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  # to ensure right geometry defaults in the absence of user specified values
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)
  }

  format$pre_processor <- pre_processor
  format
}


#' @describeIn apa6_pdf Format to create .docx-files. \code{class} parameter is ignored. \emph{This function
#'    should be considered experimental.}
#' @export

apa6_word <- function(
  fig_caption = TRUE
  , pandoc_args = NULL
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)

  # Get APA6 reference file
  reference_docx <- system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6_man.docx"
    , package = "papaja"
  )
  if(reference_docx == "") stop("No .docx-reference file found.")

  # Call word_document() with the appropriate options
  format <- rmarkdown::word_document(
    reference_docx = reference_docx
    , fig_caption = fig_caption
    , pandoc_args = pandoc_args
    , ...
  )

  # Set chunk defaults
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$results <- "asis"
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "word"
  format$knitr$knit_hooks$inline <- inline_numbers

#   format$knitr$opts_chunk$dev <- c("png", "pdf", "svg", "tiff")
#   format$knitr$opts_chunk$dpi <- 300
  format$clean_supporting <- FALSE # Always keep images files


  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  # to ensure right geometry defaults in the absence of user specified values
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    word_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)
  }

  format$pre_processor <- pre_processor
  format
}


# Set hook to print default numbers
inline_numbers <- function (x) { # http://www.jason-french.com/blog/2014/04/25/formatting-sweave-and-knitr-output-for-2-digits/
  if (is.numeric(x)) {
    res <- ifelse(
      x == round(x)
      , sprintf("%d", x)
      , sprintf("%.2f", x)
    )
    paste(res, collapse = ", ")
  } else if (is.character(x)) x
}


# Preprocessor functions are adaptations from the RMarkdown package
# (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
# to ensure right geometry defaults in the absence of user specified values

set_csl <- function(x) {
  # Use APA6 CSL citations template if no other file is supplied
  has_csl <- function(text) {
    length(grep("^csl:.*$", text)) > 0
  }

  if (!has_csl(readLines(x, warn = FALSE))) {
    csl_template <- system.file(
      "rmarkdown", "templates", "apa6", "resources"
      , "apa6.csl"
      , package = "papaja"
    )
    if(csl_template == "") stop("No CSL template file found.")
    return(c("--csl", rmarkdown::pandoc_path_arg(csl_template)))
  } else NULL
}

pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
  args <- set_csl(input_file)
  args
}

word_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
  args <- set_csl(input_file)
  # args <- c(args, '--variable abstract="test"')
  args
}
