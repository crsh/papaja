#' APA article (6th edition)
#'
#' Template for creating an article according to APA guidelines (6th edition) in PDF format.
#'
#' @param class Character. Specifies if the document will be a manuscript (\code{man}), a document (\code{doc}),
#'    or a fully typsetted journal article (\code{jou}). Currently only manuscripts are supported for
#'    MS Word documents. At this point coversion to MS word is considered experimental. See details.
#' @param fig_caption Logical. Indicates if figures are rendered with captions.
#' @param pandoc_args Additional command line options to pass to pandoc
#' @param keep_tex Logical. Keep the intermediate tex file used in the conversion to PDF.
#' @param ... Further arguments to pass to \code{\link[rmarkdown]{pdf_document}} or \code{\link[rmarkdown]{word_document}}.
#' @details
#'    When creating PDF documents \code{class} is passed to the class options of the LaTeX apa6 document class. In this case,
#'    additional options are available. Refer to the apa6 document class
#'    \href{ftp://ftp.fu-berlin.de/tex/CTAN/macros/latex/contrib/apa6/apa6.pdf}{documentation} to find out about class options
#'    such as paper size or draft watermarks.
#'
#'    When creating PDF documents the output device for figures defaults to \code{c("pdf", "postscript", "png", "tiff")},
#'    so that each figure is saved in all four formats at a resolution of 300 dpi.
#' @seealso \code{\link[rmarkdown]{pdf_document}}, \code{\link[rmarkdown]{word_document}}
#' @examples NULL
#' @export

apa6_pdf <- function(
  class = "man"
  , fig_caption = TRUE
  , pandoc_args = NULL
  , keep_tex = TRUE
  , ...
) {

  validate(class, check_class = "character", check_length = 1)
  if(!any(class %in% c("man", "doc", "jou"))) stop("Document class must be either 'man', 'doc', or 'jou'.")
  validate(fig_caption, check_class = "logical", check_length = 1)
  validate(keep_tex, check_class = "logical", check_length = 1)

  # Get CSL template for APA6 citations
  csl_template <- system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6.csl"
    , package = "papaja"
  )
  if(csl_template == "") stop("No CSL template file found.")

  pandoc_args <- c(pandoc_args, c("--csl", rmarkdown::pandoc_path_arg(csl_template)))

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

  # Set options
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$results <- "asis"
  format$knitr$opts_chunk$fig.cap <- " "
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  format$knitr$knit_hooks$inline <- inline_numbers

  format$knitr$opts_chunk$dev <- c("pdf", "postscript", "png", "tiff")
  format$knitr$opts_chunk$dpi <- 300
  format$clean_supporting <- FALSE # Always keep images files
  format
}


#' @describeIn apa6_pdf Format to create .docx-files. \code{class} parameter is ignored. \emph{This function
#'    should be considered experimental.}
#' @export

apa6_word <- function(
  class = "man"
  , fig_caption = TRUE
  , pandoc_args = NULL
  , ...
) {

  # Get CSL template for APA6 citations
  csl_template <- system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6.csl"
    , package = "papaja"
  )
  if(csl_template == "") stop("No CSL template file found.")

  pandoc_args <- c(pandoc_args, c("--csl", rmarkdown::pandoc_path_arg(csl_template)))

  # Get APA6 reference file
  reference_docx <- system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6_man.docx"
    , package = "papaja"
  )
  if(reference_docx == "") stop("No .docx-reference file found.")
  if(class != "man") warning("Currently only class == 'man' is supported for MS Word documents.")

  # Call word_document() with the appropriate options
  format <- rmarkdown::word_document(
    reference_docx = reference_docx
    , fig_caption = fig_caption
    , pandoc_args = pandoc_args
    , ...
  )

  # Set options
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$results <- "asis"
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "word"
  format$knitr$knit_hooks$inline <- inline_numbers

#   format$knitr$opts_chunk$dev <- c("png", "pdf", "svg", "tiff")
#   format$knitr$opts_chunk$dpi <- 300
  format$clean_supporting <- FALSE # Always keep images files
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
