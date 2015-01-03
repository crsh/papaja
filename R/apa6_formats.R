#' APA article (6th edition)
#'
#' Template for creating an article according to APA guidelines (6th edition) in PDF format.
#'
#' @param class Character. Specifies if the document will be a manuscript ("man"), a document ("doc"),
#'    or a fully typsetted journal article ("jou"). Currently only manuscripts are supported for
#'    MS Word documents.
#' @param fig_caption Logical. Indicates if figures are rendered with captions.
#' @param pandoc_args Additional command line options to pass to pandoc
#' @param Logical. Keep the intermediate tex file used in the conversion to PDF.
#' @param ... Further arguments to pass to \code{rmarkdown::pdf_document} or \code{rmarkdown::word_document}.
#' @seealso \code{\link[rmarkdown]{pdf_document}}, \code{\link[rmarkdown]{word_document}}
#' @examples NULL
#' @export

apa6_pdf <- function(
  class = "man"
  , fig_caption = TRUE
  , pandoc_args = NULL
  , keep.tex = TRUE
  , ...
) {

  requireNamespace("knitr", quietly = TRUE)
  requireNamespace("rmarkdown", quietly = TRUE)

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
      , pandoc_args = pandoc_args
      , ...
    )

  # Set options
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"

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
  format$knitrknit_hooks$inline <- inline_numbers
  format
}


#' @describeIn apa6_pdf Format to create .docx-files. \code{class} parameter is ignored.
#' @export

apa6_word <- function(
  class = "man"
  , fig_caption = TRUE
  , pandoc_args = NULL
  , ...
) {

  requireNamespace("knitr", quietly = TRUE)
  requireNamespace("rmarkdown", quietly = TRUE)

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
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "word"

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
  format$knitrknit_hooks$inline <- inline_numbers
  format
}
