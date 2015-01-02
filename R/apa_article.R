#' APA article (6th edition, PDF)
#'
#' Template for creating an article according to APA guidelines (6th edition) in PDF format.
#'
#' @param class Character. Specifies if the document will be a manuscript ("man"), a document ("doc"),
#'    or a fully typsetted journal article ("jou"). Currently only manuscripts are supported for
#'    MS Word documents.
#' @param fig.caption Logical. Indicates if figures are rendered with captions.
#' @param ... Further arguments to pass to \code{rmarkdown::pdf_document} or \code{rmarkdown::word_document}.
#' @example ...
#' @export

apa_article <- function(
  class = "man"
  , fig.caption = TRUE
  , ...
) {

  requireNamespace("knitr", quietly = TRUE)
  requireNamespace("rmarkdown", quietly = TRUE)

  # Get CSL template for APA6 citations
  csl_template <- system.file(
    "inst/rmarkdown/templates/apa6/resources/apa6.csl",
    package = "papaja"
  )

  # Get APA6 template or reference file depending on output format
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  if(output_format == "latex") {
    template <-  system.file(
      "inst/rmarkdown/templates/apa6/resources/apa6.tex",
      package = "papaja"
    )

    # Call pdf_document() with the appropriate options
    format <- rmarkdown::pdf_document(
      template = template
      , fig.caption = fig.caption
      , ...
    )
  } else {
    docx_reference <- system.file(
      "inst/rmarkdown/templates/apa6/resources/apa6_man.docx",
      package = "papaja"
    )
    if(class != "man") warning("Currently only class == 'man' is supported for MS Word documents.")

    # Call word_document() with the appropriate options
    format <- rmarkdown::word_document(
      docx_reference = docx_reference
      , fig.caption = fig.caption
      , ...
    )
  }


  # Set options
  knitr_options$opts_chunk$echo <- FALSE

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
  knitr_options$knit_hooks$inline <- inline_numbers

  # Override the knitr settings
  format$knitr <- knitr_options
  format
}
