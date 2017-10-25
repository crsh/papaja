#' APA article (6th edition)
#'
#' Template for creating an article according to APA guidelines (6th edition) in PDF format.
#'
#' @param fig_caption Logical. Indicates if figures are rendered with captions.
#' @param number_sections Logical. Indicates if section headers are be numbered. If
#' \code{TRUE}, figure/table numbers will be of the form X.i, where X is the current first
#' -level section number, and i is an incremental number (the i-th figure/table); if
#' \code{FALSE}, figures/tables will be numbered sequentially in the document from 1, 2,
#'  ..., and you cannot cross-reference section headers in this case.
#' @param toc Logical. Indicates if a table of contents is included.
#' @param pandoc_args Additional command line options to pass to pandoc
#' @param keep_tex Logical. Keep the intermediate tex file used in the conversion to PDF.
#' @param md_extensions Character. Markdown extensions to be added or removed from the
#'  default definition or R Markdown. See the \code{\link[rmarkdown]{rmarkdown_format}} for additional details.
#' @param ... Further arguments to pass to \code{\link[bookdown]{pdf_document2}} or \code{\link[bookdown]{word_document2}}.
#' @details
#'    When creating PDF documents the YAML option \code{class} is passed to the class options of the LaTeX apa6 document class.
#'    In this case, additional options are available. Refer to the apa6 document class
#'    \href{ftp://ftp.fu-berlin.de/tex/CTAN/macros/latex/contrib/apa6/apa6.pdf}{documentation} to find out about class options
#'    such as paper size or draft watermarks.
#'
#'    When creating PDF documents the output device for figures defaults to \code{c("pdf", "postscript", "png", "tiff")},
#'    so that each figure is saved in all four formats at a resolution of 300 dpi.
#' @seealso \code{\link[bookdown]{pdf_document2}}, \code{\link[bookdown]{word_document2}}
#' @examples NULL
#' @export

apa6_pdf <- function(
  fig_caption = TRUE
  , number_sections = FALSE
  , toc = FALSE
  , pandoc_args = NULL
  , keep_tex = TRUE
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)
  validate(number_sections, check_class = "logical", check_length = 1)
  validate(toc, check_class = "logical", check_length = 1)
  validate(keep_tex, check_class = "logical", check_length = 1)

  # Get APA6 template
  template <-  system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6.tex"
    , package = "papaja"
  )
  if(template == "") stop("No LaTeX template file found.")

  # Call pdf_document() with the appropriate options
  format <- bookdown::pdf_document2(
    template = template
    , fig_caption = fig_caption
    , number_sections = number_sections
    , toc = toc
    , keep_tex = keep_tex
    , pandoc_args = pandoc_args
    , ...
  )

  # Set chunk defaults
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  # format$knitr$opts_chunk$results <- "asis"
  format$knitr$opts_chunk$fig.cap <- " " # Ensures that figure environments are added
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  format$knitr$knit_hooks$inline <- inline_numbers

  format$knitr$opts_chunk$dev <- c("pdf", "png") # , "postscript", "tiff"
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

    args <- pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(is.null(metadata$citeproc) || metadata$citeproc) {
      metadata$citeproc <- FALSE
      assign("yaml_front_matter", metadata, pos = parent.frame())
    }

    args
  }

  format$pre_processor <- pre_processor

  if(Sys.info()["sysname"] == "Windows") {
    format$on_exit <- function() if(file.exists("_papaja_ampersand_filter.bat")) file.remove("_papaja_ampersand_filter.bat")
  }

  format
}


#' @describeIn apa6_pdf Format to create .docx-files. \code{class} parameter is ignored. \emph{This function
#'    should be considered experimental.}
#' @export

apa6_word <- function(
  fig_caption = TRUE
  , pandoc_args = NULL
  , md_extensions = NULL
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
  format <- bookdown::word_document2(
    reference_docx = reference_docx
    , fig_caption = fig_caption
    , pandoc_args = pandoc_args
    , ...
  )

  # Set chunk defaults
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  # format$knitr$opts_chunk$results <- "asis"
  format$knitr$opts_knit$rmarkdown.pandoc.to <- "docx"
  format$knitr$knit_hooks$inline <- inline_numbers
  # format$knitr$knit_hooks$plot <- function(x, options) {
  #   options$fig.cap <- paste("*", getOption("papaja.terms")$figure, ".* ", options$fig.cap)
  #   knitr::hook_plot_md(x, options)
  # }

  format$knitr$opts_chunk$dev <- c("png", "pdf") #, "svg", "tiff")
  format$knitr$opts_chunk$dpi <- 300
  format$clean_supporting <- FALSE # Always keep images files


  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL
  from_rmarkdown <- utils::getFromNamespace("from_rmarkdown", "rmarkdown")
  .from <- from_rmarkdown(fig_caption, md_extensions)

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  # to ensure right geometry defaults in the absence of user specified values
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from = .from) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- word_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(is.null(metadata$citeproc) || metadata$citeproc) {
      metadata$citeproc <- FALSE
      assign("yaml_front_matter", metadata, pos = parent.frame())
    }

    args
  }

  format$pre_processor <- pre_processor

  if(Sys.info()["sysname"] == "Windows") {
    format$on_exit <- function() if(file.exists("_papaja_ampersand_filter.bat")) file.remove("_papaja_ampersand_filter.bat")
  }

  format
}


# Set hook to print default numbers
inline_numbers <- function (x) {
  if(class(x) %in% c("difftime")) x <- as.numeric(x)
  if(is.numeric(x)) {
    printed_number <- ifelse(
      x == round(x)
      , as.character(x)
      , printnum(x)
    )
    n <- length(printed_number)
    if(n == 1) {
      printed_number
    } else if(n == 2) {
      paste(printed_number, collapse = " and ")
    } else if(n > 2) {
      paste(paste(printed_number[1:(n - 1)], collapse = ", "), printed_number[n], sep = ", and ")
    }
  } else if(is.character(x)) x
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
  # Parse and modify YAML header
  input_text <- readLines(input_file, encoding = "UTF-8")
  yaml_params <- get_yaml_params(input_text)

  ## Adds correspondence line to author note
  corresponding_author <- yaml_params$author[which(unlist(lapply(lapply(yaml_params$author, "[[", "corresponding"), isTRUE)))]

  if(length(corresponding_author) > 0) {
    yaml_params$author_note <- paste(
      yaml_params$author_note
      , corresponding_author_line(corresponding_author[[1]])
      , sep = "\n\n"
    )
  }

  ## Concatenate author names
  yaml_params$author <- author_ampersand(yaml_params$author, format = "latex")

  ## Add modified YAML header
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_text)
  augmented_input_text <- c("---", yaml::as.yaml(yaml_params), "---", input_text[(yaml_delimiters[2] + 1):length(input_text)])
  input_file_connection <- file(input_file, encoding = "UTF-8")
  writeLines(augmented_input_text, input_file)
  close(input_file_connection)

  args <- NULL
  if(is.null(metadata$citeproc) || metadata$citeproc) {

    # Set CSL
    args <- set_csl(input_file)

    # Set ampersand filter
    args <- set_ampersand_filter(args, metadata$csl)
  }

  args
}

word_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from) {
  # Parse and modify YAML header
  input_text <- readLines(input_file, encoding = "UTF-8")
  yaml_params <- get_yaml_params(input_text)

  ## Create title page
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_text)
  augmented_input_text <- c(word_title_page(yaml_params), input_text[(yaml_delimiters[2] + 1):length(input_text)])

  ## Remove abstract to avoid redundancy introduced by pandoc
  yaml_params$abstract <- NULL

  ## Add modified YAML header
  augmented_input_text <- c("---", yaml::as.yaml(yaml_params), "---", augmented_input_text)
  input_file_connection <- file(input_file, encoding = "UTF-8")
  writeLines(input_file_connection, input_file)
  close(input_file_connection)

  args <- NULL
  if(is.null(metadata$citeproc) || metadata$citeproc) {

    # Set CSL
    args <- set_csl(input_file)

    # Set ampersand filter
    args <- set_ampersand_filter(args, metadata$csl)
  }

  # Process markdown
  process_markdown <- utils::getFromNamespace("process_markdown", "bookdown")
  process_markdown(input_file, from, args, TRUE)

  args
}


get_yaml_params <- function(x) {
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", x)

  if(length(yaml_delimiters) >= 2 &&
     (yaml_delimiters[2] - yaml_delimiters[1] > 1) &&
     grepl("^---\\s*$", x[yaml_delimiters[1]])) {
    yaml_params <- yaml::yaml.load(paste(x[(yaml_delimiters[1] + 1):(yaml_delimiters[2] - 1)], collapse = "\n"))
    yaml_params
  } else NULL
}


author_ampersand <- function(x, format) {

  if(format == "latex") {
    authors <- lapply(x, function(y) {
      affiliation <- if(!is.null(y[["affiliation"]])) paste0("\\textsuperscript{", y[["affiliation"]], "}") else ""
      paste0(y["name"], affiliation, collapse = "")
    })
  } else if(format == "word") {
    authors <- lapply(x, function(y) {
      affiliation <- if(!is.null(y[["affiliation"]])) paste0("^", y[["affiliation"]], "^") else ""
      paste0(y["name"], affiliation, collapse = "")
    })
  } else {
    stop("Format not supported.")
  }

  authors <- unlist(authors)

  n_authors <- length(authors)
  x[[1]]$name <- authors[1]
  if(n_authors >= 2) {
    if(n_authors > 2) {
      x[[n_authors]]$name <- paste(", &", authors[n_authors])
      for(i in 2:(n_authors - 1)) {
        x[[i]]$name <- paste(",", authors[i])
      }
    } else {
      x[[n_authors]]$name <- paste("\\ &", authors[n_authors]) # Otherwise space before ampersand disappears
    }
  }
  x
}

set_ampersand_filter <- function(args, csl_file) {
  pandoc_citeproc <- utils::getFromNamespace("pandoc_citeproc", "rmarkdown")

  if(!is.null(args)) { # CSL has not been specified manually
    # Correct in-text ampersands
    filter_path <- system.file(
      "rmarkdown", "templates", "apa6", "resources"
      , "ampersand_filter.sh"
      , package = "papaja"
    )

    if(Sys.info()["sysname"] == "Windows") {
      filter_path <- gsub("\\.sh", ".bat", filter_path)
      ampersand_filter <- readLines(filter_path)
      ampersand_filter[2] <- gsub("PATHTORSCRIPT", paste0(R.home("bin"), "/Rscript.exe"), ampersand_filter[2])
      filter_path <- "_papaja_ampersand_filter.bat"
      filter_path_connection <- file(filter_path, encoding = "UTF-8")
      writeLines(ampersand_filter, filter_path_connection)
      close(filter_path_connection)
    }

    args <- c(args, "--filter", pandoc_citeproc(), "--filter", filter_path)
  } else {
    args <- c(args, "--csl", csl_file, "--filter", pandoc_citeproc())
  }

  args
}
