#' APA article (6th edition)
#'
#' Template for creating an article according to APA guidelines (6th edition) in PDF format.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param md_extensions Markdown extensions to be added or removed from the default definition or R Markdown. See the \code{\link[rmarkdown]{rmarkdown_format}} for additional details.
#' @param ... Further arguments to pass to \code{\link[rmarkdown]{pdf_document}} or \code{\link[rmarkdown]{word_document}}.
#' @details
#'    When creating PDF documents the YAML option \code{classoption} is passed to the class options of the LaTeX apa6 document class.
#'    In this case, additional options are available. Refer to the apa6 document class
#'    \href{ftp://ftp.fu-berlin.de/tex/CTAN/macros/latex/contrib/apa6/apa6.pdf}{documentation} to find out about class options
#'    such as paper size or draft watermarks.
#'
#'    When creating PDF documents the output device for figures defaults to \code{c("pdf", "png")},
#'    so that each figure is saved in all four formats at a resolution of 300 dpi.
#' @seealso \code{\link[bookdown]{pdf_document2}}, \code{\link[rmarkdown]{pdf_document}}, \code{\link[bookdown]{word_document2}}, \code{\link[rmarkdown]{word_document}}
#' @examples NULL
#' @export

apa6_pdf <- function(
  fig_caption = TRUE
  , number_sections = FALSE
  , toc = FALSE
  , keep_tex = TRUE
  , md_extensions = NULL
  # , includes = NULL
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)
  validate(number_sections, check_class = "logical", check_length = 1)
  validate(toc, check_class = "logical", check_length = 1)
  validate(keep_tex, check_class = "logical", check_length = 1)
  # if(!is.null(includes)) {
  #   validate(includes, check_class = "list")
  # } else {
  #   includes <- rmarkdown::includes()
  # }

  # apa6_header_includes <-  system.file(
  #   "rmarkdown", "templates", "apa6", "resources"
  #   , "apa6_header_includes.tex"
  #   , package = "papaja"
  # )
  # if(apa6_header_includes == "") stop("LaTeX header includes file not found.")

  # includes$in_header <- c(includes$in_header, apa6_header_includes)

  # Call pdf_document() with the appropriate options
  config <- bookdown::pdf_document2(
    # template = template
    # , fig_caption = fig_caption
    fig_caption = fig_caption
    , number_sections = number_sections
    , toc = toc
    , keep_tex = keep_tex
    , md_extensions = md_extensions
    # , includes = includes
    , ...
  )

  # Set chunk defaults
  config$knitr$opts_chunk$echo <- FALSE
  config$knitr$opts_chunk$message <- FALSE
  # config$knitr$opts_chunk$results <- "asis"
  config$knitr$opts_chunk$fig.cap <- " " # Ensures that figure environments are added
  config$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  config$knitr$knit_hooks$inline <- inline_numbers

  config$knitr$opts_chunk$dev <- c("pdf", "png") # , "postscript", "tiff"
  config$knitr$opts_chunk$dpi <- 300
  config$clean_supporting <- FALSE # Always keep images files

  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(
      (is.null(metadata$replace_ampersands) || metadata$replace_ampersands) &&
      (is.null(metadata$citeproc) || metadata$citeproc)
    ) {
      metadata$citeproc <- FALSE
      assign("yaml_front_matter", metadata, pos = parent.frame())
    }

    args
  }

  config$pre_processor <- pre_processor

  config$post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    # Remove indentation so that endfloat can process the lltable environments
    output_text <- readLines(output_file, encoding = "UTF-8")
    appendix_lines <- grep("\\\\(begin|end)\\{appendix\\}", output_text)
    if(length(appendix_lines) == 2) {
      output_text[appendix_lines[1]:appendix_lines[2]] <- gsub(
        "^\\s+"
        , ""
        , output_text[appendix_lines[1]:appendix_lines[2]]
      )
      output_file_connection <- file(output_file, encoding = "UTF-8")
      writeLines(output_text, output_file_connection)
      close(output_file_connection)
    }

    # Apply bookdown postprocesser and pass format options
    bookdown_post_processor <- bookdown::pdf_document2()$post_processor
    pp_env <- environment(bookdown_post_processor)
    assign("post", NULL, envir = pp_env) # Postprocessor is not self-contained
    assign("config", config, envir = pp_env) # Postprocessor is not self-contained
    bookdown_post_processor(metadata = metadata, input = input_file, output = output_file, clean = clean, verbose = verbose)
  }

  if(Sys.info()["sysname"] == "Windows") {
    config$on_exit <- function() if(file.exists("_papaja_ampersand_filter.bat")) file.remove("_papaja_ampersand_filter.bat")
  }

  config
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
  config <- bookdown::word_document2(
    reference_docx = reference_docx
    , fig_caption = fig_caption
    , pandoc_args = pandoc_args
    , md_extensions = md_extensions
    , ...
  )

  # Set chunk defaults
  config$knitr$opts_chunk$echo <- FALSE
  config$knitr$opts_chunk$message <- FALSE
  # config$knitr$opts_chunk$results <- "asis"
  config$knitr$opts_knit$rmarkdown.pandoc.to <- "docx"
  config$knitr$knit_hooks$inline <- inline_numbers
  # config$knitr$knit_hooks$plot <- function(x, options) {
  #   options$fig.cap <- paste("*", getOption("papaja.terms")$figure, ".* ", options$fig.cap)
  #   knitr::hook_plot_md(x, options)
  # }

  config$knitr$opts_chunk$dev <- c("png", "pdf") #, "svg", "tiff")
  config$knitr$opts_chunk$dpi <- 300
  config$clean_supporting <- FALSE # Always keep images files


  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL
  .from <- rmarkdown::from_rmarkdown(fig_caption, md_extensions)

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from = .from) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- word_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(
      (is.null(metadata$replace_ampersands) || metadata$replace_ampersands) &&
      (is.null(metadata$citeproc) || metadata$citeproc)
    ) {
      metadata$citeproc <- FALSE
      assign("yaml_front_matter", metadata, pos = parent.frame())
    }

    args
  }

  config$pre_processor <- pre_processor

  if(Sys.info()["sysname"] == "Windows") {
    config$on_exit <- function() if(file.exists("_papaja_ampersand_filter.bat")) file.remove("_papaja_ampersand_filter.bat")
  }

  config
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
  } else if(is.integer(x)) {
    x <- printnum(x)
  } else if(is.character(x)) {
    x
  }
}


# Preprocessor functions are adaptations from the RMarkdown package
# (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)

set_csl <- function(x) {
  # Use APA6 CSL citations template if no other file is supplied
  has_csl <- function(text) {
    length(grep("^csl:.*$", text)) > 0
  }

  if (!has_csl(readLines(x, warn = FALSE))) {
    csl_template <- system.file(
      "rmd", "apa6.csl"
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

  ## Set template defaults
  if(is.null(yaml_params$documentclass)) yaml_params$documentclass <- "apa6"

  if(!is.null(yaml_params[["class"]])) { # Depricated class options
    classoption <- paste(yaml_params[["class"]], collapse = ",")
    yaml_params$classoption <- paste(paste(yaml_params$classoption, collapse = ","), classoption, sep = ",")
  } else if(is.null(yaml_params$classoption)) {
    yaml_params$classoption <- "man"
  }

  ## Class options
  if(isTRUE(yaml_params$mask)) yaml_params$classoption <- paste0(yaml_params$classoption, ",mask")
  if(isTRUE(yaml_params$figsintext) || isTRUE(yaml_params$floatsintext)) {
    yaml_params$classoption <- paste0(yaml_params$classoption, ",floatsintext")
  }
  if(isTRUE(yaml_params$draft)) yaml_params$classoption <- paste0(yaml_params$classoption, ",draftall")

  ## Deprecated lang arguments
  if(!is.null(yaml_params$lang)) { # Depricated default lang options in papaja templates
    yaml_params$lang <- switch(
      yaml_params$lang
      , english = "en-EN"
      , american = "en-US"
      , yaml_params$lang
    )
  }

  ## Add necessary header includes
  header_includes <- list()

  ### Essential manuscript parts
  if(!is.null(yaml_params$shorttitle)) {
    short_title <- paste0("\\shorttitle{", yaml_params$shorttitle, "}")
  } else {
    short_title <- paste0("\\shorttitle{SHORTTITLE}")
  }
  header_includes <- c(header_includes, short_title)

  if(!is.null(yaml_params$leftheader)) {
    header_includes <- c(header_includes, paste0("\\leftheader{", yaml_params$leftheader, "}"))
  }

  corresponding_author <- yaml_params$author[which(unlist(lapply(lapply(yaml_params$author, "[[", "corresponding"), isTRUE)))]

  if(length(corresponding_author) > 0) {
    author_note <- paste(
      c(yaml_params$author_note, yaml_params$authornote)
      , corresponding_author_line(corresponding_author[[1]])
      , sep = "\n\n"
    )

    header_includes <- c(header_includes, paste0("\\authornote{", author_note, "}"))
  }

  affiliations <- paste_affiliations(yaml_params$affiliation, format = "latex")
  header_includes <- c(header_includes, paste0("\\affiliation{\n\\vspace{0.5cm}\n", affiliations, "}"))

  yaml_params$author <- paste_authors(yaml_params$author, format = "latex")

  if(!is.null(yaml_params$note)) {
    header_includes <- c(header_includes, paste0("\\note{", yaml_params$note, "}"))
  }

  if(!is.null(yaml_params$abstract)) {
    abstract <- yaml_params$abstract
    yaml_params$abstract <- NULL

    header_includes <- c(header_includes, paste0("\\abstract{", abstract, "}"))
  }

  if(!is.null(yaml_params$keywords) || !is.null(yaml_params$wordcount)) {
    keywords <- paste(unlist(yaml_params$keywords), collapse = ", ")
    if(!is.null(yaml_params$wordcount)) {
      keywords <- paste0(keywords, "\\newline\\indent Word count: ", yaml_params$wordcount)
    }
    header_includes <- c(header_includes, paste0("\\keywords{", keywords, "}"))
  }

  ### Manuscript and table formatting
  apa6_header_includes <-  system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6_header_includes.tex"
    , package = "papaja"
  )
  if(apa6_header_includes == "") stop("LaTeX header includes file not found.")

  apa6_header_includes <- readLines(apa6_header_includes, encoding = "UTF-8")
  apa6_header_includes <- apa6_header_includes[!grepl("^%", apa6_header_includes)]
  apa6_header_includes <- gsub("\\s*%.+$||\t", "", apa6_header_includes)

  header_includes <- c(header_includes, apa6_header_includes)

  if(
    ((!is.null(yaml_params$figsintext) & !isTRUE(yaml_params$figsintext)) ||
    (!is.null(yaml_params$floatsintext) & !isTRUE(yaml_params$floatsintext))) &&
    grepl("man", yaml_params$classoption)
  ) {
    header_includes <- c(
      header_includes
      , "\\DeclareDelayedFloatFlavor{ThreePartTable}{table}" # Make endfloat play with longtable
      # , "\\DeclareDelayedFloatFlavor{ltable}{table}" # Make endfloat play with lscape
      , "\\DeclareDelayedFloatFlavor{lltable}{table}" # Make endfloat play with lscape & longtable
      # Patch \efloat@iwrite to use \protected@write (bug in endfloat package < 2.6)
      # Solution found at https://tex.stackexchange.com/questions/144372/error-when-using-endfloat-with-unicode-characters/144425
      # Details at https://github.com/axelsommerfeldt/endfloat/blob/master/README#L58
      , "\\makeatletter"
      , "\\renewcommand{\\efloat@iwrite}[1]{\\expandafter\\immediate\\expandafter\\protected@write\\csname efloat@post#1\\endcsname{}}"
      , "\\makeatother"
    )
  }

  ### Additional options
  if(isTRUE(yaml_params$lineno)) {
    header_includes <- c(header_includes, "\\usepackage{lineno}\n\n\\linenumbers")
  }

  if(isTRUE(yaml_params$footnotelist)) {
    header_includes <- c(
      header_includes
      , "\\usepackage{endnotes}"
      , "\\let\\footnote\\endnote"
    )

    yaml_params$`include-after` <- c(yaml_params$`include-after`, "\\theendnotes")
  }

  if(isTRUE(yaml_params$lof) || isTRUE(yaml_params$figurelist) || isTRUE(yaml_params$lot) || isTRUE(yaml_params$tablelist)) {
    header_includes <- c(header_includes, "\\usepackage[titles]{tocloft}")
  }

  if(isTRUE(yaml_params$lof) || isTRUE(yaml_params$figurelist)) {
    yaml_params$lof <- FALSE
    yaml_params$figurelist <- TRUE

    header_includes <- c(
      header_includes
      , "\\cftpagenumbersoff{figure}"
      , "\\renewcommand{\\cftfigpresnum}{\\itshape\\figurename\\enspace}"
      , "\\renewcommand{\\cftfigaftersnum}{.\\space}"
      , "\\setlength{\\cftfigindent}{0pt}"
      , "\\setlength{\\cftafterloftitleskip}{0pt}"
      , "\\settowidth{\\cftfignumwidth}{Figure 10.\\qquad}"
    )

    yaml_params$`include-after` <- c(
      yaml_params$`include-after`
      , "\\clearpage"
      , "\\renewcommand{\\listfigurename}{Figure captions}"
      , "\\listoffigures"
    )
  }

  if(isTRUE(yaml_params$lot) || isTRUE(yaml_params$tablelist)) {
    yaml_params$lot <- FALSE
    yaml_params$tablelist <- TRUE

    header_includes <- c(
      header_includes
      , "\\cftpagenumbersoff{table}"
      , "\\renewcommand{\\cfttabpresnum}{\\itshape\\tablename\\enspace}"
      , "\\renewcommand{\\cfttabaftersnum}{.\\space}"
      , "\\setlength{\\cfttabindent}{0pt}"
      , "\\setlength{\\cftafterloftitleskip}{0pt}"
      , "\\settowidth{\\cfttabnumwidth}{Table 10.\\qquad}"
    )

    yaml_params$`include-after` <- c(
      yaml_params$`include-after`
      , "\\clearpage"
      , "\\renewcommand{\\listtablename}{Table captions}"
      , "\\listoftables"
    )
  }

  yaml_params$`header-includes` <- c(yaml_params$`header-includes`, header_includes)


  ## Add modified YAML header
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_text)
  augmented_input_text <- c("---", yaml::as.yaml(yaml_params), "---", input_text[(yaml_delimiters[2] + 1):length(input_text)])
  input_file_connection <- file(input_file, encoding = "UTF-8")
  writeLines(augmented_input_text, input_file_connection)
  close(input_file_connection)

  # Add pancod arguments
  args <- NULL

  if((!is.list(metadata$output) || is.null(metadata$output[[1]]$citation_package)) &
     (is.null(metadata$citeproc) || metadata$citeproc)) {

    # Set CSL
    args <- set_csl(input_file)

    # Set ampersand filter
    if((is.null(metadata$replace_ampersands) || metadata$replace_ampersands)) {
      args <- set_ampersand_filter(args, metadata$csl)
    }
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
  writeLines(augmented_input_text, input_file_connection)
  close(input_file_connection)

  # Add pancod arguments
  args <- NULL

  # Process markdown
  process_markdown <- utils::getFromNamespace("process_markdown", "bookdown")
  process_markdown(input_file, from, args, TRUE)

  if(is.null(metadata$citeproc) || metadata$citeproc) {

    # Set CSL
    args <- set_csl(input_file)

    # Set ampersand filter
    if((is.null(metadata$replace_ampersands) || metadata$replace_ampersands)) {
      args <- set_ampersand_filter(args, metadata$csl)
    }
  }

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


paste_authors <- function(x, format) {

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
  if(format == "latex") x[[n_authors]]$name <- gsub("\\&", "\\\\&", x[[n_authors]]$name)
  paste(unlist(lapply(x, "[[", "name")), collapse = "")
}

paste_affiliations <- function(x, format) {
  add_superscript <- function(y, format) {
    if(format == "latex") {
      superscript <- paste0("\\textsuperscript{", y["id"], "}")
    } else if(format == "word") {
      superscript <- paste0("^", y["id"], "^")
    }  else {
      stop("Format not supported.")
    }

    location <- c(y[["institution"]], y[["city"]], y[["state"]], y[["country"]])

    paste(
      superscript
      , paste(location, collapse = ", ")
      , collapse = " "
    )
  }

  affiliations <- vapply(x, add_superscript, format = format, FUN.VALUE = "a")
  if(format == "latex") {
    paste(affiliations, collapse = "\\\\")
  } else {
    affiliations
  }
}


set_ampersand_filter <- function(args, csl_file) {
  pandoc_citeproc <- utils::getFromNamespace("pandoc_citeproc", "rmarkdown")

  # Correct in-text ampersands
  filter_path <- system.file(
    "rmd", "ampersand_filter.sh"
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

  if(!is.null(args)) { # CSL has not been specified manually
    args <- c(args, "--filter", pandoc_citeproc(), "--filter", filter_path)
  } else {
    args <- c(args, "--csl", csl_file, "--filter", pandoc_citeproc(), "--filter", filter_path)
  }

  args
}
