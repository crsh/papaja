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
#' @seealso \code{\link[bookdown]{html_document2}}, \code{\link[rmarkdown]{pdf_document}}, \code{\link[rmarkdown]{word_document}}
#' @examples NULL
#' @export

apa6_pdf <- function(
  fig_caption = TRUE
  , number_sections = FALSE
  , toc = FALSE
  , keep_tex = TRUE
  , md_extensions = NULL
  , includes = NULL
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)
  validate(number_sections, check_class = "logical", check_length = 1)
  validate(toc, check_class = "logical", check_length = 1)
  validate(keep_tex, check_class = "logical", check_length = 1)
  if(!is.null(includes)) {
    validate(includes, check_class = "list")
  } else {
    includes <- rmarkdown::includes()
  }

  apa6_header_includes <-  system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6_header_includes.tex"
    , package = "papaja"
  )
  if(apa6_header_includes == "") stop("LaTeX header includes file not found.")

  includes$in_header <- c(includes$in_header, apa6_header_includes)

  if(is.null(md_extensions) || !grepl("raw\\_attribute", md_extensions)) {
    md_extensions <- paste0(md_extensions, "+raw_attribute")
  }

  # Call pdf_document() with the appropriate options
  config <- bookdown::pdf_document2(
    fig_caption = fig_caption
    , number_sections = number_sections
    , toc = toc
    , keep_tex = keep_tex
    , md_extensions = md_extensions
    , includes = includes
    , ...
  )

  # Set chunk defaults
  config$knitr$opts_chunk$echo <- FALSE
  config$knitr$opts_chunk$message <- FALSE
  config$knitr$opts_chunk$fig.cap <- " " # Ensures that figure environments are added
  config$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  config$knitr$knit_hooks$inline <- inline_numbers

  config$knitr$opts_chunk$dev <- c("pdf", "png") # , "postscript", "tiff"
  config$knitr$opts_chunk$dpi <- 300
  config$clean_supporting <- FALSE # Always keep images files

  config$pre_knit <- function(input, ...) { modify_input_file(input=input, format = "papaja::apa6_pdf") }

  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  config$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)

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

  config$post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    output_text <- readLines(output_file, encoding = "UTF-8")

    # Remove indentation so that endfloat can process the lltable environments
    appendix_lines <- grep("\\\\(begin|end)\\{appendix\\}", output_text)
    if(length(appendix_lines) == 2) {
      output_text[appendix_lines[1]:appendix_lines[2]] <- gsub(
        "^\\s+"
        , ""
        , output_text[appendix_lines[1]:appendix_lines[2]]
        , useBytes = TRUE
      )
    }

    # Correct abstract and note environment
    ## Note is added to the end of the document by Lua filter and needs to be
    ## moved to the preamble
    lua_addition_start <- which(grepl("^% papaja Lua-filter additions$", output_text))
    lua_addition_end <- which(grepl("^% End of papaja Lua-filter additions$", output_text))

    if(lua_addition_end - lua_addition_start > 1) {
      header_additions <- output_text[c((lua_addition_start + 1):(lua_addition_end - 1))]
      output_text <- output_text[-c(lua_addition_start:lua_addition_end)]
      begin_doc <- which(output_text == "\\begin{document}")
      output_text <- c(
        output_text[1:(begin_doc-1)]
        , header_additions
        , output_text[begin_doc:length(output_text)]
      )
    }
    output_text <- paste(output_text, collapse = "\n")

    output_text <- gsub(
      "\\\\begin\\{document\\}\n\\\\maketitle\n\\\\begin\\{abstract\\}(.+)\\\\end\\{abstract\\}"
      , paste0(
        "\\\\abstract{\\1}\n\n"
        , "\n\n\\\\begin\\{document\\}\n\\\\maketitle"
      )
      , output_text
      , useBytes = TRUE
    )

    # Remove abstract environment if empty
    output_text <- gsub("\\\\abstract\\{\n\n\\}", "", output_text, useBytes = TRUE)

    # Remove pandoc listof...s
    if(sum(gregexpr("\\listoffigures", output_text, fixed = TRUE)[[1]] > 0)) {
      output_text <- sub("\\\\listoffigures", "", output_text, useBytes = TRUE) # Replace first occurance
    }
    if(sum(gregexpr("\\listoftables", output_text, fixed = TRUE)[[1]] > 0)) {
      output_text <- sub("\\\\listoftables", "", output_text, useBytes = TRUE) # Replace first occurance
    }

    # Prevent (re-)loading of geometry package
    output_text <- gsub("\\\\usepackage\\[?.*\\]?\\{geometry\\}", "", output_text, useBytes = TRUE)


    output_file_connection <- file(output_file)
    on.exit(close(output_file_connection))
    writeLines(output_text, output_file_connection, useBytes = TRUE)

    # Apply bookdown postprocesser and pass format options
    bookdown_post_processor <- bookdown::pdf_document2()$post_processor
    pp_env <- environment(bookdown_post_processor)
    assign("post", NULL, envir = pp_env) # Postprocessor is not self-contained
    assign("config", config, envir = pp_env) # Postprocessor is not self-contained
    bookdown_post_processor(metadata = metadata, input = input_file, output = output_file, clean = clean, verbose = verbose)
  }

  if(Sys.info()["sysname"] == "Windows") {
    config$on_exit <- function() {
      revert_original_input_file(2)
    }
  } else {
    config$on_exit <- revert_original_input_file
  }

  config
}


#' @describeIn apa6_pdf Format to create .docx-files. \code{class} parameter is ignored. \emph{This function
#'    should be considered experimental.}
#' @export

apa6_docx <- function(
  fig_caption = TRUE
  # , pandoc_args = NULL
  , md_extensions = NULL
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)

  # Get APA6 reference file
  ellipsis <- list(...)
  if(is.null(ellipsis$reference_docx)) {
    ellipsis$reference_docx <- system.file(
      "rmarkdown", "templates", "apa6", "resources"
      , "apa6_man.docx"
      , package = "papaja"
    )
    if(ellipsis$reference_docx == "") stop("No .docx-reference file found.")
  }


  # Call word_document() with the appropriate options
  config <- do.call(
    bookdown::word_document2
    , c(
      fig_caption = fig_caption
      # , pandoc_args = pandoc_args
      , md_extensions = md_extensions
      , ellipsis
    )
  )

  # Set chunk defaults
  config$knitr$opts_chunk$echo <- FALSE
  config$knitr$opts_chunk$message <- FALSE
  config$knitr$opts_knit$rmarkdown.pandoc.to <- "docx"
  config$knitr$knit_hooks$inline <- inline_numbers
  # config$knitr$knit_hooks$plot <- function(x, options) {
  #   options$fig.cap <- paste("*", getOption("papaja.terms")$figure, ".* ", options$fig.cap)
  #   knitr::hook_plot_md(x, options)
  # }

  config$knitr$opts_chunk$dev <- c("png", "pdf") #, "svg", "tiff")
  config$knitr$opts_chunk$dpi <- 300
  config$clean_supporting <- FALSE # Always keep images files

  config$pre_knit <- function(input, ...) { modify_input_file(input=input, format="papaja::apa6_docx") }

  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL
  .from <- rmarkdown::from_rmarkdown(fig_caption, md_extensions)

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  config$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from = .from) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- word_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from)

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

  config$post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    # Add correct running head
    docx_files <- zip::zip_list(zipfile = output_file)$filename

    if(!is.null(metadata$shorttitle)) {
      running_head <- metadata$shorttitle
    } else {
      running_head <- metadata$title
    }

    zip::unzip(zipfile = output_file)
    on.exit(
      unlink(c("[Content_Types].xml", "_rels", "word", "docProps"), recursive = TRUE)
    )

    for(i in paste0("word/header", 2:3, ".xml")) {
      xml <- readLines(i, warn = FALSE)
      xml <- gsub("TITLE", toupper(running_head), xml, useBytes = TRUE)
      i_con <- file(i)
      writeLines(xml, con = i_con, useBytes = TRUE)
      close(i_con)
    }

    zip::zipr(
      zipfile = output_file
      , files = c("[Content_Types].xml", "_rels", "word", "docProps")
      , recurse = TRUE
      , include_directories = FALSE
    )
  }

  if(Sys.info()["sysname"] == "Windows") {
    config$on_exit <- function() {
      revert_original_input_file(2)
    }
  } else {
    config$on_exit <- revert_original_input_file
  }

  config
}

#' @describeIn apa6_pdf Format to create .docx-files. Alias of \code{apa6_docx}.
#' @export
apa6_word <- apa6_docx


# Set hook to print default numbers
inline_numbers <- function (x) {

  if(inherits(x, "difftime")) x <- as.numeric(x)
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
    x <- printnum(x, numerals = x > 10)
  } else if(is.character(x)) {
    x
  } else {
    paste(as.character(x), collapse = ', ')
  }
}


# Preprocessor functions are adaptations from the RMarkdown package
# (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)

set_default_csl <- function(x) {
  # Use APA6 CSL citations template if no other file is supplied
  has_csl <- function(text) {
    length(grep("^csl\\s*:.*$", text)) > 0
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

  parse_metadata_filter <- system.file(
    "lua", "parse_metadata.lua"
    , package = "papaja"
  )
  args <- rmdfiltr::add_custom_filter(args, filter_path = parse_metadata_filter, lua = TRUE)

  if(isTRUE(metadata$quote_labels)) {
    label_quotes_filter <- system.file(
      "lua", "label_quotes.lua"
      , package = "papaja"
    )
    args <- rmdfiltr::add_custom_filter(args, filter_path = label_quotes_filter, lua = TRUE)
  }

  ## Set template variables and defaults
  if(is.null(metadata$documentclass)) {
    args <- c(args, "--variable", "documentclass:apa6")
  }

  if(!is.null(metadata[["class"]])) { # Depricated class options
    classoption <- paste(metadata[["class"]], collapse = ",")
    metadata$classoption <- paste(paste(metadata$classoption, collapse = ","), classoption, sep = ",")
  } else if(is.null(metadata$classoption)) {
    metadata$classoption <- "man"
  }

  if(isTRUE(metadata$mask)) metadata$classoption <- paste0(metadata$classoption, ",mask")

  if(isTRUE(metadata$figsintext) || isTRUE(metadata$floatsintext)) {
    metadata$classoption <- paste0(metadata$classoption, ",floatsintext")
  }

  if(isTRUE(metadata$draft)) metadata$classoption <- paste0(metadata$classoption, ",draftall")

  args <- c(args, "--variable", paste0("classoption:", metadata$classoption))

  if (is.null(metadata$lang)) {
    lang_tag <- "en-EN"
  } else { # Depricated default lang options in papaja templates
    lang_tag <- switch(
      metadata$lang
      , english = "en-EN"
      , american = "en-US"
      , metadata$lang
    )
  }

  args <- c(args, "--variable", paste0("lang:", lang_tag))

  if(is.null(metadata$title)) {
    args <- c(args, "--variable", "title:TITLE")
  }

  ## Surpresses redefinitino of paragraph and subparagraph
  if(is.null(metadata$subparagraph)) { # For compatibility with older pandoc versions
    args <- c(args, "--variable", "subparagraph:yes")
  }

  if(is.null(metadata$`block-headings`)) {
    args <- c(args, "--variable", "block-headings:no")
  }


  # Add necessary includes
  header_includes <- c()
  after_body_includes <- c()
  before_body_includes <- c()


  ## Essential manuscript parts
  if(!is.null(metadata$shorttitle)) {
    short_title <- paste0("\\shorttitle{", escape_latex(metadata$shorttitle), "}")
  } else {
    short_title <- paste0("\\shorttitle{SHORTTITLE}")
  }
  header_includes <- c(header_includes, short_title)

  if(!is.null(metadata$leftheader)) {
    header_includes <- c(header_includes, paste0("\\leftheader{", escape_latex(metadata$leftheader), "}"))
  }

  if(!is.null(metadata$keywords) || !is.null(metadata$wordcount)) {
    keywords <- paste(unlist(metadata$keywords), collapse = ", ")
    if(!is.null(metadata$wordcount)) {
      keywords <- paste0(keywords, "\\newline\\indent Word count: ", metadata$wordcount)
    }
    header_includes <- c(header_includes, paste0("\\keywords{", keywords, "}"))
  }

  ## Manuscript and table formatting

  if(
    ((!is.null(metadata$figsintext) & !isTRUE(metadata$figsintext)) ||
     (!is.null(metadata$floatsintext) & !isTRUE(metadata$floatsintext))) &&
    grepl("man", metadata$classoption)
  ) {
    header_includes <- c(
      header_includes
      , "\\DeclareDelayedFloatFlavor{ThreePartTable}{table}" # Make endfloat play with longtable
      # , "\\DeclareDelayedFloatFlavor{ltable}{table}" # Make endfloat play with lscape
      , "\\DeclareDelayedFloatFlavor{lltable}{table}" # Make endfloat play with lscape & longtable
      , "\\DeclareDelayedFloatFlavor*{longtable}{table}" # Make endfloat play with ordinary longtable (for kableExtra)
      # Patch \efloat@iwrite to use \protected@write (bug in endfloat package < 2.6)
      # Solution found at https://tex.stackexchange.com/questions/144372/error-when-using-endfloat-with-unicode-characters/144425
      # Details at https://github.com/axelsommerfeldt/endfloat/blob/master/README#L58
      , "\\makeatletter"
      , "\\renewcommand{\\efloat@iwrite}[1]{\\immediate\\expandafter\\protected@write\\csname efloat@post#1\\endcsname{}}"
      # , "`\\renewcommand{\\efloat@iwrite}[1]{\\immediate\\expandafter\\protected@write\\csname efloat@post#1\\endcsname{}}`{=latex}"
      , "\\makeatother"
    )
  }

  ## Additional options
  # Enable placement for table star environment
  if(any(grepl("jou", c(metadata$classoption, metadata$class)))) {
    header_includes <- c(header_includes, "\\usepackage{dblfloatfix}\n\n")
  }

  if(isTRUE(metadata$lineno) || isTRUE(metadata$linenumbers) ) {
    header_includes <- c(header_includes, "\\usepackage{lineno}\n\n\\linenumbers")
  }
  # Add after lineno to avoid LaTeX warning
  # https://tex.stackexchange.com/questions/447006/lineno-package-in-latex-causes-warning-message
  header_includes <- c(header_includes, "\\usepackage{csquotes}")


  if(!is.null(metadata$geometry)) {
    header_includes <- c(header_includes, paste0("\\geometry{", metadata$geometry, "}\n\n"))
  }

  if(isTRUE(metadata$footnotelist)) {
    header_includes <- c(
      header_includes
      , "\\usepackage{endnotes}"
      , "\\let\\footnote\\endnote"
    )

    after_body_includes <- c(after_body_includes, "\\clearpage", "\\theendnotes")
  }

  if(isTRUE(metadata$lof) || isTRUE(metadata$figurelist) || isTRUE(metadata$lot) || isTRUE(metadata$tablelist)) {
    header_includes <- c(header_includes, "\\usepackage[titles]{tocloft}")
  }

  if(isTRUE(metadata$lof) || isTRUE(metadata$figurelist)) {
    header_includes <- c(
      header_includes
      , "\\cftpagenumbersoff{figure}"
      , "\\renewcommand{\\cftfigpresnum}{\\itshape\\figurename\\enspace}"
      , "\\renewcommand{\\cftfigaftersnum}{.\\space}"
      , "\\setlength{\\cftfigindent}{0pt}"
      , "\\setlength{\\cftafterloftitleskip}{0pt}"
      , "\\settowidth{\\cftfignumwidth}{Figure 10.\\qquad}"
    )

    after_body_includes <- c(
      after_body_includes
      , "\\clearpage"
      , "\\renewcommand{\\listfigurename}{Figure captions}"
      , "\\listoffigures"
    )
  }

  if(isTRUE(metadata$lot) || isTRUE(metadata$tablelist)) {

    header_includes <- c(
      header_includes
      , "\\cftpagenumbersoff{table}"
      , "\\renewcommand{\\cfttabpresnum}{\\itshape\\tablename\\enspace}"
      , "\\renewcommand{\\cfttabaftersnum}{.\\space}"
      , "\\setlength{\\cfttabindent}{0pt}"
      , "\\setlength{\\cftafterloftitleskip}{0pt}"
      , "\\settowidth{\\cfttabnumwidth}{Table 10.\\qquad}"
    )

    after_body_includes <- c(
      after_body_includes
      , "\\clearpage"
      , "\\renewcommand{\\listtablename}{Table captions}"
      , "\\listoftables"
    )
  }

  tmp_includes_file <- function(x) {
    tmp_file <- tempfile(pattern = "includes_", tmpdir = tempdir(), fileext = ".tex")
    writeLines(x, con = tmp_file)
    tmp_file
  }

  header_includes <- c(header_includes, metadata$`header-includes`)
  if(length(header_includes) > 0) {
    args <- c(args, "--include-in-header", tmp_includes_file(header_includes))
  }

  before_body_includes <- c(before_body_includes, metadata$`before-includes`)
  if(length(before_body_includes) > 0) {
    args <- c(args, "--include-before", tmp_includes_file(before_body_includes))
  }

  after_body_includes <- c(after_body_includes, metadata$`after-includes`)
  if(length(after_body_includes) > 0) {
    args <- c(args, "--include-after", tmp_includes_file(after_body_includes))

  }


  ## Add appendix
  if(!is.null(metadata$appendix)) {
    appendices <- sapply(metadata$appendix, function(x) tools::file_path_sans_ext(tools::file_path_as_absolute(x)))
    args <- c(args, paste0("--include-after-body=", appendices, ".tex"))
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
  # input_file_connection <- file(input_file)
  # on.exit(close(input_file_connection))
  # writeLines(augmented_input_text, input_file_connection, useBytes = TRUE)
  replace_yaml_front_matter(yaml_params, augmented_input_text, input_file)

  # Add pandoc arguments
  args <- NULL

  # Process markdown
  process_markdown <- utils::getFromNamespace("process_markdown", "bookdown")
  process_markdown(input_file, from, args, TRUE)

  if(is.null(metadata$citeproc) || metadata$citeproc) {

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

  # Set additional lua filters
  args <- rmdfiltr::add_wordcount_filter(args, error = FALSE)

  docx_fixes_lua <-  system.file(
    "lua", "docx_fixes.lua"
    , package = "papaja"
  )
  if(docx_fixes_lua == "") stop("docx_fixes Lua-filter not found.")

  args <- rmdfiltr::add_custom_filter(args, filter_path = docx_fixes_lua, lua = TRUE, error = FALSE)

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

replace_yaml_front_matter <- function(x, input_text, input_file) {
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_text)
  augmented_input_text <- c("---", yaml::as.yaml(x), "---", input_text[(yaml_delimiters[2] + 1):length(input_text)])


  input_file_connection <- file(input_file)
  on.exit(close(input_file_connection))
  writeLines(augmented_input_text, input_file_connection, useBytes = TRUE)
}


modify_input_file <- function(input, format) {
  input_connection <- file(input, encoding = "UTF-8")
  on.exit(close.connection(input_connection))
  input_text <- readLines(con = input_connection)

  yaml_params <- get_yaml_params(input_text)

  if(!is.null(yaml_params$appendix)) {
    hashed_name <- paste0(base64enc::base64encode(charToRaw(basename(input))), ".Rmd")

    if(!file.copy(input, file.path(dirname(input), hashed_name))) {
      stop(paste0("Could not create a copy of the original input file '", input, "' while trying to render the appendix."))
    } else {
      # Add render_appendix()-chunk
      for(i in seq_along(yaml_params$appendix)) {
        input_text <- c(
          input_text
          , if(format %in% c("papaja::apa6_word", "papaja::apa6_docx")) {
            paste0(
              "<div custom-style='h1-pagebreak'>Appendix "
              , if(length(yaml_params$appendix) > 1) LETTERS[i] else NULL
              , "</div>"
            )
          } else NULL
          , ""
          , "```{r echo = FALSE, results = 'asis', cache = FALSE}"
          , paste0("papaja::render_appendix('", yaml_params$appendix[i], "')")
          , "```"
          , ""
        )
      }

      writeLines(input_text, input_connection, useBytes = TRUE)
    }
  }

  return(NULL)
}

revert_original_input_file <- function(x = 1) {
  # Get name of input file from render() because nothing is passed into on_exit()
  input_file <- get("original_input", envir = parent.frame(x))
  input_file <- tools::file_path_as_absolute(input_file)

  hashed_name <- paste0(base64enc::base64encode(charToRaw(basename(input_file))), ".Rmd")
  hashed_path <- file.path(dirname(input_file), hashed_name)

  if(file.exists(hashed_path)) {

    if(!file.copy(hashed_path, input_file, overwrite = TRUE)) {
      stop(paste0("Could not revert modified input file to original input file after trying to render the appendix. The file '", basename(input_file), "' has been modified. A copy of the orignal input file named '", hashed_name, "' has been saved in the same directory."))
    } else {
      unlink(hashed_path)
    }
  }

  return(NULL)
}
