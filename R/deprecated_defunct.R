#' Prepare APA document (deprecated)
#'
#' Prepares objects to be used in the rendering process and creates title page
#' and abstract for MS Word documents. *This function has been defunct. It is
#' no longer needed.*
#'
#' @details The function creates and locks a non-exported object `apa_lang`
#'   that is used by other \pkg{papaja}-functions. `apa_lang` is a
#'   `list` containing localizations for document elements such as abstract
#'   and title. The selected language is defined by the `lang`-parameter
#'   in the documents yaml-header. Currently, English (default) and German
#'   (`"german"`) are supported.
#'
#'   If the output document is MS Word (`output: \link{apa6_word}`) the
#'   function additionally creates a title page and adds the abstract. You
#'   should, therefore, always call `apa_prepare_doc` at the very
#'   beginning of the R Markdown document.
#' @seealso \code{\link{apa6_word}}
#' @export

apa_prepare_doc <- function() {
  .Deprecated(msg = "'apa_prepare_doc' is deprecated as of version 0.1.0.9423 because it is no longer needed. You may remove it from your manuscript without consequences.")
  return(invisible(NULL))
}



#' Fetch a .bib-reference file from the web (defunct)
#'
#' Downloads and saves a `.bib`-reference file form the web, so it can be
#' used to cite references in a Markdown-document using pandoc or LaTeX.
#' *This function has been defunct. Please use \code{download} from the
#' \pkg{downloader} instead.*
#'
#' @param x Character. URL of the \code{.bib}-file to fetch.
#' @param bib_name Character. The path and name of the file to be created.
#' @details
#'   If the function is called in an RMarkdown-document the file name
#'   specified as \code{bib_name} can be used in the YAML header as
#'   \code{bibliography}.
#' @seealso \code{\link{cite_r}}, \code{\link{r_refs}},
#'   \code{\link[knitr]{write_bib}}
#' @examples NULL
#' @export

fetch_web_refs <- function(x, bib_name) {
  .Defunct("downloader::download")
  return(invisible(NULL))
}


#' Save a collection from a Zotero-Account to a BibTeX-file (defunct)
#'
#' Downloads and saves a Zotero reference library (or a subset) and saves it as
#' BibTeX file. *This function has been defunct. Use `ReadZotero()` from the
#' \pkg{RefManageR} package instead.*
#'
#' @param x Character. Zotero user or group ID, see details.
#' @param lib_type Character. Specifies if the supplied ID is associated with a
#'   Zotero `user` or `group`.
#' @param collection Character. Optional ID of a collection in the Zotero
#'   library, see details.
#' @param API_key Character. Zotero API key, see details.
#' @param bib_name Character. Name of the BibTeX-file references are saved to.
#'
#' @details
#'  This function retrieves references through the Zotero web API. `x`
#'  takes a Zotero user or group ID that can be retrieved from the Zotero.org
#'  user or group Feeds/API settings.
#'  An authentication key (`API_key`) is required to access nonpublic
#'  Zotero libraries. Authentication keys can also be generated in the
#'  Zotero.org user or group Feeds/API settings.
#'
#'  If the requested reference collection is larger than 100 records, multiple
#'  API calls are initiated because the number of retrieved records is limited
#'  to 100 per API call. Frequent API calls will result in a temporary access
#'  block. Thus, there is an (currently unknown) upper limit to the length of
#'  reference collections that can be retrieved through this function. It is
#'  generally recommended to comment out calls to this function in R
#'  Markdown documents during periods of frequent knitting to limit the number
#'  of API calls and limit the number of references to those needed for the
#'  current document by setting up collections in your Zotero library.
#'
#'  Collection keys (\code{collection}), i.e. identifiers of reference library
#'  subsets, can be retrieved by accessing them via a web browser. They keys
#'  are contained in the URL:
#'
#' `https://www.zotero.org/<USERNAME>/items/collectionKey/<COLLECTIONKEY>`
#'
#'  Zotero web API calls can be slow, especially for large reference
#'  collections. If available, this function will use the
#'  \pkg{downloader}-package, which speeds up reference downloads considerably.
#'
#' @author Christoph Stahl, Frederik Aust
#' @return Returns \code{bib_name} invisibly.
#'
#' @seealso \code{\link{r_refs}}, \code{\link{cite_r}}
#'
#' @examples NULL

fetch_zotero_refs <- function(
  x
  , bib_name
  , API_key = NULL
  , collection = NULL
  , lib_type = "user"
) {
  .Defunct("citr::fetch_zotero_refs")
  return(invisible(NULL))
}


apa_table.word <- function(x, ...) {
  .Deprecated(msg = "'apa_table.word' is deprecated as of version 0.1.0.9793 because it was more accurately renamed to 'apa_table.markdown'.")

  apa_table.markdown(x, ...)
}


deprecate_ci <- function(conf.int, ...) {
  x <- list(...)

  partial_matches <- pmatch(names(x), table = c("ci", "conf.level", "args_confint"), duplicates.ok = TRUE)
  names(partial_matches) <- c("ci", "conf.level", "args_confint")[partial_matches]
  x_deprecated <- x[!is.na(partial_matches)]
  names(x_deprecated) <- names(partial_matches[!is.na(partial_matches)])
  if(length(x_deprecated) > 1L) {
    stop(
      "Using arguments "
      , paste(encodeString(names(x_deprecated), quote = "'"), collapse = " and ")
      , " in calls to 'apa_print()' is deprecated. Please use 'conf.int' instead. "
      , "Your call to 'apa_print()' failed because conflicting deprecated arguments were provided."
      , call. = FALSE
    )
  }

  if(length(x_deprecated)) {
    warning(
      "Using argument "
      , encodeString(names(x_deprecated), quote = "'")
      , " in calls to 'apa_print()' is deprecated. "
      , "Please use 'conf.int' instead."
      , call. = FALSE
    )
    conf.int <- x_deprecated[[1L]]
  }
  list(
    conf.int = conf.int
    , ellipsis = x[is.na(partial_matches)]
  )
}




prettify_terms <- function(...) {
  .Defunct("beautify_terms")
  beautify_terms(...)
}


#' Render Appendix
#'
#' This function renders an R Markdown document *without* YAML header to a TeX
#' fragment inside an `appendix` environment, or to a markdown fragment (for
#' Word output). *This function has been defunct. Please use the appendix
#' syntax provided by \pkg{bookdown} (see the [bookdown manual](https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#special-headers)).*
#'
#' @param x Character. Path to R Markdown file.
#' @param bibliography Character. Paths to relevant bibliography file(s).
#' @param csl Character. Path to CSL file to use. Defaults to APA-style.
#' @param quiet Logical. Suppresses pandoc command line output; see \code{\link[rmarkdown]{render}}.
#'    If `FALSE` output will be included in the document.
#' @inheritDotParams rmarkdown::pandoc_convert
#' @details
#'   **This function is only exported for backwards compatibility.**
#'   It is now recommended *not* to call `render_appendix()` directly.
#'   Instead, to add appendices to your manuscript, add the R Markdown file
#'   to the YAML front matter by using `appendix: "appendix.Rmd"`.
#'
#'   Default chunk options and hooks are set to those used in the R Markdown
#'   document from which `render_appendix` is called; otherwise defaults of
#'   \code{\link[rmarkdown]{md_document}} are used.
#'
#'   By default, `x` is converted to a TeX file, which can be included in an
#'   R Markdown document as \code{include}:
#'
#'   \preformatted{
#'   output:
#'     pdf_document:
#'       include:
#'         after_body: appendix.tex
#'   }
#'
#'   If \code{render_appendix} is called form an R Markdown document with a
#'   target document type other than a PDF file, a markdown fragment is
#'   included.
#' @keywords internal
#' @export

render_appendix <- function(
  x
  , bibliography = rmarkdown::metadata$bibliography
  , csl = rmarkdown::metadata$csl
  , quiet = TRUE
  , ...
) {
  .Defunct(msg = "'render_appendix' is defunct as of version 0.1.0.9998 because it is no longer needed. Please use the appendix syntax provided by {bookdown}: https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#special-headers")

  validate(x, check_class = "character", check_length = 1)
  if(!is.null(csl)) validate(csl, check_class = "character", check_length = 1)
  validate(quiet, check_class = "logical", check_length = 1)

  if(length(bibliography) > 0) {
    validate(bibliography, check_class = "character")
    bibliography <- sapply(bibliography, tools::file_path_as_absolute)
    existing_bibliographies <- bibliography[file.exists(bibliography)]
    if(length(bibliography) > length(existing_bibliographies)) {
      warning(
        paste(
          "The following bibliography files could not be located:"
          , bibliography[!bibliography %in% existing_bibliographies]
          , sep = "\n"
          , collapse = "\n"
        )
      )
    }
    if(length(existing_bibliographies) > 0) {
      bib_call <- paste0("--bibliography=", existing_bibliographies)
    }
  } else {
    bib_call <- NULL
  }

  if(
    is.list(rmarkdown::metadata$output) &&
    is.list(rmarkdown::metadata$output[[1]]) &&
    !is.null(rmarkdown::metadata$output[[1]]$citation_package)
  ) {
    citation_package_call <- paste0(
      "--", rmarkdown::metadata$output[[1]]$citation_package
    )
  } else {
    citation_package_call <- NULL
  }

  if(is.null(citation_package_call)) {
    if(!is.null(bib_call) & length(csl) > 0) {
      validate(csl, check_class = "character", check_length = 1)
    } else {
      csl <- system.file(
        "rmd", "apa6.csl"
        , package = "papaja"
      )
    }

    csl_call <- paste0("--csl=", rmarkdown::pandoc_path_arg(tools::file_path_as_absolute(csl)))
  } else {
    bib_call <- gsub("^--", "-M ", bib_call)
    csl_call <- NULL
  }

  ellipsis <- list(...)
  ellipsis$options <- c(
    ellipsis$options
    , bib_call
    , csl_call
    , citation_package_call
  )

  validate(quiet, check_class = "logical", check_length = 1)

  target_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(length(target_format) == 0L) stop("render_appendix() can only be used within an R Markdown document; please include the function call in a code chunk.")


  if(!target_format %in% c("latex", "word", "docx")) {
    warning(
      target_format
      , " documents currently do not support appendices via includes."
    )
  }

  # Render Markdown file ----
  md_file <- tempfile(fileext = ".md")
  res <- knitr::knit_child(input = x, output = md_file, quiet = quiet)
  md_fragment <- readLines_utf8(md_file)

  if(target_format == "latex") {

    # Create TeX-file ----
    tex_file <- paste0(tools::file_path_sans_ext(normalizePath(x)), ".tex")

    ellipsis$input    <- md_file
    ellipsis$output   <- tex_file
    ellipsis$citeproc <- is.null(citation_package_call)

    status <- do.call(rmarkdown::pandoc_convert, ellipsis)


    # Add appendix environment ----
    tex <- readLines_utf8(con = tex_file)

    # Check whether Rmd starts with heading, otherwise add empty section ----
    # when checking, ignore rows with latex newlines or html comments
    if(!grepl(pattern = "^#(\\b|\\s)", x = md_fragment[!grepl("^\\\\|^<!--", md_fragment) & md_fragment != ""][1])) {
      tex <- c("\\section{}", tex)
    }

    appendix_endfloat_fix <- ifelse(
      any(grepl("man", c(rmarkdown::metadata$classoption, rmarkdown::metadata$class)))
      , "\\makeatletter\n\\efloat@restorefloats\n\\makeatother"
      , ""
    )
    tex <- c("\\clearpage", appendix_endfloat_fix, "\n\n\\begin{appendix}", tex, "\\end{appendix}")

    writeLines(tex, con = tex_file, useBytes = TRUE)

    if(!is.null(status)) return(status)
  } else if(target_format %in% c("word", "docx")) {
    cat(c("", md_fragment), sep = "\n")
  }

  return(invisible(0))
}

#' Create a Regression Table
#'
#' These methods take \code{glm} or \code{lm} objects to create
#' a data frame containing a regression table.
#' *This function has been defunct. It is
#' no longer needed.*
#'
#' @param x \code{glm} or \code{lm} object. See details.
#' @inheritParams apa_print.glm
#'
#' @return
#'    \code{data.frame} of class \code{apa_regression_table}.
#'
#' @keywords internal

arrange_regression <- function(x, est_name, standardized, conf.int, ...) {
  .Defunct(msg = "arrange_regression() is defunct because it is no longer needed.")
  invisible(NULL)
}
