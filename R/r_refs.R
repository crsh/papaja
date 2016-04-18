#' Create a reference file for R and its packages
#'
#' Creates a \code{.bib}-reference file for the installed R version and R-packages, so they can be cited
#' in a Markdown-document using pandoc or LaTeX.
#'
#' @param x Character. Names of packages to include in bibliography.
#' @param file Character. The path and name of the file to be created or updated.
#' @param append Logical. Indicates if existing \code{.bib}-files should be complemented or overwritten.
#'    See details.
#' @param prefix Character. A prefix to be used for all R-package reference handles.
#' @param type_pref Character. A vector of BibTeX entry types in the order to look for them in packages
#'    CITATION file. See details.
#' @param tweak Logical. Indicates whether to fix some known problems in the citations (based on
#'    \code{\link[knitr]{write_bib}}).
#' @details
#'    \code{r_refs} is a wrapper for \code{\link{create_bib}} to create a bibliography for R and all attached or
#'    cached packages.
#'
#'    By default, if a file exists at the specified location, \code{r_refs} reads the file and
#'    appends missing citation information to the end of the file (\code{\link{create_bib}} always overwrites
#'    existing files). This behavior may result in redundant package references in your \code{.bib}-file,
#'    which will be cited when using \code{\link{cite_r}}. It is recommended to use a \code{.bib}-file
#'    dedicated to R-references, setting \code{append = FALSE}, and adding the file to the
#'    \code{bibliography} parameter in the document's yaml-header.
#'
#'    Beware that chunks loading packags should \emph{not} be cached. \code{r_refs} will make all packages
#'    loaded in cached chunks citable, but it won't know when you remove a package from a cached chunk. This
#'    can result in unused package references in your \code{.bib}-file that will be cited when using
#'    \code{\link{cite_r}}.
#'
#'    If a package provides citation information in a CITATION file, a reference is selected based on the
#'    prefered order of reference types specified in \code{type_pref}. By default, articles are cited if
#'    available rather than books. If no reference of the specified types is available, the first reference
#'    is used. If multiple references of the prefered type are given in the CITATION file all are cited.
#'    Finally, if no CITATION file exists a reference is generated from the DESCRIPTION file by
#'    \code{\link{citation}}.
#' @seealso \code{\link{cite_r}}, \code{\link[knitr]{write_bib}}, \code{\link{fetch_web_refs}}, \code{\link{citation}}, \code{\link{toBibtex}}
#' @examples NULL
#' @export

r_refs <- function(file, append = TRUE, prefix = "R-", type_pref = c("Article", "Book"), tweak = TRUE) {
  validate(append, check_class = "logical", check_NA = TRUE, check_length = 1)

  r_session <- sessionInfo()

  # Ensure that cached packages are also citable
  cache_path <- knitr::opts_chunk$get("cache.path")
  if (!is.null(cache_path) && file_test("-d", cache_path)) {
    cached_pkgs <- readLines(paste0(cache_path, "__packages"))
    cached_pkgs <- setdiff(cached_pkgs, r_session$basePkgs)
    pkgs_to_cite <- unique(c(names(r_session$otherPkgs), cached_pkgs))
  } else {
    pkgs_to_cite <- names(r_session$otherPkgs)
    if(!is.null(pkgs_to_cite)) pkgs_to_cite <- sort(pkgs_to_cite)
  }

  pkg_list <- c("base", pkgs_to_cite)

  if(file_test("-f", file) && append) {
    bib_file <- readLines(file)
    missing_pkgs <- sapply(pkg_list, function(x) !any(grep(paste0(prefix, x), bib_file)))
    missing_pkgs <- names(missing_pkgs[missing_pkgs])
  } else {
    missing_pkgs <- pkg_list
  }

  if(length(missing_pkgs) > 0 || !append) {
    create_bib(missing_pkgs, file = file, prefix = prefix, type_pref = type_pref, tweak = tweak, append = append)
  }
}




#' @rdname r_refs
#' @export

create_bib <- function(x, file, append = TRUE, prefix = "R-", type_pref = c("Article", "Book"), tweak = TRUE) {
  validate(x, check_class = "character", check_NA = TRUE)
  validate(file, check_class = "character", check_NA = TRUE, check_length = 1)
  validate(prefix, check_class = "character", check_NA = TRUE, check_length = 1)
  validate(type_pref, check_class = "character", check_NA = TRUE)
  validate(tweak, check_class = "logical", check_NA = TRUE, check_length = 1)

  # Remove packages that are not installed
  missing_packages <- mapply(system.file, package = x) == ""
  if(any(missing_packages)) {
    warning("package(s) ", paste(x[missing_packages], collapse = ", "), " not found")
    x <- x[!missing_packages]
  }

  # Generate citation from DESCRIPTION if CITATION is missing
  no_citations <- mapply(system.file, "CITATION", package = x) == ""

  bib <- sapply(
    seq_along(x)
    , function(pkg) {
      cite <- citation(x[pkg], auto = if(no_citations[pkg]) TRUE else NULL)

      if(length(cite) > 1) {
        bibtypes <- unlist(cite$bibtype)

        pref_entry <- type_pref[tolower(type_pref) %in% tolower(bibtypes)][1]
        cite <- if(is.na(pref_entry)) cite[[1]] else cite[[bibtypes == pref_entry]]
      }

      if(tweak) {
        cite <- lapply(cite, function(cit) {
          cit$title = gsub(sprintf("^(%s: )(\\1)", pkg), "\\1", cit$title)
          cit$title = gsub(" & ", " \\\\& ", cit$title)
          cit
        })
      }

      entry <- lapply(cite, toBibtex)
      specifier <- if(length(entry) > 1) paste0("_", letters[seq_along(entry)]) else NULL
      entry <- sapply(seq_along(entry), function(ent) {
        entry[[ent]][1] <- sub("\\{,$", paste0("{", prefix, x[pkg], specifier[ent], ","), entry[[ent]][1])
        entry[[ent]]
      })
      entry
    }
    , simplify = FALSE
  )

  # if(tweak) {
  #   for (i in intersect(names(knitr:::.tweak.bib), x)) {
  #     message("tweaking ", i)
  #     bib[[i]] = merge_list(bib[[i]], knitr:::.tweak.bib[[i]])
  #   }
  #   bib <- lapply(bib, function(b) {
  #     b["author"] <- sub("Duncan Temple Lang", "Duncan {Temple Lang}", b["author"])
  #     if (!("year" %in% names(b))) b["year"] <- sprintf('  year = {%s},', format(Sys.Date(), '%Y'))
  #     idx <- which(names(b) == "")
  #     structure(c(b[idx[1L]], b[-idx], b[idx[2L]]), class = "Bibtex")
  #   })
  # }

  bib <- bib[sort(x, index.return = TRUE)$ix]
  if(!is.null(file)) cat(unlist(bib), sep = "\n", file = file, append = append)
  invisible(bib)
}
