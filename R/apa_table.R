#' Prepare table for printing
#'
#' Formats \code{matrices} and \code{data.frames} to report them as tables according to APA guidelines
#' (6th edition).
#' @param x Object to print, can be \code{matrix}, \code{data.frame}, or \code{list}. See details.
#' @param caption Character. Caption to be printed above the table.
#' @param note Character. Note to be printed below the table.
#' @param placement Character. Indicates whether table should be placed at the exact location (\code{h}),
#'    at the top (\code{t}), bottom (\code{b}), or on a separate page (\code{p}). Arguments can be combined
#'    to indicate order of preference (\code{htb}); ignored in MS Word documents.
#' @param landscape Logical. If \code{TRUE} the table is printed in landscape format; ignored in MS Word
#'    documents.
#' @param added_colnames Character. Vector of names for first unnamed columns. See details.
#' @param ... Further arguments to pass to \code{\link[knitr]{kable}}.
#'
#' @details
#'    When using \code{apa_table()}, the type of the ouput (LaTeX or MS Word) is determined automatically
#'    by the rendered document type. If no rendering is in progress the output default is LaTeX.
#'
#'    If \code{x} is a \code{list}, all list elements are merged by columns into a single table with
#'    the first column giving the names of the list elements elements.
#'
#'    If the first column(s) of the table are unnamed, names for these columns can be supplied using the
#'    \code{added_colnames} parameter. This can be done, e.g., when an object has rownames (unless
#'    \code{row.names = FALSE} is passed to \code{\link[knitr]{kable}}) and when elements of a \code{list} are
#'    merged.
#' @seealso \code{\link[knitr]{kable}}
#' @examples
#'
#' my_table <- apply(cars, 2, function(x) # Create data
#'    round(c(Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)), 2)
#' )
#'
#' apa_table(
#'    my_table
#'    , align = c("l", "r", "r")
#'    , caption = "A summary table of the cars dataset."
#'    , note = "This table was created using apa\\_table()"
#'    , added_colnames = "Descriptives"
#' )
#' @export

apa_table <- function(...) {
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(length(output_format) == 0) output_format <- "latex"
  if(output_format == "latex") {
    apa_table.latex(...)
  } else {
    apa_table.word(...)
  }
}

#' @rdname apa_table
#' @export

apa_table.latex <- function(
  x
  , caption = NULL
  , note = NULL
  , placement = "tbp"
  , landscape = FALSE
  , added_colnames = NULL
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")
  if(!is.null(caption)) validate(caption, check_class = "character", check_length = 1)
  if(!is.null(note)) validate(note, check_class = "character", check_length = 1)
  validate(placement, check_class = "character", check_length = 1)
  validate(landscape, check_class = "logical")
  if(!is.null(added_colnames)) validate(added_colnames, check_class = "character")

  if(landscape) table_env <- "sidewaystable" else table_env <- "table"
  cat("\\begin{", table_env, "}[", placement, "]\n\\centering\n\\begin{threeparttable}\n\\caption{", caption, "}", sep = "")

  if(is.list(x) && !is.data.frame(x)) {
    tables_to_merge <- names(x)
    prep_table <- lapply(seq_along(x), function(i) {
      prep_table <- cbind(
        c(tables_to_merge[i], rep("", nrow(x[[i]])-1))
        , rownames(x[[i]])
        , x[[i]]
      )
      rownames(prep_table) <- NULL
      second_col <- ifelse(is.null(rownames(x[[i]])), NULL, "")
      if(is.null(added_colnames)) {
        colnames(prep_table) <- c("", "", colnames(x[[i]]))
      } else {
        colnames(prep_table) <- c(added_colnames, colnames(x[[i]]))
      }

      prep_table
    }
    )
    x_merged <- do.call(rbind, prep_table)
    print(knitr::kable(x_merged, format = "latex", booktabs = TRUE, ...))
  } else {
    prep_table <- cbind(rownames(x), x)
    if(!is.null(added_colnames)) colnames(prep_table) <- c(added_colnames, colnames(x))
    rownames(prep_table) <- NULL
    print(knitr::kable(prep_table, format = "latex", booktabs = TRUE, ...))
  }

  cat("\n")
  if(!is.null(note)) cat("\\tablenotes{\\textit{Note.}", note, "}\n")
  cat("\\end{threeparttable}\n\\end{", table_env, "}", sep = "")
}

#' @rdname apa_table
#' @export

apa_table.word <- function(
  x
  , caption = NULL
  , note = NULL
  , added_colnames = NULL
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")
  if(!is.null(caption)) validate(caption, check_class = "character", check_length = 1)
  if(!is.null(note)) validate(note, check_class = "character", check_length = 1)
  if(!is.null(added_colnames)) validate(added_colnames, check_class = "character")

  if(is.list(x) && !is.data.frame(x)) {
    tables_to_merge <- names(x)
    prep_table <- lapply(seq_along(x), function(i) {
      prep_table <- cbind(
        c(tables_to_merge[i], rep("&nbsp;", nrow(x[[i]])-1)) # Without the &nbsp; empty cells are set as standard instead of compact
        , rownames(x[[i]])
        , x[[i]]
      )
      rownames(prep_table) <- NULL
      second_col <- ifelse(is.null(rownames(x[[i]])), NULL, "")
      if(is.null(added_colnames)) {
        colnames(prep_table) <- c("&nbsp;", "&nbsp;", colnames(x[[i]]))
      } else {
        colnames(prep_table) <- c(added_colnames, colnames(x[[i]]))
      }

      prep_table
    }
    )
    x_merged <- do.call(rbind, prep_table)
    cat("<center>")
    cat("Table. ")
    cat("*", caption, "*", sep = "")
    cat("</center>")
    print(knitr::kable(x_merged, format = "pandoc", ...))
  } else {
    colnames(x) <- ifelse(colnames(x) == "", "&nbsp;", colnames(x))
    prep_table <- cbind(rownames(x), x)
    if(!is.na(added_colnames)) colnames(prep_table) <- c(added_colnames, colnames(x)) else colnames(prep_table) <- c("&nbsp;", colnames(x))
    rownames(prep_table) <- NULL
    print(knitr::kable(prep_table, format = "pandoc", ...))
  }

  if(!is.null(note)) {
    cat("\n")
    cat("<center>")
    cat("*Note.*", note)
    cat("</center>")
    cat("\n\n\n\n")
  }
}
