#' Prepare table for printing
#'
#' Formats \code{matrices} and \code{data.frames} to report them as tables according to APA guidelines
#' (6th edition).
#' @param x Object to print, can be \code{matrix}, \code{data.frame}, or \code{list}. See details.
#' @param caption Character. Caption to be printed above the table.
#' @param note Character. Note to be printed below the table.
#' @param placement Character. Indicates wheter table should be placed at the exact location ("h"), at the
#'    top ("t"), bottom ("b"), or on a new page ("p"). Arguments can be combined ("htb"); ignored in MS
#'    Word documents.
#' @param landscape Logical. If \code{TRUE} the table is printed in landscape format. Ignored in MS Word
#'    documents.
#' @param added_colnames Character. Vector of names for first unnamed columns. See details.
#' @param ... Further arguments to pass to \code{kntir::kable}.
#' @seealso \code{\link{apply}}
#' @details If \code{x} is a \code{list}, all list elements are merged by columns into a single table with
#'    the first column giving the names of elements.
#'
#'    If the first column(s) of the table are unnamed, the names for these columns can be supplied using the
#'    \code{added_colnames} parameter. This is the case when an object has rownames (unless
#'    \code{row.names = FALSE} is passed to \code{knitr::kable}) and when elements of a \code{list} are
#'    merged.
#'
#'    The syntax of the ouput is determined automatically by the rendered document type. If no rendering is
#'    in progress the output default is LaTeX.
#' @examples ...
#' @export

apa_table <- function(...) {
  requireNamespace("knitr", quietly = TRUE)
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(length(output_format) == 0) output_format <- "latex"
  if(output_format == "latex") {
    apa_table.latex(...)
  } else {
    apa_table.word(...)
  }
}

apa_table.latex <- function(x, caption = NULL, note = NULL, placement = "tbp", landscape = FALSE, added_colnames = NULL, ...) {
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

apa_table.word <- function(x, caption = NULL, note = NULL, added_colnames = NULL, ...) {
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
