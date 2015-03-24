#' Prepare table for printing
#'
#' Formats \code{matrices} and \code{data.frames} to report them as tables according to APA guidelines
#' (6th edition).
#'
#' @param x Object to print, can be \code{matrix}, \code{data.frame}, or \code{list}. See details.
#' @param caption Character. Caption to be printed above the table.
#' @param note Character. Note to be printed below the table.
#' @param row_names Logical. Indicates whether to include row names; by default, row names are included if
#'    \code{rownames(x)} is neither \code{NULL} nor identical to \code{1:nrow(x)}.
#' @param added_colnames Character. Vector of names for first unnamed columns. See details.
#' @param midrules Numeric. Vector of line numbers in table (not counting column headings) that should be
#'    followed by a horizontal rule; ignored in MS Word documents.
#' @param placement Character. Indicates whether table should be placed at the exact location (\code{h}),
#'    at the top (\code{t}), bottom (\code{b}), or on a separate page (\code{p}). Arguments can be combined
#'    to indicate order of preference (\code{htb}); ignored in MS Word documents.
#' @param landscape Logical. If \code{TRUE} the table is printed in landscape format; ignored in MS Word
#'    documents.
#' @param ... Further arguments to pass to \code{\link[knitr]{kable}}. \code{row.names} argument is overwritten
#'    by \code{row_names}.
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
  , row_names = TRUE
  , added_colnames = NULL
  , midrules = NULL
  , placement = "tbp"
  , landscape = FALSE
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")
  if(!is.null(caption)) validate(caption, check_class = "character", check_length = 1)
  if(!is.null(note)) validate(note, check_class = "character", check_length = 1)
  validate(row_names, check_class = "logical", check_length = 1)
  if(!is.null(added_colnames)) validate(added_colnames, check_class = "character")
  if(!is.null(midrules)) validate(midrules, check_class = "numeric", check_range = c(1, nrow(x)))
  validate(placement, check_class = "character", check_length = 1)
  validate(landscape, check_class = "logical", check_length = 1)

  if(landscape) table_env <- "sidewaystable" else table_env <- "table"
  cat("\\begin{", table_env, "}[", placement, "]\n\\centering\n\\begin{threeparttable}\n\\caption{", caption, "}", sep = "")

  if(is.list(x) && !is.data.frame(x)) {
    prep_table <- merge_tables(
      x
      , ""
      , row_names = row_names
      , added_colnames = added_colnames
    )

    x_merged <- do.call(rbind, prep_table)
    print(knitr::kable(x_merged, format = "latex", booktabs = TRUE, ...))
  } else {
    prep_table <- x
    if(row_names && !is.null(rownames(x)) && rownames(x) != 1:nrow(x)) {
      prep_table <- cbind(rownames(x), x)
      colnames(prep_table) <- c("", colnames(x))
    }
    if(!is.null(added_colnames)) {
      new_colnames <- c(added_colnames, colnames(x))
      if(length(new_colnames) > ncol(prep_table)) stop("Too many column names. Please check length of 'added_colnames'.")
      colnames(prep_table) <- new_colnames
    }

    res_table <- knitr::kable(prep_table, format = "latex", booktabs = TRUE, ...)
    if(!is.null(midrules)) {
      table_lines <- unlist(strsplit(res_table, "\n"))
      table_content_boarders <- grep("\\\\midrule|\\\\bottomrule", table_lines)
      table_lines[table_content_boarders[1] + midrules] <- sapply(
        table_lines[table_content_boarders[1] + midrules]
        , paste, "\\midrule"
      )
      res_table <- paste(table_lines, collapse = "\n")
    }
    cat(res_table)
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
  , row_names = TRUE
  , added_colnames = NULL
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")
  if(!is.null(caption)) validate(caption, check_class = "character", check_length = 1)
  if(!is.null(note)) validate(note, check_class = "character", check_length = 1)
  validate(row_names, check_class = "logical", check_length = 1)
  if(!is.null(added_colnames)) validate(added_colnames, check_class = "character")

  if(is.list(x) && !is.data.frame(x)) {
    prep_table <- merge_tables(
      x
      , "&nbsp;"
      , row_names = row_names
      , added_colnames = added_colnames
    )

    x_merged <- do.call(rbind, prep_table)

    cat("<center>")
    cat("Table. ")
    cat("*", caption, "*", sep = "")
    cat("</center>")

    print(knitr::kable(x_merged, format = "pandoc", ...))
  } else {
    prep_table <- x
    if(row_names && !is.null(rownames(x)) && rownames(x) != 1:nrow(x)) {
      prep_table <- cbind(rownames(x), x)
      colnames(prep_table) <- c("", colnames(x))
    }
    if(!is.null(added_colnames)) {
      new_colnames <- c(added_colnames, colnames(x))
      if(length(new_colnames) > ncol(prep_table)) stop("Too many column names. Please check length of 'added_colnames'.")
      colnames(prep_table) <- new_colnames
    }
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


#' Merge tables in list
#'
#' Takes a list of containing one or more \code{matrix} or \code{data.frame} and merges them into a single table.
#' \emph{This function is not exported.}
#'
#' @param x List containing one or more \code{matrix} or \code{data.frame}.
#' @param empty_cell Character. String to place in empty cells; should be \code{""} if the target document is LaTeX and
#'    \code{"&nbsp;"} if the target document is Word.
#' @param added_colnames Character. Vector of names for first unnamed columns. See \code{\link{apa_table}}.
#' @seealso \code{\link{apa_table}}
#'
#' @examples
#' NULL

merge_tables <- function(x, empty_cells, row_names, added_colnames) {
  tables_to_merge <- names(x)
  prep_table <- lapply(seq_along(x), function(i) {

    # Add rownames
    if(row_names && !is.null(rownames(x[[i]])) && rownames(x[[i]]) != 1:nrow(x[[i]])) {
      i_table <- cbind(rownames(x[[i]]), x[[i]])
      colnames(i_table) <- c("", colnames(x[[i]]))
    } else i_table <- x[[i]]
    prep_table <- cbind(
      c(tables_to_merge[i], rep("", nrow(x[[i]])-1))
      , i_table
    )
    rownames(prep_table) <- NULL

    # Add colnames
    if(row_names && !is.null(rownames(x[[i]])) && rownames(x[[i]]) != 1:nrow(x[[i]]) && length(added_colnames) < 2) {
      second_col <- ""
    } else second_col <- NULL
    if(is.null(added_colnames)) {
      colnames(prep_table) <- c("", second_col, colnames(x[[i]]))
    } else {
      new_colnames <- c(added_colnames, second_col, colnames(x[[i]]))
      if(length(new_colnames) > ncol(prep_table)) stop("Too many column names. Please check length of 'added_colnames'.")
      colnames(prep_table) <- new_colnames
    }

    prep_table
  })
}

