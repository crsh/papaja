#' Prepare table for printing
#'
#' Formats \code{matrices} and \code{data.frames} to report them as tables according to APA guidelines
#' (6th edition).
#'
#' @param x Object to print, can be \code{matrix}, \code{data.frame}, or \code{list}. See details.
#' @param caption Character. Caption to be printed above the table.
#' @param note Character. Note to be printed below the table.
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
  , added_colnames = NULL
  , midrules = NULL
  , placement = "tbp"
  , landscape = FALSE
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")
  if(!is.null(caption)) validate(caption, check_class = "character", check_length = 1)
  if(!is.null(note)) validate(note, check_class = "character", check_length = 1)
  if(!is.null(added_colnames)) validate(added_colnames, check_class = "character")
  validate(placement, check_class = "character", check_length = 1)
  validate(landscape, check_class = "logical", check_length = 1)

  # Parse ellipsis
  ellipsis <- list(...)
  ellipsis$format <- "latex"
  ellipsis$booktabs <- TRUE
  longtable <- if(!is.null(ellipsis$longtable)) ellipsis$longtable else FALSE
  if(longtable) {
    table_env <-  "ThreePartTable"
    table_note_env <- "TableNotes"
  } else {
    table_env <- "threeparttable"
    table_note_env <- "tablenotes"
  }

  if(!is.null(ellipsis$escape) && ellipsis$escape) {
       x <- escape_latex(x)
       colnames(x) <- escape_latex(colnames(x))
       rownames(x) <- escape_latex(rownames(x))
  }
  ellipsis$escape <- FALSE

  if(!is.null(ellipsis$row.names)) {
    row_names <- ellipsis$row.names
  } else { # Default to FALSE if rownames are 1:x or NULL
    if(is.list(x) && !is.data.frame(x)) {
      row_names <- !all(sapply(x, function(x) all(rownames(x) == 1:nrow(x))))
    } else {
      row_names <- !all(rownames(x) == 1:nrow(x))
    }
  }
  ellipsis$row.names <- FALSE

  # Assemble table
  place_opt <- paste0("[", placement, "]")
  if(landscape) {
    cat("\\begin{sidewaystable}", place_opt, sep = "")
    place_opt <- NULL
  }
  if(!longtable) cat("\\begin{table}", place_opt, sep = "")
  cat("\n\\begin{center}\n\\begin{", table_env, "}", sep = "")
  if(!longtable) cat("\n\\caption{", caption, "}", sep = "")
  if(!is.null(note) & longtable) cat("\n\\begin{", table_note_env, "}\n\\textit{Note.} ", note, "\n\\end{", table_note_env, "}", sep = "")

  if(is.list(x) && !is.data.frame(x)) {
    n_rows <- sum(sapply(x, nrow))
    prep_table <- merge_tables(
      x
      , ""
      , row_names = row_names
      , added_colnames = added_colnames
    )

    x_merged <- do.call(rbind, prep_table)
    res_table <- do.call(function(...) knitr::kable(x_merged, ...), ellipsis)
  } else {
    n_rows <- nrow(x)
    prep_table <- x
    if(row_names && !is.null(rownames(x))) {
      prep_table <- cbind(rownames(x), x)
      colnames(prep_table) <- c("", colnames(x))
      rownames(prep_table) <- NULL
    }
    if(!is.null(added_colnames)) {
      new_colnames <- c(added_colnames, colnames(x))
      if(length(new_colnames) > ncol(prep_table)) stop("Too many column names. Please check length of 'added_colnames'.")
      colnames(prep_table) <- new_colnames
    }

    colnames(prep_table)[-1] <- paste0("\\multicolumn{1}{c}{", colnames(prep_table), "}")[-1] # Center title row

    res_table <- do.call(function(...) knitr::kable(prep_table, ...), ellipsis)
  }

  table_lines <- unlist(strsplit(res_table, "\n"))

  if(longtable) table_lines <- c(table_lines[1:2], paste0("\\caption{", caption, "}\\\\"), table_lines[-c(1:2)])

  if(!is.null(midrules)) {
    validate(midrules, check_class = "numeric", check_range = c(1, n_rows))

    table_lines <- table_lines[!grepl("\\\\addlinespace", table_lines)] # Remove \addlinespace so midrules are in accurate place
    table_content_boarders <- grep("\\\\midrule|\\\\bottomrule", table_lines)
    table_lines[table_content_boarders[1] + midrules] <- sapply(
      table_lines[table_content_boarders[1] + midrules]
      , paste, "\\midrule"
    )
  }

  if(!is.null(note) & longtable) table_lines <- c(table_lines[-length(table_lines)], "\\insertTableNotes", table_lines[length(table_lines)])
  res_table <- paste(table_lines, collapse = "\n")

  cat(res_table)

  if(!is.null(note) & !longtable) cat("\n\\begin{", table_note_env, "}\n\\textit{Note.} ", note, "\n\\end{", table_note_env, "}", sep = "")
  cat("\n\\end{", table_env, "}\n\\end{center}", sep = "")
  if(!longtable) cat("\n\\end{table}")
  if(landscape) cat("\n\\end{sidewaystable}")
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

  # Parse ellipsis
  ellipsis <- list(...)
  if(!is.null(ellipsis$format)) ellipsis$format <- "pandoc"

  if(!is.null(ellipsis$escape) && ellipsis$escape) {
    x <- escape_latex(x)
    colnames(x) <- escape_latex(colnames(x))
    rownames(x) <- escape_latex(rownames(x))
  }
  ellipsis$escape <- FALSE

  if(!is.null(ellipsis$row.names)) {
    row_names <- ellipsis$row.names
  } else { # Default to FALSE if rownames are 1:x or NULL
    if(is.list(x) && !is.data.frame(x)) {
      row_names <- !all(sapply(x, function(x) all(rownames(x) == 1:nrow(x))))
    } else {
      row_names <- !all(rownames(x) == 1:nrow(x))
    }
  }
  ellipsis$row.names <- FALSE

  # Assemble table
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

    print(do.call(function(...) knitr::kable(x_merged, ...), ellipsis))
  } else {
    prep_table <- x
    if(row_names && !is.null(rownames(x))) {
      prep_table <- cbind(rownames(x), x)
      colnames(prep_table) <- c("", colnames(x))
      rownames(prep_table) <- NULL
    }
    if(!is.null(added_colnames)) {
      new_colnames <- c(added_colnames, colnames(x))
      if(length(new_colnames) > ncol(prep_table)) stop("Too many column names. Please check length of 'added_colnames'.")
      colnames(prep_table) <- new_colnames
    }
    print(do.call(function(...) knitr::kable(prep_table, ...), ellipsis))
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
    if(row_names && !is.null(rownames(x[[i]]))) {
      i_table <- cbind(rownames(x[[i]]), x[[i]])
      colnames(i_table) <- c("", colnames(x[[i]]))
    } else i_table <- x[[i]]
    prep_table <- cbind(
      c(tables_to_merge[i], rep("", nrow(x[[i]])-1))
      , i_table
    )
    rownames(prep_table) <- NULL

    # Add colnames
    if(row_names && !is.null(rownames(x[[i]])) && length(added_colnames) < 2) {
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

