#' Prepare table for printing
#'
#' Formats \code{matrices} and \code{data.frames} to report them as tables according to APA guidelines
#' (6th edition).
#'
#' @param x Object to print, can be \code{matrix}, \code{data.frame}, or \code{list}. See details.
#' @param caption Character. Caption to be printed above the table.
#' @param note Character. Note to be printed below the table.
#' @param added_stub_head Character. Used as stub head (name of first column) if \code{row.names = TRUE}
#'    is passed to \code{\link[knitr]{kable}}; ignored if row names are omitted from the table.
#' @param col_spanners List. A named list of vectors of length 2 giving the first and second column to
#'    span with a grouping column name.
#' @param stub_indents List. A named list of vectors of length 2 giving the first and second row to
#'    indent. Names of list elements will be used as titles for indented sections.
#' @param midrules Numeric. Vector of line numbers in table (not counting column headings) that should be
#'    followed by a horizontal rule; ignored in MS Word documents.
#' @param placement Character. Indicates whether table should be placed at the exact location (\code{h}),
#'    at the top (\code{t}), bottom (\code{b}), or on a separate page (\code{p}). Arguments can be combined
#'    to indicate order of preference (\code{htb}); ignored in MS Word documents.
#' @param landscape Logical. If \code{TRUE} the table is printed in landscape format; ignored in MS Word
#'    documents.
#' @param small Logical. If \code{TRUE} the font size of the table content is reduced.
#' @param ... Further arguments to pass to \code{\link[knitr]{kable}}.
#'
#' @details
#'    When using \code{apa_table()}, the type of the ouput (LaTeX or MS Word) is determined automatically
#'    by the rendered document type. If no rendering is in progress the output default is LaTeX.
#'
#'    If \code{x} is a \code{list}, all list elements are merged by columns into a single table with
#'    the first column giving the names of the list elements elements.
#'
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
#'    , added_stub_head = "Descriptives"
#' )
#' @export

apa_table <- function(...) {

  # Set defaults and rename ellipsis arguments
  ellipsis <- list(...)
  x <- if(!is.null(ellipsis$x)) ellipsis$x else ellipsis[[1]]
  added_stub_head <- ellipsis$added_stub_head
  stub_indents <- ellipsis$stub_indents
  row_names <- if(is.null(ellipsis$row.names)) TRUE else ellipsis$row.names

  validate(row_names, "row.names", check_class = "logical", check_length = 1)
  if(!is.null(ellipsis$caption)) validate(ellipsis$caption, "caption", check_class = "character", check_length = 1)
  if(!is.null(ellipsis$note)) validate(ellipsis$note, "note", check_class = "character", check_length = 1)
  if(!is.null(added_stub_head)) validate(added_stub_head, check_class = "character", check_length = 1)
  if(!is.null(stub_indents)) validate(stub_indents, check_class = "list")

  # List of tables?
  if(is.list(x) && !is.data.frame(x)) {

    ## Assemble table
    # Old table merging by adding an additional column is depricated for the moment
#     prep_table <- merge_tables(
#       x
#       , empty_cells = ""
#       , row_names = row_names
#       , added_stub_head = added_stub_head
#     )

    if(row_names) {
      prep_table <- lapply(x, add_row_names, added_stub_head = added_stub_head)
    } else prep_table <- x

    prep_table <- do.call(rbind, prep_table)

    ### Indent individual tables
    list_indents <- lapply(x, function(x) 1:nrow(x))
    for(i in seq_along(list_indents)[-1]) list_indents[[i]] <- list_indents[[i]] + max(list_indents[[i - 1]])
    prep_table <- indent_stubs(prep_table, list_indents)

  } else {

    ## Assemble table
    if(row_names) {
      prep_table <- add_row_names(x, added_stub_head = added_stub_head)
    } else prep_table <- x
  }

  if(!is.null(ellipsis$escape) && ellipsis$escape) {
    x <- escape_latex(x)
    colnames(x) <- escape_latex(colnames(x))
  }

  # Indent stubs
  if(!is.null(stub_indents)) prep_table <- indent_stubs(prep_table, stub_indents)


  # Fix ellipsis for further use
  ellipsis$escape <- FALSE
  ellipsis$row.names <- FALSE
  if(!is.null(ellipsis$x)) ellipsis$x <- prep_table else ellipsis[[1]] <- prep_table


  # Pass to markup generating functions
  if(!is.null(ellipsis$format)) {
    output_format <- ellipsis$format
  } else {
    output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if(length(output_format) == 0) output_format <- "latex"
  }

  if(output_format == "latex") {
    do.call(apa_table.latex, ellipsis)
  } else {
    do.call(apa_table.word, ellipsis)
  }
}

#' @rdname apa_table
#' @export

apa_table.latex <- function(
  x
  , caption = NULL
  , note = NULL
  , added_stub_head = NULL
  , col_spanners = NULL
  , stub_indents = NULL
  , midrules = NULL
  , placement = "tbp"
  , landscape = FALSE
  , small = FALSE
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")
  if(!is.null(col_spanners)) {
    validate(col_spanners, check_class = "list")
    validate(unlist(col_spanners), "col_spanners", check_range = c(1, ncol(x)))
  }
  validate(placement, check_class = "character", check_length = 1)
  validate(landscape, check_class = "logical", check_length = 1)

  # Parse ellipsis
  ellipsis <- list(...)
  ellipsis$booktabs <- TRUE
  longtable <- if(!is.null(ellipsis$longtable)) ellipsis$longtable else FALSE
  if(longtable) {
    table_env <-  "ThreePartTable"
    table_note_env <- "TableNotes"
  } else {
    table_env <- "threeparttable"
    table_note_env <- "tablenotes"
  }

  n_cols <- ncol(x)
  n_rows <- nrow(x)

  # Center title row
  colnames(x)[-1] <- paste0("\\multicolumn{1}{c}{", colnames(x), "}")[-1]

  res_table <- do.call(function(...) knitr::kable(x, ...), ellipsis)

  ## Add midrules
  table_lines <- unlist(strsplit(res_table, "\n"))
  table_lines <- table_lines[!grepl("\\\\addlinespace", table_lines)] # Remove \\addlinespace

  if(!is.null(col_spanners)) table_lines <- add_col_spanners(table_lines, col_spanners, n_cols)

  if(longtable) table_lines <- c(table_lines[1:2], paste0("\\caption{", caption, "}\\\\"), table_lines[-c(1:2)])

  table_content_boarders <- grep("\\\\midrule|\\\\bottomrule", table_lines)

  if(!is.null(midrules)) {
    validate(midrules, check_class = "numeric", check_range = c(1, n_rows))

    table_lines[table_content_boarders[1] + midrules] <- paste(
      table_lines[table_content_boarders[1] + midrules]
      , "\\midrule"
    )
  }

  if(!is.null(note) & longtable) table_lines <- c(table_lines[-length(table_lines)], "\\insertTableNotes", table_lines[length(table_lines)])
  res_table <- paste(table_lines, collapse = "\n")

  # Print table
  place_opt <- paste0("[", placement, "]")
  if(landscape) {
    cat("\\begin{sidewaystable}\n", place_opt, sep = "")
    place_opt <- NULL
  }
  if(!longtable & !landscape) cat("\\begin{table}", place_opt, sep = "")
  if(small) cat("\n\\small{")
  cat("\n\\begin{center}\n\\begin{", table_env, "}", sep = "")
  if(!longtable) cat("\n\\caption{", caption, "}", sep = "")
  if(!is.null(note) & longtable) cat("\n\\begin{", table_note_env, "}\n\\textit{Note.} ", note, "\n\\end{", table_note_env, "}", sep = "")

  cat(res_table)

  if(!is.null(note) & !longtable) cat("\n\\begin{", table_note_env, "}\n\\textit{Note.} ", note, "\n\\end{", table_note_env, "}", sep = "")
  cat("\n\\end{", table_env, "}\n\\end{center}", sep = "")
  if(small) cat("\n}")
  if(!longtable & !landscape) cat("\n\\end{table}")
  if(landscape) cat("\n\\end{sidewaystable}")
}


#' @rdname apa_table
#' @export

apa_table.word <- function(
  x
  , caption = NULL
  , note = NULL
  , added_stub_head = NULL
  , stub_indents = NULL
  , ...
) {
  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'")

  # Parse ellipsis
  ellipsis <- list(...)
  res_table <- do.call(function(...) knitr::kable(x, ...), ellipsis)

  # Print table
  cat("<center>")
  cat("Table. ")
  cat("*", caption, "*", sep = "")
  cat("</center>\n")

  print(res_table)

  if(!is.null(note)) {
    cat("\n")
    cat("<center>")
    cat("*Note.*", note)
    cat("</center>")
    cat("\n\n\n\n")
  }
}


#' Add row names as first column
#'
#' Adds row names as the first column of the table and sets \code{row.name} attribute to \code{NULL}.
#' \emph{This function is not exported.}
#'
#' @param x data.frame or matrix.
#' @param added_stub_head Character. Used as stub head (name of first column).
#' @seealso \code{\link{apa_table}}
#'
#' @examples
#' NULL

add_row_names <- function(x, added_stub_head) {
  if(!is.null(rownames(x)) && all(rownames(x) != 1:nrow(x))) {
    mod_table <- cbind(rownames(x), x)

    if(!is.null(added_stub_head)) {
      colnames(mod_table) <- c(added_stub_head, colnames(x))
    } else {
      colnames(mod_table) <- c("", colnames(x))
    }
  } else mod_table <- x

  rownames(mod_table) <- NULL
  mod_table
}


#' Add stub indentation
#'
#' Indents stubs by line and adds section headings
#' \emph{This function is not exported.}
#'
#' @param x data.frame.
#' @param lines List. A named list of vectors of length 2 giving the first and second row to
#'    indent. Names of list elements will be used as titles for indented sections.
#' @param filler Character. Symbols used to indent stubs.
#' @seealso \code{\link{apa_table}}
#'
#' @examples
#' NULL

indent_stubs <- function(x, lines, filler = "\\ \\ \\ ") {

  # Add indentation
  stubs <- x[, 1]
  for(i in seq_along(lines)) {
    stubs[lines[[i]]] <- paste0(filler, stubs[lines[[i]]])
  }
  x[, 1] <- stubs

  section_titles <- lines[which(names(lines) != "")]
  section_titles <- sapply(section_titles, min)

  # Add section headings
  if(length(section_titles) > 0) {
    for(i in seq_along(section_titles)) {
      top <- if(section_titles[i] != 1) x[1:(section_titles[i] - 1 + (i-1)), ] else NULL
      bottom <- if(section_titles[i] != nrow(x)) x[(section_titles[i] + (i-1)):nrow(x), ] else x[nrow(x), ]
      x <- rbind(top, c(names(section_titles[i]), rep("", ncol(x) - 1)), bottom)
    }
  }

  x
}


#' Add table headings to group columns
#'
#' Takes a named list of containing column numbers to group with a heading
#' \emph{This function is not exported.}
#'
#' @param table_lines Character. Vector of characters containing one line of a LaTeX table each.
#' @param col_spanners List. A named list containing the indices of the first and last columns to group, where the names are the headings.
#' @param n_cols Numeric. Number of columns of the table.
#' @seealso \code{\link{apa_table}}
#'
#' @examples
#' NULL

add_col_spanners <- function(table_lines, col_spanners, n_cols) {

  # Grouping column names
  multicols <- sapply(
    seq_along(col_spanners)
    , function(i, names) {
      paste0("\\multicolumn{", diff(col_spanners[[i]]) + 1, "}{c}{", names[i], "}")
    }
    , names(col_spanners)
  )

  ## Calculate group distances
  n_ampersands <- c()
  if(length(col_spanners) > 1) {
    group_indices <- sort(unlist(col_spanners))
    group_indices <- group_indices[-c(1, length(group_indices))] # Remove first and last column number
    n_ampersands <- diff(group_indices)[-seq(from = 2, to = length(group_indices), by = 2)] # Remove differences between column numbers of the same group
  }
  n_ampersands <- c(n_ampersands, 0)

  ## Add ampersands for empty columns
  leading_amps <- paste(rep(" &", min(unlist(col_spanners)) - 1), collapse = " ")
  trailing_amps <- if(n_cols - max(unlist(col_spanners)) > 0) {
    paste(rep(" &", n_cols - max(unlist(col_spanners))), collapse = " ")
  } else ""


  group_headings <- c()
  for(i in 1:(length(multicols))) {
    group_headings <- paste(group_headings, multicols[i], paste(rep("&", n_ampersands[i]), collapse = " "))
  }
  group_headings <- paste(leading_amps, group_headings, trailing_amps, "\\\\", sep = "")

  # Grouping midrules
  group_midrules <- sapply(
    seq_along(col_spanners)
    , function(i) {
      paste0("\\cmidrule(r){", min(col_spanners[[i]]), "-", max(col_spanners[[i]]), "}")
    }
  )
  group_midrules <- paste(group_midrules, collapse = " ")


  table_environment <- which(grepl("\\\\toprule", table_lines))

  table_lines <- c(
    table_lines[1:table_environment]
    , group_headings
    , group_midrules
    , table_lines[(table_environment + 1):length(table_lines)]
  )

  table_lines
}


#' Merge tables in list
#'
#' Takes a list of containing one or more \code{matrix} or \code{data.frame} and merges them into a single table.
#' \emph{This function is not exported and currently unused.}
#'
#' @param x List. A named list containing one or more \code{matrix} or \code{data.frame}.
#' @param empty_cells Character. String to place in empty cells; should be \code{""} if the target document is LaTeX and
#'    \code{"&nbsp;"} if the target document is Word.
#' @param row_names Logical. Vector of boolean values specifying whether to print column names for the corresponding list
#'    element.
#' @param added_stub_head Character. Vector of names for first unnamed columns. See \code{\link{apa_table}}.
#' @seealso \code{\link{apa_table}}
#'
#' @examples
#' NULL

merge_tables <- function(x, empty_cells, row_names, added_stub_head) {

  table_names <- names(x)

  prep_table <- lapply(seq_along(x), function(i) {

    if(row_names[i]) {
      i_table <- add_row_names(x[[i]], added_stub_head = added_stub_head[(length(added_stub_head) == 2) + 1])
    } else if(any(row_names)) { # Add empty column if row names are added to any table
      i_table <- cbind("", i_table)
      colnames(i_table) <- c("", colnames(i_table))
    } else i_table <- x[[i]]

    # Merge with added column
    prep_table <- cbind(
      c(table_names[i], rep("", nrow(x[[i]])-1))
      , i_table
    )

    # Add colnames
    if(row_names[i] && !is.null(rownames(x[[i]])) && length(added_stub_head) < 2) {
      second_col <- ""
    } else second_col <- NULL
    if(is.null(added_stub_head)) {
      colnames(prep_table) <- c("", second_col, colnames(x[[i]]))
    } else {
      colnames(prep_table) <- c(added_stub_head, second_col, colnames(x[[i]]))
    }

    as.data.frame(prep_table, stringsAsFactors = FALSE)
  })

  prep_table
}
