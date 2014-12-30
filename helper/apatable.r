apa.table <- function(...) {
  require("knitr")
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(output_format == "latex") {
    apa.table.latex(...)
  } else {
    apa.table.word(...)
  }
}

apa.table.latex <- function(x, caption = NULL, note = NULL, placement = "tbp", landscape = FALSE, added.colnames = NULL, ...) {
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
      if(is.null(added.colnames)) {
        colnames(prep_table) <- c("", "", colnames(x[[i]]))
      } else {
        colnames(prep_table) <- c(added.colnames, colnames(x[[i]]))
      }
      
      return(prep_table)
    }
    )
    x_merged <- do.call(rbind, prep_table)
    print(kable(x_merged, format = "latex", booktabs = TRUE, ...))
  } else {
    prep_table <- cbind(rownames(x), x)
    if(!is.null(added.colnames)) colnames(prep_table) <- c(added.colnames, colnames(x))
    rownames(prep_table) <- NULL
    print(kable(prep_table, format = "latex", booktabs = TRUE, ...))
  }
  
  cat("\n")
  if(!is.null(note)) cat("\\tablenotes{\\textit{Note.}", note, "}\n")
  cat("\\end{threeparttable}\n\\end{", table_env, "}", sep = "")
}

apa.table.word <- function(x, caption = NULL, note = NULL, added.colnames = NULL, ...) {
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
      if(is.null(added.colnames)) {
        colnames(prep_table) <- c("&nbsp;", "&nbsp;", colnames(x[[i]]))
      } else {
        colnames(prep_table) <- c(added.colnames, colnames(x[[i]]))
      }
      
      return(prep_table)
    }
    )
    x_merged <- do.call(rbind, prep_table)
    cat("<center>")
    cat("Table. ")
    cat("*", caption, "*", sep = "")
    cat("</center>")
    print(kable(x_merged, format = "pandoc", ...))
  } else {
    colnames(x) <- ifelse(colnames(x) == "", "&nbsp;", colnames(x))
    prep_table <- cbind(rownames(x), x)
    if(!is.na(added.colnames)) colnames(prep_table) <- c(added.colnames, colnames(x)) else colnames(prep_table) <- c("&nbsp;", colnames(x))
    rownames(prep_table) <- NULL
    print(kable(prep_table, format = "pandoc", ...))
  }
  
  if(!is.null(note)) {
    cat("\n")
    cat("<center>")
    cat("*Note.*", note)
    cat("</center>")
    cat("\n\n\n\n")
  }
}