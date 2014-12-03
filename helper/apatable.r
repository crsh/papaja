apa.table <- function(...) {
  require("knitr")
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(output_format == "latex") {
    apa.table.latex(...)
  } else {
    apa.table.word(...)
  }
}

apa.table.latex <- function(x, caption = NULL, note = NULL, placement = "tbp", landscape = FALSE, var.names = c("", ""), ...) {
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
      colnames(prep_table) <- c(var.names, colnames(x[[i]]))
      
      return(prep_table)
    })
    x_merged <- do.call(rbind, prep_table)
    kable(x_merged, format = "latex", booktabs = TRUE, ...)
  } else {
    if(!is.null(rownames(x)) & !(all(rownames(x) == ""))) {
      prep_table <- cbind(rownames(x), x)
      colnames(prep_table) <- c(var.names[1], colnames(x))
    } else prep_table <- x
    rownames(prep_table) <- NULL
    
    kable(prep_table, format = "latex", booktabs = TRUE, ...)
  }
  
  cat("\n")
  if(!is.null(note)) cat("\\tablenotes{\\textit{Note.}", note, "}\n")
  cat("\\end{threeparttable}\n\\end{", table_env, "}", sep = "")
}

apa.table.word <- function(x, caption = NULL, note = NULL, var.names = c("&nbsp;", "&nbsp;"), ...) {
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
      colnames(prep_table) <- c(var.names, colnames(x[[i]]))
      
      return(prep_table)
    }
    )
    x_merged <- do.call(rbind, prep_table)
    
    cat("\n\n")
    cat("<center>")
    cat("Table. ")
    cat("*", caption, "*", sep = "")
    cat("</center>")
    kable(x_merged, format = "pandoc", ...)
  } else {
    colnames(x) <- ifelse(colnames(x) == "", "&nbsp;", colnames(x))
    if(!is.null(rownames(x)) & !(all(rownames(x) == ""))) {
      rownames(x) <- ifelse(rownames(x) == "", "&nbsp;", rownames(x))
      prep_table <- cbind(rownames(x), x)
      colnames(prep_table) <- c(var.names[1], colnames(x))
    } else prep_table <- x
    rownames(prep_table) <- NULL
    
    cat("<center>")
    cat("Table. ")
    cat("*", caption, "*", sep = "")
    cat("</center>")
    kable(prep_table, format = "pandoc", ...)
  }
  
  if(!is.null(note)) {
    cat("\n")
    cat("<center>")
    cat("*Note.*", note)
    cat("</center>")
  }
  cat("\n\n\n\n")
}