
#' @import tinylabels
NULL

#' Set Default Variable Labels from Column Names
#'
#' This internal function creates variable labels from the column names of a data frame
#'
#' @param x A \code{data.frame}
#' @return Returns a \code{data.frame} with labelled columns. Labels are preserved (if already specified), otherwise generated from column names.
#' @rdname default_label
#' @keywords internal

default_label <- function(x, ...) {
  UseMethod("default_label", x)
}



#' @rdname default_label
#' @keywords internal

default_label.default <- function(x, ...) no_method(x)



#' @rdname default_label
#' @keywords internal

default_label.data.frame <- function(x) {

  x_out <- as.data.frame.list(
    x = mapply(
      FUN = function(y, value) {
        if(is.null(variable_label(y))) {
          variable_label(y) <- value
        }
        y
      }
      , y = x
      , value = colnames(x)
      , USE.NAMES = TRUE
      , SIMPLIFY = FALSE
    )
    , check.names = FALSE
    , stringsAsFactors = FALSE
    , col.names = attr(x, "names")
    , row.names = attr(x, "row.names") # `rownames(x)` would coerce to character
  )
  attributes(x_out) <- attributes(x)
  x_out
}



# ------------------------------------------------------------------------------
# Functions to generate very simple codebooks

tidy_variable_lables <- function(x) {
  x <- default_label.data.frame(x)
  labels <- unlist(variable_labels(x))

label_range <- function(y) {

  if(inherits(y, "numeric")) return(paste0("[", min(y, na.rm = TRUE), ", ", max(y, na.rm = TRUE), "]"))
  if(inherits(y, "factor")) return(nlevels(y))
  if(inherits(y, "character")) return(length(unique(y[!is.na(y)])))

  return("")
}

  tidied_labels <- data.frame(
    variable = names(labels)
    , label = ifelse(labels == names(labels), NA, labels)
    , type = sapply(x, function(y) paste(class(y), collapse = ", "))
    , range = sapply(x, label_range)
    , missing = sapply(x, function(y) sum(is.na(y)))
    , stringsAsFactors = FALSE
  )

  if(isTRUE(package_available("skimr"))) {
    tidied_labels <- cbind(
      tidied_labels
      , distribution = sapply(x, function(y) if (is.numeric(y)) skimr::inline_hist(y) else "")
    )
  }

  tidied_labels
}


#' Simple codebook
#'
#' Generate a simple codebook in CSV-format
#'
#' @param x data.frame. Data to be written.
#' @inheritDotParams utils::write.table -x
#'
#' @details If the \pkg{skimr} package is installed, an inline histogram is added
#'   for all numeric variables.
#'
#' @seealso \code{\link[utils]{write.csv}}
#' @export
#'
#' @examples
#' \dontrun{
#' variable_labels(cars) <- c(speed = "Speed [ft/s]", dist = "Distance traveled [m]")
#' simple_codebook(cars, file = "cars_codebook.csv")
#' }

simple_codebook <- function(x, ...) {
  tidied_labels <- tidy_variable_lables(x)
  utils::write.csv(tidied_labels, ...)
}
