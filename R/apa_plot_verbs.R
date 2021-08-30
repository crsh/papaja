

#' @export

change <- function(x, ...) {
  UseMethod("change", x)
}


#' @export
change.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(X = x$plots, FUN = change, ...)
  x
}

#' @export

change.apa_plot <- function(x, ...) {
  x$.state <- "modify"
  x
}


#' @export

add <- function(x, ...) {
  UseMethod("add")
}

#' @export

add.apa_plot_list <- function(x, ...) {
  x$plots <- lapply(x$plots, add, ...)
  x
}

#' @export

add.apa_plot <- function(x, ...) {
  x$.state <- "add"
  x
}
