#' Aggregate data much faster using dplyr
#'
#' This is a convenience wrapper for aggregating your data using dplyr functions that tend to be much faster than the
#' usual \code{aggregate} command. It is also easy to call from within a function. \emph{This function is not exported.}
#'
#' @param data A \code{data.frame} that contains the data.
#' @param factors Character. A vector of factor names to aggregate data by.
#' @param dv Character. The dependent variable to aggregate. All variables in \code{data} that contain this substring
#'    will be aggregated separately.
#' @param fun Closure. The function used for aggregation.
#' @examples NULL

fast_aggregate <- function(data, factors, dv, fun) {

  fl <- lapply(as.list(factors), FUN = as.name)

  data <- data[, colnames(data) %in% c(factors) | grepl(dv, colnames(data))]

  grouped <- dplyr::grouped_df(data, vars = fl, drop = TRUE)
  agg.data <- as.data.frame(dplyr::summarise_all(.tbl = grouped, .funs = dplyr::funs(fun(., na.rm = TRUE))))

  return(agg.data)
}

