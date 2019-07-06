#' Aggregate data much faster using dplyr
#'
#' This is a convenience wrapper for aggregating your data using \pkg{dplyr} functions that tend to be much faster than the
#' usual \code{aggregate} command. It is also easy to call from within a function. \emph{This function is not exported.}
#'
#' @param data A \code{data.frame} that contains the data.
#' @param factors Character. A vector of factor names to aggregate data by.
#' @param dv Character. The dependent variable to aggregate. All variables in \code{data} that contain this substring
#'    will be aggregated separately.
#' @param fun Closure. The function used for aggregation.
#' @keywords internal
#' @examples NULL

fast_aggregate <- function(data, factors, dv, fun, na.rm = TRUE) {
  # subset: this is a bit faster than subset.data.frame
  data <- data[, colnames(data) %in% c(factors, dv)]
  # the dplyr magic: this construct seems to be as fast as using pipes
  grouped <- dplyr::grouped_df(data, vars = factors, drop = TRUE)

  dv_ <- dplyr::sym(dv)
  args <- list(x = dplyr::quo(!!dv_), na.rm = na.rm)
  agg_data <- as.data.frame(dplyr::summarise(.data = grouped, temporary_dv_name = fun(!!!args)))
  # do this in base R to avoid using `:=`
  colnames(agg_data)[colnames(agg_data) == "temporary_dv_name"] <- dv

  # soft-deprecated in dplyr:
  # agg_data <- as.data.frame(dplyr::summarise_all(.tbl = grouped, .funs = dplyr::funs(fun(., na.rm = TRUE))))

  return(agg_data)
}

