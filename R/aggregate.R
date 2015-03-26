#' Aggregate data much faster using dplyr
#'
#' This is a convenience wrapper for aggregating your data using dplyr functions that tend to be much faster than the usual \code{aggregate} command.
#' It is also easy to call from within a function.
#'
#' @param data A \code{data.frame} that contains the data.
#' @param factors Character. Data is intended to be aggregated by these factors.
#' @param dv Character. The dependent variable that is aggregated. All variables in \code{data} that contain this string will be aggregated separately.
#' @param fun The function used for aggregation.
#' @details If \code{x} is a vector, \code{digits}, \code{gt1}, and \code{zero} can be vectors
#'    according to which each element of the vector is formated. Parameters are recycled if length of \code{x}
#'    exceeds length of the parameter vectors. If \code{x} is a matrix, the vectors specify the formating
#'    of either rows or columns according to the value of \code{margin}.
#' @examples NULL

fast_aggregate<-function(data, factors, dv, fun, na.rm=FALSE){

  fl <- lapply(as.list(factors),FUN=as.name)

  grouped<-dplyr::grouped_df(data,vars=fl,drop=TRUE)
  agg.data<-as.data.frame(dplyr::summarise_each(grouped,dplyr::funs(fun(., na.rm=na.rm)),matches(dv)))

  return(agg.data)
}


# stolen from dplyr, because it is not exported by dplyr
matches <- function(vars, match, ignore.case = TRUE) {
  stopifnot(is.string(match), nchar(match) > 0)

  grep(match, vars, ignore.case = ignore.case)
}
