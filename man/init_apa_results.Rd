% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{init_apa_results}
\alias{init_apa_results}
\title{Create Empty Container for Results}
\usage{
init_apa_results()
}
\value{
A named list (with additional class \code{apa_results}) containing the following components:

\describe{
\item{\code{estimate}}{A (named list of) character strings giving effect-size estimates.}
\item{\code{statistic}}{A (named list of) character strings giving test statistic, parameters, and \emph{p} values.}
\item{\code{full_result}}{A (named list of) character strings comprised of \code{estimate} and \code{statistic} for each factor.}
\item{\code{table}}{A data frame containing all results; can, for example, be passed to \code{\link[=apa_table]{apa_table()}}.}
}
}
\description{
Creates the default empty container for the results of \code{\link[=apa_print]{apa_print()}}.
\emph{This function is not exported.}
}
\keyword{internal}
