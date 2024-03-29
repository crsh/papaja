% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apa_print_wsci.R
\name{apa_print.papaja_wsci}
\alias{apa_print.papaja_wsci}
\title{Typeset Within-Subjects Confidence Intervals}
\usage{
\method{apa_print}{papaja_wsci}(x, ...)
}
\arguments{
\item{x}{An object of class \code{papaja_wsci}.}

\item{...}{
  Arguments passed on to \code{\link[=apa_num]{apa_num}}
  \describe{
    \item{\code{}}{}
  }}
}
\description{
This method takes an output object from \code{\link{wsci}} and creates a
table and character strings to report means and within-subjects confidence
intervals in a table or in text.
}
\value{
\code{apa_print()}-methods return a named list of class \code{apa_results} containing the following elements:

\item{estimate}{One or more character strings giving point estimates, confidence intervals, and confidence level. A single string is returned in a vector; multiple strings are returned as a named list. If no estimate is available the element is \code{NULL}.}
\item{statistic}{One or more character strings giving the test statistic, parameters (e.g., degrees of freedom), and p-value. A single string is returned in a vector; multiple strings are returned as a named list. If no estimate is available the element is \code{NULL}.}
\item{full_result}{One or more character strings comprised `estimate` and `statistic`. A single string is returned in a vector; multiple strings are returned as a named list. }
\item{table}{A \code{data.frame} of class \code{apa_results_table} that contains all elements of \code{estimate} and \code{statistics}. This table can be passed to \code{apa_table()} for reporting.}

Column names in \code{apa_results_table} are standardized following the \href{https://www.tidymodels.org/learn/develop/broom/#glossary}{\pkg{broom} glossary} (e.g., \code{term}, \code{estimate} \code{conf.int}, \code{statistic}, \code{df}, \code{df.residual}, \code{p.value}). Additionally, each column is labelled (e.g., \code{$\\hat{\\eta}^2_G$} or \code{$t$}) using the \pkg{tinylabels} package and these labels are used as column names when an \code{apa_results_table} is passed to \code{apa_table()}.

}
