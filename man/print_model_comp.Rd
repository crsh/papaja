% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_model_comp.R
\name{print_model_comp}
\alias{print_model_comp}
\title{Typeset Statistical Results from Model Comparisons}
\usage{
print_model_comp(
  x,
  models = NULL,
  conf.int = NULL,
  boot_samples = 1000,
  progress_bar = FALSE,
  in_paren = FALSE,
  observed = TRUE
)
}
\arguments{
\item{x}{A data frame of class \code{apa_variance_table} as returned by \code{\link[=arrange_anova]{arrange_anova()}}.}

\item{models}{List. List containing fitted \code{lm} objects that were compared using \code{\link[=anova]{anova()}}. If the list is named, element names are used as model names in the output object.}

\item{conf.int}{Numeric. Confidence level for the confidence interval for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}. If \code{conf.int = NULL} no confidence intervals are estimated.}

\item{boot_samples}{Numeric. Number of bootstrap samples to estimate confidence intervals for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}; ignored if \code{conf.int = NULL}.}

\item{progress_bar}{Logical. Determines whether a progress bar is printed while bootstrapping.}

\item{in_paren}{Logical. Whether the formatted string is to be reported in
parentheses. If \code{TRUE}, parentheses in the formatted string (e.g., those
enclosing degrees of freedom) are replaced with brackets.}

\item{observed}{Logical. Indicates whether predictor variables were
observed. See details.}
}
\description{
This function is the workhorse of the \link[=apa_print.list]{apa_print()} method
for model comparisons. It takes a data frame of class \code{apa_model_comp} and
produces strings to report the results in accordance with APA manuscript
guidelines.
\emph{This function is not exported.}
}
\seealso{
\code{\link[=arrange_anova]{arrange_anova()}}, \code{\link[=apa_print.aov]{apa_print.aov()}}
}
\keyword{internal}
