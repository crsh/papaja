% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apa_print_glht.R
\name{apa_print.glht}
\alias{apa_print.glht}
\alias{apa_print.summary.glht}
\title{Typeset Statistical Results from General Linear Hypothesis Tests}
\usage{
\method{apa_print}{glht}(x, test = multcomp::adjusted(), ...)

\method{apa_print}{summary.glht}(x, conf.int = 0.95, in_paren = FALSE, ...)
}
\arguments{
\item{x}{Object}

\item{test}{Function. Computes p-values (adjusted for multiple comparisons).}

\item{...}{Further arguments to pass to \code{\link{apa_num}} to format the estimate.}

\item{conf.int}{Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.}

\item{in_paren}{Logical. Whether the formatted string is to be reported in
parentheses. If \code{TRUE}, parentheses in the formatted string (e.g., those
enclosing degrees of freedom) are replaced with brackets.}
}
\description{
\emph{These methods are not properly tested and should be
considered experimental.}
}
\examples{
   # From the multcomp::glht() examples:
   library(multcomp)
   amod <- aov(breaks ~ tension, data = warpbreaks)
   glht_out <- glht(amod, linfct = mcp(tension = "Tukey"))
   apa_print(glht_out)

  # In this example, because degrees of freedom are equal across all rows
  # of the output, it is possible to move that information to the variable
  # labels. This is useful if a compact results table is required:

  df_into_label(apa_print(glht_out))
}
\seealso{
Other apa_print: 
\code{\link{apa_print}()},
\code{\link{apa_print.BFBayesFactor}()},
\code{\link{apa_print.aov}()},
\code{\link{apa_print.emmGrid}()},
\code{\link{apa_print.htest}()},
\code{\link{apa_print.list}()},
\code{\link{apa_print.lm}()},
\code{\link{apa_print.lme}()},
\code{\link{apa_print.merMod}()}
}
\concept{apa_print}
\value{
\code{apa_print()}-methods return a named list of class \code{apa_results} containing the following elements:

\item{estimate}{One or more character strings giving point estimates, confidence intervals, and confidence level. A single string is returned in a vector; multiple strings are returned as a named list. If no estimate is available the element is \code{NULL}.}
\item{statistic}{One or more character strings giving the test statistic, parameters (e.g., degrees of freedom), and p-value. A single string is returned in a vector; multiple strings are returned as a named list. If no estimate is available the element is \code{NULL}.}
\item{full_result}{One or more character strings comprised `estimate` and `statistic`. A single string is returned in a vector; multiple strings are returned as a named list. }
\item{table}{A \code{data.frame} of class \code{apa_results_table} that contains all elements of \code{estimate} and \code{statistics}. This table can be passed to \code{apa_table()} for reporting.}

Column names in \code{apa_results_table} are standardized following the \href{https://www.tidymodels.org/learn/develop/broom/#glossary}{\pkg{broom} glossary} (e.g., \code{term}, \code{estimate} \code{conf.int}, \code{statistic}, \code{df}, \code{df.residual}, \code{p.value}). Additionally, each column is labelled (e.g., \code{$\\hat{\\eta}^2_G$} or \code{$t$}) using the \pkg{tinylabels} package and these labels are used as column names when an \code{apa_results_table} is passed to \code{apa_table()}.

}
