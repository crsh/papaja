#' APA manuscripts from R Markdown
#'
#' \pkg{papaja} is a R-package in the making including a \href{http://rmarkdown.rstudio.com/}{R Markdown} template
#' that can be used with (or without) \href{http://www.rstudio.com/}{RStudio} to produce documents, which conform to
#' the American Psychological Association (APA, 2010) manuscript guidelines (6th Edition). The package uses the LaTeX
#' document class \href{http://www.ctan.org/pkg/apa6}{apa6} and a .docx-reference file, so you can create PDF documents,
#' or Word documents if you have to. Moreover, `papaja` supplies R-functions (e.g., \code{\link{apa_print}}) that
#' facilitate reporting results of your analyses in accordance with APA guidelines.
#'
#' Note, at this point `papaja` is in active development and should be considered alpha. If you experience any problems,
#' please \href{https://github.com/crsh/papaja/issues}{open an issue} with a reproducible example of the encountered problem.
#'
#' @section Functions:
#'    To get a list of all functions try \code{ls("package:papaja")} or click the Index link at the bottom of this document.
#' @section System requirements:
#'    To use \pkg{papaja} you need to make sure the following software is installed on your computer:
#'
#'    \itemize{
#'        \item{\href{http://www.rstudio.com/}{RStudio} (>= 0.98.932) or if you don't use RStudio
#'            , you need to install \href{http://johnmacfarlane.net/pandoc/}{pandoc} using the
#'            \href{https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md}{instructions for your operating system}}
#'        \item{A \href{http://de.wikipedia.org/wiki/TeX}{TeX} distribution (2013 or later; e.g., \href{http://miktex.org/}{MikTeX}
#'            for Windows, \href{https://tug.org/mactex/}{MacTeX} for Mac, obviously, or \href{http://www.tug.org/texlive/}{TeX Live}
#'            for Linux)}
#'    }
#'
#'    \emph{Windows} users should use MikTeX if possible. Currently, pandoc and the Windows version of Tex Live
#'    \href{https://github.com/rstudio/rmarkdown/issues/6}{don't seem to like each other}. Make sure you install the
#'    \emph{complete}---not the basic---version.
#'
#'    \emph{Ubuntu 14.04} users need a few additional TeX packages for the document class
#'    \href{http://www.ctan.org/pkg/apa6}{apa6} to work:
#'
#'    \code{sudo apt-get install texlive texlive-publishers texlive-fonts-extra texlive-latex-extra texlive-humanities lmodern}
#'
#' @section Authors:
#'    Frederik Aust (frederik.aust at uni-koeln.de).
#'    Marius Barth (marius.barth at uni-koeln.de).
#' @section Maintainer:
#'    Frederik Aust (frederik.aust at uni-koeln.de).
#' @section Contributors:
#'  \itemize{
#'    \item{Birk Diedenhofen}
#'    \item{Christoph Stahl}
#'  }
#' @references
#' American Psychological Association. (2010). Publication Manual of the American Psychological Association (6th edition).
#' Washington, DC: American Psychological Association.
#' @docType package
#' @name papaja

NULL
