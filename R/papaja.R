#' APA manuscripts from RMarkdown
#'
#' A R-package in the making including a \href{http://rmarkdown.rstudio.com/}{RMarkdown} template
#' to produce complete manuscripts (PDF and Word documents), which conform to the American Psychological Association (APA)
#' manuscript guidelines (6th Edition). To do so, \pkg{papaja} uses the LaTeX document class
#' \href{http://www.ctan.org/pkg/apa6}{apa6} and a .docx-reference file. The supplied R-functions (e.g., \code{\link{apa_print}}
#' or \code{\link{apa_table}} facilitate the reporting of statistics in accordance with APA guidelines.
#'
#' Note, at this point coversion to MS word is considered experimental. If you experience any problems, please
#' \href{https://github.com/crsh/papaja/issues}{open an issue} with a reproducible example of the encountered problem.
#' @section Functions:
#'    To get a list of all functions try \code{ls("package:papaja")}.
#' @section System requirements:
#'    Before using \pkg{papaja} to create a manuscript, make sure the following software is installed on your computer:
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
#'    If you are running \emph{Windows}, use MikTex if possible. Currently, pandoc and the Windows version of Tex Live
#'    \href{https://github.com/rstudio/rmarkdown/issues/6}{don't seem to like each other}. Make sure you install the
#'    \emph{complete}---not the basic---version.
#'
#'    If you are running \emph{Ubuntu 14.04} you need a couple of TeX packages in addition to the already installed ones for the document
#'    class \href{http://www.ctan.org/pkg/apa6}{apa6} to work:
#'
#'    \code{sudo apt-get install texlive texlive-latex-extra texlive-bibtex-extra texlive-publishers texlive-fonts-recommended
#'    texlive-fonts-extra texlive-humanities}
#'
#' @section Authors:
#'    Frederik Aust (frederik.aust at uni-koeln.de).
#'    Marius Barth (marius.bart at uni-koeln.de).
#' @section Maintainer:
#'    Frederik Aust (frederik.aust at uni-koeln.de).
#' @section Contributors:
#'  \itemize{
#'    \item{Birk Diedenhofen}
#'  }
#' @references
#' American Psychological Association. (2010). Publication Manual of the American Psychological Association (6th edition).
#' Washington, DC: American Psychological Association.
#' @docType package
#' @name papaja

NULL
