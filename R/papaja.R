#' Prepare APA Journal Articles with R Markdown
#'
#' `papaja` is an R package that facilitates creating computationally reproducible, submission-ready manuscripts which conform to the American Psychological Association (APA) manuscript guidelines (6th Edition).
#'
#' @details
#'
#' `papaja` provides
#'
#' - an [R Markdown](http://rmarkdown.rstudio.com/) template that can be used with (or without) [RStudio](http://www.rstudio.com/) to create PDF documents (using the [apa6](http://www.ctan.org/pkg/apa6) LaTeX class) or Word documents (using a .docx-reference file).
#' - Functions to *typeset* the results from *statistical analyses* (e.g., [apa_print()]),
#' - functions to create *tables* ([apa_table()]), and
#' - functions to create *figures* in accordance with APA guidelines (e.g., [apa_factorial_plot()]).
#'
#' @section System requirements:
#'   To use `papaja` you need either an up-to-date version of
#'   [RStudio](http://www.rstudio.com/) or
#'   [pandoc](http://johnmacfarlane.net/pandoc/). If you want to create PDF-
#'   in addition to DOCX-documents you additionally need a
#'   [TeX](http://de.wikipedia.org/wiki/TeX) distribution. We recommend
#'   [TinyTex](https://yihui.name/tinytex/), which can be installed from within
#'   R via the \pkg{tinytex} package.
#'
#'   Please refer to the [`papaja` manual](https://crsh.github.io/papaja_man/introduction.html#getting-started)
#'   for detailed installation instructions.
#'
#' @section Getting help:
#' For a comprehensive introduction to `papaja`, check out the current draft of the [`papaja` manual](https://crsh.github.io/papaja_man/).
#' If you have a specific question that is not answered in the manual, feel free to ask a question on Stack Overflow [using the `papaja` tag](https://t.co/Z3auyUrbTa).
#' If you believe you have found a bug or you want to request a new feature, [open an issue](https://github.com/crsh/papaja/issues) on Github and provide a [minimal complete verifiable example](https://stackoverflow.com/help/mcve).
#'
#' @section Authors:
#'    Frederik Aust (frederik.aust at uni-koeln.de).
#'    Marius Barth (marius.barth at uni-koeln.de).
#' @section Maintainer:
#'    Frederik Aust (frederik.aust at uni-koeln.de).
#'
#' @docType package
#' @name papaja

NULL
