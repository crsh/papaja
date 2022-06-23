#' Prepare APA Journal Articles with R Markdown
#'
#' `papaja` is an [award-winning](https://improvingpsych.org/mission/awards/) R package that facilitates creating computationally reproducible, submission-ready manuscripts which conform to the American Psychological Association (APA) manuscript guidelines (6th Edition).
#'
#' @details
#'
#' `papaja` provides
#'
#' - an [R Markdown](https://rmarkdown.rstudio.com/) template that can be used with (or without) [RStudio](https://www.rstudio.com/) to create PDF documents (using the [apa6](https://ctan.org/pkg/apa6) LaTeX class) or Word documents (using a .docx-reference file).
#' - Functions to *typeset* the results from *statistical analyses* (e.g., [apa_print()]),
#' - functions to create *tables* ([apa_table()]), and
#' - functions to create *figures* in accordance with APA guidelines (e.g., [apa_factorial_plot()]).
#'
#' @section System requirements:
#'   To use `papaja` you need either an up-to-date version of
#'   [RStudio](https://www.rstudio.com/) or
#'   [pandoc](https://pandoc.org/). If you want to create PDF-
#'   in addition to DOCX-documents you additionally need a
#'   [TeX](https://en.wikipedia.org/wiki/TeX) distribution. We recommend
#'   [TinyTex](https://yihui.org/tinytex/), which can be installed from within
#'   R via the \pkg{tinytex} package.
#'
#'   Please refer to the [`papaja` manual](http://frederikaust.com/papaja_man/introduction.html#getting-started)
#'   for detailed installation instructions.
#'
#' @section Getting help:
#' For a comprehensive introduction to `papaja`, see the current draft of the [manual](http://frederikaust.com/papaja_man/).
#' If you have a specific question that is not answered in the manual, feel free to ask a question on Stack Overflow [using the `papaja` tag](https://stackoverflow.com/questions/tagged/papaja).
#' If you believe you have found a bug or would like to request a new feature, [open an issue](https://github.com/crsh/papaja/issues) on Github and provide a [minimal complete verifiable example](https://stackoverflow.com/help/mcve).
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
