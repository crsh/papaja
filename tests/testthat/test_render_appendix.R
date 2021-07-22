
test_that(
  "Unicode (Russian) in appendix files"
  , {

    op <- knitr::opts_knit$get()
    knitr::opts_knit$set(
      rmarkdown.pandoc.to = "latex"
      , output.dir = "inst"
    )
    oldwd <- getwd()
    papaja::render_appendix("appendix.rmd")
    setwd(oldwd)
    knitr::opts_knit$set(op)

    testthat::expect_identical(
      object = papaja:::readLines_utf8("inst/appendix.tex")
      , expected = c(
        "\\clearpage"
        , "", "", ""
        , "\\begin{appendix}"
        , "\\section{}"
        , "Hello, world! Привет, мир."
        , "\\end{appendix}"
      )
    )
  }
)
