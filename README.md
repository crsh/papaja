# R2APA
For now, R2APA is a template for [pandoc](http://johnmacfarlane.net/pandoc/) using the LaTeX [apa6-package](http://www.ctan.org/pkg/apa6) to produce documents that conform with the American Psychology Association (APA) manuscript guidelines (6th Edition) from RMarkdown-files. In the future, I hope to extend this project to include R functions to report statistics and more.

# Example
Take a look at the [example document](https://github.com/crsh/R2APA/blob/master/example/example.pdf) created in [RStudio](http://www.rstudio.com/) using this template.

# Setup
To start writing your manuscript, simply copy the files in the install-folder to your computer and start writing in `manuscript.rmd`. Before knitting the manuscript make sure you specify a .bib-file in the document header or remove the `bibliography` parameter.