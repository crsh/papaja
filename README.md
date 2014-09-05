# R2APA
R2APA is a [pandoc](http://johnmacfarlane.net/pandoc/) template that can be used with [RStudio](http://www.rstudio.com/) to produce complete manscripts (in PDF format), which conform with the American Psychology Association (APA) manuscript guidelines (6th Edition), from [RMarkdown](http://rmarkdown.rstudio.com/)-files. To do so, R2APA uses the LaTeX document class [apa6](http://www.ctan.org/pkg/apa6).

In the future, I hope to add R functions to easily report statistics and wrap everything up nicely in an R-Package to facilitate the setup. Any contributions are welcome.

## Example
Take a look at the [.pdf](https://raw.githubusercontent.com/crsh/R2APA/master/example/example.pdf) and the [.rmd](https://github.com/crsh/R2APA/blob/master/example/example.rmd) of the example manuscript I created using this template.

## Setup
### Requirements
To create an APA-manuscript, make sure the following software is installed on your computer:

- [R](http://www.r-project.org/) (2.11.1 or later)
- [RStudio](http://www.rstudio.com/) (0.98.932 or later)
- The R-Package [`knitr`](http://cran.r-project.org/web/packages/knitr/index.html)
- A [TeX](http://de.wikipedia.org/wiki/TeX) distribution (2013 or later; e.g., [TeX Live](http://www.tug.org/texlive/), [MacTeX](https://tug.org/mactex/), or [MikTeX](http://miktex.org/))
  - If you are running Ubuntu 14.04 you need a couple extra `texlive` packages for the document class `apa6` to work:
        apt-get install texlive texlive-latex-extra texlive-bibtex-extra texlive-publishers texlive-fonts-recommended texlive-fonts-extra texlive-humanities

### Creating a manuscript
To start writing your manuscript, download `r2apa.zip` and extract the files to your computer. You can now fire up RStudio and start writing by editing `manuscript.rmd`. Before clicking RStudio's knit-button make sure you specify a .bib-file in the document header or remove the `bibliography` parameter.

## Known issues
- Citations may mess with RStudios syntax highlighting in the current line. Incorrect highlighting following a citation does not necessarily indicate incorrect syntax.
- When using the R-Package [`xtable`](http://cran.r-project.org/web/packages/xtable/index.html) to produce LaTeX-tables, the caption is set to the left page margin (s. the [example manuscript](https://github.com/crsh/R2APA/blob/master/example/example.pdf)). This is a more general issue of the `apa6` document class. It can be resolved using `\captionbox` instead of `\caption` (see my [SE question](http://tex.stackexchange.com/questions/42209/centering-tables-in-document-class-apa6)), which is AFAIK not possible with `xtable()`. You may try out my helper function `apa.table()` in the folder `helper`, which should do the trick.
- Printing PDF from RStudio's PDF viewer can produce weird results. If you want to print your manuscript I suggest you use any other PDF viewer of your choice.
