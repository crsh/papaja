# papaja: Create APA manuscripts with RMarkdown
papaja is a R-package in the making including a [RMarkdown](http://rmarkdown.rstudio.com/) template that can be used with [RStudio](http://www.rstudio.com/) (or without) to produce complete manuscripts (PDF and Word documents), which conform to the American Psychological Association (APA) manuscript guidelines (6th Edition). To do so, papaja uses the LaTeX document class [apa6](http://www.ctan.org/pkg/apa6) and a .docx-reference file. The supplied R-functions are ment to facilitate the reporting of statistics in accordance with APA guidelines.

Note, at this point `papaja` is in active development and should be considered alpha. If you experience any problems, please [open an issue](https://github.com/crsh/papaja/issues).

If you are looking for other journal article templates, take a look at [the list below](#other-journal-templates). Finally, in case you prefer to work with Python, have a look at the [Academic Markdown](https://github.com/smathot/academicmarkdown)-module.

## Examples
Take a look at the [.rmd](https://github.com/crsh/papaja/blob/master/example/example.Rmd) of the example manuscript in the folder `example` and the resulting [.pdf](https://raw.githubusercontent.com/crsh/papaja/master/example/example.pdf). More examples to come.

The example document also contains some basic instructions.

## Setup
### Requirements
Before using `papaja` to create an APA-manuscript, make sure the following software is installed on your computer:

- [R](http://www.r-project.org/) (2.11.1 or later)
- [RStudio](http://www.rstudio.com/) (0.98.932 or later) is optional; if you don't use RStudio, you need to install [pandoc](http://johnmacfarlane.net/pandoc/) using the [instructions for your operating system](https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md)
- A [TeX](http://de.wikipedia.org/wiki/TeX) distribution (2013 or later; e.g., [MikTeX](http://miktex.org/) for Windows, [MacTeX](https://tug.org/mactex/) for Mac, obviously, or [TeX Live](http://www.tug.org/texlive/) for Linux)
  - If you are running **Windows**, use MikTex if possible. Currently, pandoc and the Windows version of Tex Live [don't seem to like each other](https://github.com/rstudio/rmarkdown/issues/6). Make sure you install the *complete*---not the basic---version.
  - If you are running **Ubuntu 14.04** you need a couple of TeX packages in addition to the already installed ones for the document class `apa6` to work:

```S
sudo apt-get install texlive texlive-latex-extra texlive-bibtex-extra texlive-publishers texlive-fonts-recommended texlive-fonts-extra texlive-humanities
````

### Install papaja
Finally install `papaja` from this GitHub repository:

```S
devtools::install_github("crsh/papaja")
```

### Create a manuscript
Once you have installed the `papaja` package you can select the APA template when creating a new Markdown file through the menus in RStudio.

![APA template selection](https://www.dropbox.com/s/f1q6sb0ma0ligm9/template_selection.png?dl=1)

If you want to add citations specify your .bib-file in the document header and under the `bibliography` parameter and you can start citing.

The supplied functions `apa_print()` and `apa_table()` facilitate reporting of results.

#### Using papaja without RStudio
In addition to the above, you need to do the following to use `papaja` without RStudio:

- Install the `rmarkdown` package:

```S
install.packages("rmarkdown")
```

- Use the `rmarkdown::render` function to create articles:

```S
rmarkdown::render("mymanuscript.Rmd")
```

## Known issues
- The references violates the APA guidelines in that there is no hanging indentation (i.e. indentation of all lines but the first one). See the [related issue](https://github.com/crsh/papaja/issues/37) for a fix.
- If building the example manuscript throws the error `! Incomplete \iffalse; all text was ignored after line 20.` make sure you have saved the .Rmd and are not working from a temporary directory or restart RStudio. If this doesn't work try updating your TeX-packages.
- Citations may mess with RStudios syntax highlighting in the current line. Incorrect highlighting following a citation does not necessarily indicate incorrect syntax.
- Printing PDF from RStudio's PDF viewer can produce weird results. If you want to print your manuscript I suggest you use any other PDF viewer of your choice.
- When creating a word document on Windows, special characters in title, abstract, and note are not rendered correctly. This is a bug in [`r-yaml`](https://github.com/viking/r-yaml/issues/6) and has been for a while, but it will soon be addressed by a [workaround](https://github.com/rstudio/rmarkdown/issues/420) in `knitr`.

## Other journal templates
Obiously, not all journals require manuscripts and articles to be prepared according to APA guidelines. If you are looking for other journal article templates, the following list of other `rmarkdown`/`pandoc` packages and templates may be helpful:

- [rticles](https://github.com/rstudio/rticles): The rticles package includes a set of R Markdown templates that enable authoring of R related journal and conference submissions.
- [Michael Sachs' pandoc journal templates](https://github.com/sachsmc/pandoc-journal-templates): Pandoc templates for the major statistics and biostatistics journals

If you know of other packages and templates, drop us a note, so we can add them here.
