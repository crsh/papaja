# papaja: Prepare APA Journal Articles
papaja is a R-package in the making including a [RMarkdown](http://rmarkdown.rstudio.com/) template that can be used with (or without) [RStudio](http://www.rstudio.com/) to produce complete manuscripts, which conform to the American Psychological Association (APA) manuscript guidelines (6th Edition). The package uses the LaTeX document class [apa6](http://www.ctan.org/pkg/apa6) and a .docx-reference file, so you can create beautiful PDF documents or Word documents if you have to. papaja also supplies R-functions that are meant to facilitate the reporting of statistics in accordance with APA guidelines.

**Note, at this point papaja is in active development and should be considered alpha. If you experience any problems, please [open an issue](https://github.com/crsh/papaja/issues) on Github.**
  
## Examples
Take a look at the [.Rmd](https://github.com/crsh/papaja/blob/master/example/example.Rmd) of the example manuscript in the folder `example` and the resulting [.pdf](https://raw.githubusercontent.com/crsh/papaja/master/example/example.pdf). The example document also contains some basic instructions.

## Setup
### Requirements
To use papaja you need to make sure the following software is installed on your computer:
  
  - [R](http://www.r-project.org/) (2.11.1 or later)
- [RStudio](http://www.rstudio.com/) (0.98.932 or later) is optional; if you don't use RStudio, you need to install [pandoc](http://johnmacfarlane.net/pandoc/) using the [instructions for your operating system](https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md)
- The `devtools` package (`install.packages("devtools")`) to install papaja from Github
- A [TeX](http://de.wikipedia.org/wiki/TeX) distribution (2013 or later; e.g., [MikTeX](http://miktex.org/) for Windows, [MacTeX](https://tug.org/mactex/) for Mac, obviously, or [TeX Live](http://www.tug.org/texlive/) for Linux)
- If you are running **Windows**, use MikTex if possible. Currently, pandoc and the Windows version of Tex Live [don't seem to like each other](https://github.com/rstudio/rmarkdown/issues/6). Make sure you install the *complete*---not the basic---version.
- If you are running **Ubuntu 14.04** you need a couple of TeX packages in addition to the already installed ones for the document class `apa6` to work:
  
  ```S
sudo apt-get install texlive texlive-latex-extra texlive-bibtex-extra texlive-publishers texlive-fonts-recommended texlive-fonts-extra texlive-humanities
````

### Install papaja
Once all that is taken care of, install papaja from GitHub:
  
  ```S
devtools::install_github("crsh/papaja")
```

## Create a manuscript
Once papaja is installed, you can select the APA template when creating a new Markdown file through the menus in RStudio.

![APA template selection](https://www.dropbox.com/s/f1q6sb0ma0ligm9/template_selection.png?dl=1)

If you want to add citations specify your .bib-file in the YAML header of the document (`bibliography: my.bib`) and you can start citing. If necessary, have a look at RMarkdown's [overview of the citation syntax](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html).

### Helper functions to report analyses
The functions `apa_print()` and `apa_table()` facilitate reporting of results. Take a look at the [.Rmd](https://github.com/crsh/papaja/blob/master/example/example.Rmd) of the example manuscript in the folder `example` and the resulting [.pdf](https://raw.githubusercontent.com/crsh/papaja/master/example/example.pdf).

Drop a supported analysis result, such as an `htest`- or `lm`-object into `apa_print()` and receive a list of possible character strings that you can use to report the results of your analysis.

```S
my_lm <- lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
apa_lm <- apa_print(my_lm)
```

One element of this list is `apa_lm$table` that, in the case of an `lm`-object, will contain a complete regression table. Pass `apa_lm$table` to `apa_table()` to turn it into a proper table in your PDF or Word document (remember to set the chunk option `results = "asis"`).

```S
apa_table(apa_lm$table)
```

Be sure to also check out `apa_barplot()`.

### Using papaja without RStudio
You don't use RStudio? No problem. Use the `rmarkdown::render` function to create articles:

```S
rmarkdown::render("mymanuscript.Rmd")
```

## Papers that use papaja
Although papaja is not yet on CRAN and is still undergoing a lot of changes, there are already peer-reviewed publications that use it. If you have published a paper that was written with papaja, let me know and I will add it to this list.

Stahl, C., Barth, M., & Haider, H. (2015). Distorted estimates of implicit and explicit learning in applications of the process-dissociation procedure to the SRT task. *Consciousness & Cognition*, 37, 27–43. doi:[10.1016/j.concog.2015.08.003](http://dx.doi.org/10.1016/j.concog.2015.08.003)


## Contribute
Like papaja and want to contribute? Take a look at the [open issues](https://github.com/crsh/papaja/issues) if you need inspiration. Other than that, there are many output objects from analysis methods that we would like `apa_print()` to support. Any new S3-methods for this function are always appreciated (e.g., `glm`, `factanal`, `fa`, `lavaan`, `BFBayesFactor`).


## Known issues
- The references violates the APA guidelines in that there is no hanging indentation (i.e. indentation of all lines but the first one). See the [related issue](https://github.com/crsh/papaja/issues/37) for a fix.
- If building the example manuscript throws the error `! Incomplete \iffalse; all text was ignored after line 20.` make sure you have saved the .Rmd and are not working from a temporary directory or restart RStudio. If this doesn't work try updating your TeX-packages.
- Citations may mess with RStudios syntax highlighting in the current line. Incorrect highlighting following a citation does not necessarily indicate incorrect syntax.
- Printing PDF from RStudio's PDF viewer can produce weird results. If you want to print your manuscript I suggest you use any other PDF viewer of your choice.
- When creating a word document on Windows, special characters in title, abstract, and note are not rendered correctly. This is a bug in [`r-yaml`](https://github.com/viking/r-yaml/issues/6) and has been for a while, but it will soon be addressed by a [workaround](https://github.com/rstudio/rmarkdown/issues/420) in `knitr`.

# Other journal templates
Obviously, not all journals require manuscripts and articles to be prepared according to APA guidelines. If you are looking for other journal article templates, the following list of `rmarkdown`/`pandoc` packages and templates may be helpful. If you know of other packages and templates, drop us a note, so we can add them here.

- [rticles](https://github.com/rstudio/rticles): The rticles package includes a set of R Markdown templates that enable authoring of R related journal and conference submissions.
- [Michael Sachs' pandoc journal templates](https://github.com/sachsmc/pandoc-journal-templates): Pandoc templates for the major statistics and biostatistics journals

Finally, in case you prefer to work with Python, have a look at the [Academic Markdown](https://github.com/smathot/academicmarkdown)-module.
