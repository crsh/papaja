papaja: Prepare APA journal articles with R Markdown
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN/METACRAN](https://img.shields.io/cran/v/papaja?label=CRAN&logo=r)](https://cran.r-project.org/web/packages/papaja/index.html)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
![GitHub last commit
(devel)](https://img.shields.io/github/last-commit/crsh/papaja/devel?label=Last%20commit&logo=github&logoColor=%23FFF)
[![R-CMD-check](https://github.com/crsh/papaja/workflows/R-CMD-check/badge.svg)](https://github.com/crsh/papaja/actions)
[![codecov](https://codecov.io/gh/crsh/papaja/branch/master/graph/badge.svg)](https://codecov.io/gh/crsh/papaja)
[![GitHub bug
issues](https://img.shields.io/github/issues/crsh/papaja/bug?label=Bugs&logo=github&logoColor=%23FFF)](https://github.com/crsh/papaja/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
![StackOverflow
questions](https://img.shields.io/stackexchange/stackoverflow/t/papaja?label=%20Questions&logo=stackoverflow&logoColor=%23FFF)

`papaja` is an R package that facilitates creating reproducible,
submission-ready manuscripts which conform to the American Psychological
Association (APA) manuscript guidelines (6th Edition). `papaja` provides

-   an [R Markdown](http://rmarkdown.rstudio.com/) template that can be
    used with (or without) [RStudio](http://www.rstudio.com/) to create
    PDF documents (using the [apa6](http://www.ctan.org/pkg/apa6) LaTeX
    class) or Word documents (using a .docx-reference file).
-   Functions to **typeset** the results from **statistical analyses**,
-   functions to create **tables**, and
-   functions to create **figures** in accordance with APA guidelines.

## Example

Take a look at the [R Markdown
file](https://github.com/crsh/papaja/blob/master/inst/example/example.rmd)
of the example manuscript in the folder `example` and the resulting
[PDF](https://raw.githubusercontent.com/crsh/papaja/master/inst/example/example.pdf).
The example document also contains some basic instructions. For an
in-depth introduction to `papaja`, check out the current draft of the
[manual](https://crsh.github.io/papaja_man/). If the manual does not
answer your question, ask a question on Stack Overflow [using the
`papaja` tag](https://t.co/Z3auyUrbTa). If you believe your problem is
related to a bug or you want to request a new feature, [open an
issue](https://github.com/crsh/papaja/issues) on Github.

## Installation

To use `papaja` you need either an up-to-date version of
[RStudio](http://www.rstudio.com/) or
[pandoc](http://johnmacfarlane.net/pandoc/). If you want to create PDF-
in addition to DOCX-documents you additionally need a
[TeX](http://de.wikipedia.org/wiki/TeX) distribution. If you have no use
for TeX beyond rendering R Markdown documents, I recommend you use
[TinyTex](https://yihui.name/tinytex/). TinyTex can be installed from
within R as follows.

``` r
if(!"tinytex" %in% rownames(installed.packages())) install.packages("tinytex")

tinytex::install_tinytex()
```

Otherwise consider [MikTeX](http://miktex.org/) for Windows,
[MacTeX](https://tug.org/mactex/) for Mac, or [TeX
Live](http://www.tug.org/texlive/) for Linux. Please refer to the
[`papaja`
manual](https://crsh.github.io/papaja_man/introduction.html#getting-started)
for detailed installation instructions.

`papaja` is not yet available on CRAN but you can install it from this
repository:

``` r
# Install remotes package if necessary
if(!"remotes" %in% rownames(installed.packages())) install.packages("remotes")

# Install the stable development version from GitHub
remotes::install_github("crsh/papaja")

# Install the latest development snapshot from GitHub
remotes::install_github("crsh/papaja@devel")
```

## How to use papaja

Once `papaja` is installed, you can select the APA template when
creating a new R Markdown file through the RStudio menus.

![APA template selection](inst/images/template_selection.png)

If you want to add citations specify your BibTeX-file in the YAML front
matter of the document (`bibliography: my.bib`) and you can start
citing. If necessary, have a look at R Markdown’s [overview of the
citation
syntax](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html).
You may also be interested in [citr](https://github.com/crsh/citr), an R
Studio addin to swiftly insert Markdown citations.

### Helper functions to report analyses

The functions `apa_print()` and `apa_table()` facilitate reporting
results of your analyses. Take a look at the [R
Markdown-file](https://github.com/crsh/papaja/blob/master/inst/example/example.Rmd)
of the example manuscript in the folder `example` and the resulting
[PDF](https://raw.githubusercontent.com/crsh/papaja/master/inst/example/example.pdf).

Drop a supported analysis result, such as an `htest`- or `lm`-object,
into `apa_print()` and receive a list of possible character strings that
you can use to report the results of your analysis.

``` r
my_lm <- lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
apa_lm <- apa_print(my_lm)
```

`papaja` currently provides methods for the following object classes:

| A-B                            | B-L                           | L-S                       | S-Z                           |
|:-------------------------------|:------------------------------|:--------------------------|:------------------------------|
| afex\_aov                      | BFBayesFactorTop<sup>\*</sup> | lme                       | summary.aov                   |
| anova                          | default                       | lsmobj<sup>\*</sup>       | summary.aovlist               |
| anova.lme                      | emmGrid<sup>\*</sup>          | manova                    | summary.glht<sup>\*</sup>     |
| Anova.mlm                      | glht<sup>\*</sup>             | merMod                    | summary.glm                   |
| aov                            | glm                           | mixed                     | summary.lm                    |
| aovlist                        | htest                         | papaja\_wsci              | summary.manova                |
| BFBayesFactor<sup>\*</sup>     | list                          | summary\_emm<sup>\*</sup> | summary.ref.grid<sup>\*</sup> |
| BFBayesFactorList<sup>\*</sup> | lm                            | summary.Anova.mlm         |                               |

\* Not fully tested, don’t trust blindly!

### Tables

Function `apa_table()` may be used to include publication-ready tables
in your manuscript. For instance, you might want to report some
condition means (with standard errors).

``` r
library(dplyr)
npk %>%
  group_by(N, P) %>%
  summarise(mean = mean(yield), se = se(yield), .groups = "drop") %>%
  label_variables(N = "Nitrogen", P = "Phosphate", mean = "*M*", se = "*SE*") %>%
  apa_table(caption = "Mean pea yield (with standard errors)")
```

| Nitrogen | Phosphate |  *M*  | *SE* |
|:--------:|:---------:|:-----:|:----:|
|    0     |     0     | 51.72 | 1.88 |
|    0     |     1     | 52.42 | 2.65 |
|    1     |     0     | 59.22 | 2.66 |
|    1     |     1     | 56.15 | 2.08 |

Table 1. *Mean pea yield (with standard errors)*

This is a fairly simple example, but `apa_table()` may be used to
generate massive tables.

`apa_table()` is also able to include the output from `apa_print()` into
the manuscript. Thus, it is possible to conveniently include complete
regression tables, ANOVA tables, or the output from mixed-effects
models.

``` r
lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris) |>
  apa_print() |>
  apa_table(caption = "Iris regression table.")
```

<!-- GitHub markdown doesn't support MathJax -->

------------------------------------------------------------------------

Table. *Iris regression table.*

| Predictor    |   *b* |      95% CI      | *t(146)* |       *p* |
|:-------------|------:|:----------------:|---------:|----------:|
| Intercept    |  1.04 |  \[0.51, 1.58\]  |     3.85 | &lt; .001 |
| Sepal Length |  0.61 |  \[0.48, 0.73\]  |     9.77 | &lt; .001 |
| Petal Width  |  0.56 |  \[0.32, 0.80\]  |     4.55 | &lt; .001 |
| Petal Length | -0.59 | \[-0.71, -0.46\] |    -9.43 | &lt; .001 |

------------------------------------------------------------------------

### Figures

Be sure to also check out `apa_barplot()`, `apa_lineplot()`, and
`apa_beeplot()` (or the general function `apa_factorial_plot()`) if you
work with factorial designs:

``` r
par(las = 1)

apa_beeplot(
  data = stroop_data
  , dv = "response_time"
  , id = "id"
  , factors = c("congruency", "load")
  , ylim = c(0, 800)
  , dispersion = wsci # within-subjects confidence intervals
  , conf.level = .99
)
```

![Response times from a simulated Stroop experiment. Large dots
represent condition means, small dots represent individual participants’
mean response time. Error bars represent 99% within-subjects confidence
intervals.](README_files/figure-gfm/stroop-plot-1.png)

If you prefer creating your plots with `ggplot2`, try `theme_apa()`.

### Using papaja without RStudio

Don’t use RStudio? No problem. Use the `rmarkdown::render` function to
create articles:

``` r
# Create new R Markdown file
rmarkdown::draft(
  "mymanuscript.Rmd"
  , "apa6"
  , package = "papaja"
  , create_dir = FALSE
  , edit = FALSE
)

# Render manuscript
rmarkdown::render("mymanuscript.Rmd")
```

### Computational reproducibility

To ensure mid- to long-term computational reproducibility we highly
recommend conserving the software environment used to write a manuscript
(e.g. R and all R packages) either in a software container or a virtual
machine. This way you can be sure that your R code does not break
because of updates to R or any R package. For a brief primer on
containers and virtual machines see [the supplementary
material](https://psych-transparency-guide.uni-koeln.de/analytic-reproducibility.html#document-hardware-and-software-used-for-analyses)
by Klein et al. (2018).

[Docker](https://www.docker.com/) is the most widely used
containerization approach. It is open source and free to use but
requires some disk space. [CodeOcean](https://codeocean.com/) is a
commercial service that builds on Docker, facilitates setting up and
sharing containers and lets you run computations in the cloud. See the
manual on [how to get started using `papaja` with Docker or
CodeOcean](https://crsh.github.io/papaja_man/tips-and-tricks.html#reproducible-software-environments).

## Getting help

![StackOverflow
questions](https://img.shields.io/stackexchange/stackoverflow/t/papaja?label=%20Questions&logo=stackoverflow&logoColor=%23FFF)

For an in-depth introduction to `papaja`, check out the current draft of
the [manual](https://crsh.github.io/papaja_man/). If you have questions
related to the use of `papaja` that are not answered in the manual,
[StackOverflow](https://stackoverflow.com/questions/tagged/papaja) has a
[`papaja`-tag](https://stackoverflow.com/questions/tagged/papaja) and is
a great place to get answers. If you think you have found a bug, please
[open issues](https://github.com/crsh/papaja/issues) and provide a
[minimal complete verifiable
example](https://stackoverflow.com/help/mcve).

## Contribute

[![GitHub help wanted
issues](https://img.shields.io/github/issues/crsh/papaja/help%20wanted?logo=github&logoColor=%23FFF)](https://github.com/crsh/papaja/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22)
[![GitHub documentation
issues](https://img.shields.io/github/issues/crsh/papaja/documentation?logo=github&logoColor=%23FFF)](https://github.com/crsh/papaja/issues?q=is%3Aopen+is%3Aissue+label%3Adocumentation)

Like `papaja` and want to contribute? We highly appreciate any
contributions to the R package or its documentation. Take a look at the
[open issues](https://github.com/crsh/papaja/issues) if you need
inspiration. There are also many additional analyses that we would like
`apa_print()` to support. Any new S3/S4-methods for this function are
always appreciated (e.g., `factanal`, `fa`, `lavaan`).

## Papers written with papaja

Please cite `papaja` if you use it. You can for example [use
`cite_r()`](https://crsh.github.io/papaja_man/writing.html#citing-r-and-its-packages)
or copy the reference information returned by `citation('papaja')` to
your BibTeX file. Below are some (randomly selected) peer-reviewed
publications that used `papaja`. If you have published a paper that was
written with `papaja`, you can add the reference to the [public Zotero
group](https://www.zotero.org/groups/2202906/papaja) yourself or send it
to me.

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-buchanan_n400s_2018" class="csl-entry">

Buchanan, E., Scofield, J., & Nunley, N. (2018). The N400’s 3 As:
Association, Automaticity, Attenuation (and Some Semantics Too).
*PsyArXiv*. <https://doi.org/10.17605/osf.io/6w2se> (R Markdown and data
files: https://osf.io/h5sd6/)

</div>

<div id="ref-chen_does_2019" class="csl-entry">

Chen, S.-C., de Koning, B., & Zwaan, R. A. (2019). Does Object Size
Matter with Regard to the Mental Simulation of Object Orientation?
*Experimental Psychology*. <https://doi.org/ggfzxw>

</div>

<div id="ref-heycke_screen_2019" class="csl-entry">

Heycke, T., & Spitzer, L. (2019). Screen Recordings as a Tool to
Document Computer Assisted Data Collection Procedures. *Psychologica
Belgica*, *59*(1), 269–280. <https://doi.org/gf5t5c>

</div>

<div id="ref-rouder_optional_2019" class="csl-entry">

Rouder, J., & Haaf, J. M. (2019). Optional Stopping and the
Interpretation of The Bayes Factor. *PsyArXiv*. <https://doi.org/gf523c>
(R Markdown and data files: https://osf.io/uv456/)

</div>

<div id="ref-stahl_evaluative_2020" class="csl-entry">

Stahl, C., & Corneille, O. (2020). Evaluative conditioning in the
Surveillance paradigm is moderated by awareness exclusion criteria.
*PsyArXiv*. <https://doi.org/10.31234/osf.io/3xsbu> (R Markdown and data
files: https://osf.io/qs35v)

</div>

<div id="ref-zhang_wandering_2020" class="csl-entry">

Zhang, H., Miller, K. F., Sun, X., & Cortina, K. S. (2020). Wandering
eyes: Eye movements during mind wandering in video lectures. *Applied
Cognitive Psychology*, acp.3632. <https://doi.org/ggjvfp>

</div>

</div>

# Other related R packages

By now, there are a couple of R packages that provide convenience
functions to facilitate the reporting of statistics in accordance with
APA guidelines.

-   [apa](https://github.com/dgromer/apa): Format output of statistical
    tests in R according to APA guidelines
-   [APAstats](https://github.com/achetverikov/APAstats): R functions
    for formatting results in APA style and other stuff
-   [apaTables](https://github.com/dstanley4/apaTables): Create American
    Psychological Association (APA) Style Tables
-   [pubprint](https://bitbucket.org/mutluyum/pubprint): This package
    takes the output of several statistical tests, collects the
    characteristic values and transforms it in a publish-friendly
    pattern
-   [schoRsch](https://cran.r-project.org/web/packages/schoRsch/index.html):
    Tools for Analyzing Factorial Experiments
-   [sigr](https://github.com/WinVector/sigr): Concise formatting of
    significances in R

Obviously, not all journals require manuscripts and articles to be
prepared according to APA guidelines. If you are looking for other
journal article templates, the following list of `rmarkdown`/`pandoc`
packages and templates may be helpful.

-   [rticles](https://github.com/rstudio/rticles): LaTeX Journal Article
    Templates for R Markdown
-   [chi-proc-rmd-template](https://github.com/ulyngs/chi-proc-rmd-template):
    ACM CHI Proceedings R Markdown Template
-   [Michael Sachs’ pandoc journal
    templates](https://github.com/sachsmc/pandoc-journal-templates):
    Pandoc templates for the major statistics and biostatistics journals

If you know of other packages and templates, drop us a note, so we can
add them here.

<!-- # Package dependencies -->
