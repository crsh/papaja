<img src='tools/images/papaja_hex.png' align='right' height='150' />papaja:
Prepare APA Journal Articles<br />with R Markdown
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN/METACRAN](https://img.shields.io/cran/v/papaja?label=CRAN&logo=r)](https://cran.r-project.org/package=papaja)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![GitHub last commit
(devel)](https://img.shields.io/github/last-commit/crsh/papaja/devel?label=Last%20commit&logo=github)
[![R-CMD-check](https://github.com/crsh/papaja/workflows/R-CMD-check/badge.svg)](https://github.com/crsh/papaja/actions)
[![codecov](https://codecov.io/gh/crsh/papaja/branch/master/graph/badge.svg)](https://codecov.io/gh/crsh/papaja)
[![GitHub bug
issues](https://img.shields.io/github/issues/crsh/papaja/bug?label=Bugs&logo=github)](https://github.com/crsh/papaja/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
[![StackOverflow
questions](https://img.shields.io/stackexchange/stackoverflow/t/papaja?label=Questions&logo=stackoverflow)](https://stackoverflow.com/questions/tagged/papaja)

**papaja** is an
[award-winning](https://improvingpsych.org/mission/awards/) R package
that facilitates creating computationally reproducible, submission-ready
manuscripts which conform to the American Psychological Association
(APA) manuscript guidelines (6th Edition). **papaja** provides

-   an [R Markdown](https://rmarkdown.rstudio.com/) template that can be
    used with (or without) [RStudio](https://www.rstudio.com/) to create
    PDF documents (using the [apa6](http://www.ctan.org/pkg/apa6) LaTeX
    class) or Word documents (using a .docx-reference file).
-   Functions to **typeset** the results from **statistical analyses**,
-   functions to create **tables**, and
-   functions to create **figures** in accordance with APA guidelines.

For a comprehensive introduction to **papaja**, see the current draft of
the [manual](http://frederikaust.com/papaja_man/). If you have a
specific question that is not answered in the manual, feel free to ask a
question on Stack Overflow [using the **papaja**
tag](https://stackoverflow.com/questions/tagged/papaja). If you believe
you have found a bug or would like to request a new feature, [open an
issue](https://github.com/crsh/papaja/issues) on Github and provide a
[minimal complete verifiable
example](https://stackoverflow.com/help/mcve).

## Example

Take a look at the [source
file](https://github.com/crsh/papaja/blob/master/vignettes/papaja.Rmd)
of the package vignette and the resulting
[PDF](https://raw.githubusercontent.com/crsh/papaja/master/vignettes/papaja.pdf).
The vignette also contains some basic instructions.

## Installation

To use **papaja** you need either a recent version of
[RStudio](https://www.rstudio.com/) or [pandoc](https://pandoc.org/). If
you want to create PDF- in addition to DOCX-documents you additionally
need a [TeX](https://en.wikipedia.org/wiki/TeX) distribution. We
recommend you use [TinyTex](https://yihui.org/tinytex/), which can be
installed from within R:

``` r
if(!requireNamespace("tinytex", quietly = TRUE)) install.packages("tinytex")

tinytex::install_tinytex()
```

You may also consider [MikTeX](http://miktex.org/) for Windows,
[MacTeX](https://tug.org/mactex/) for Mac, or [TeX
Live](http://www.tug.org/texlive/) for Linux. Please refer to the
[**papaja**
manual](http://frederikaust.com/papaja_man/introduction.html#getting-started)
for detailed installation instructions.

**papaja** is not yet available on CRAN but you can install it from this
repository:

``` r
# Install remotes package if necessary
if(!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Install the stable development version from GitHub
remotes::install_github("crsh/papaja")

# Install the latest development snapshot from GitHub
remotes::install_github("crsh/papaja@devel")
```

## Usage

Once **papaja** is installed, you can select the APA template when
creating a new R Markdown file through the RStudio menus.

![APA template selection dialog](inst/images/template_selection.png)

To add citations, specify your bibliography-file in the YAML front
matter of the document (`bibliography: my.bib`) and start citing (for
details, see pandoc manual on the [citeproc
extension](https://pandoc.org/MANUAL.html#extension-citations). You may
also be interested in [**citr**](https://github.com/crsh/citr), an R
Studio addin to swiftly insert Markdown citations and [R Studio’s visual
editor](https://rstudio.github.io/visual-markdown-editing/), which also
enables swiftly [inserting
citations](https://rstudio.github.io/visual-markdown-editing/citations.html).

### Typeset analysis results

The functions `apa_print()` and `apa_table()` facilitate reporting
results of your analyses. When you pass the an output object of a
supported class, such as an `htest`- or `lm`-object, to `apa_print()`,
it will return a list of character strings that you can use to report
the results of your analysis.

``` r
my_lm <- lm(
  Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length
  , data = iris
)
apa_lm <- apa_print(my_lm)

apa_lm$full_result$Sepal_Length
```

    ## [1] "$b = 0.61$, 95\\% CI $[0.48, 0.73]$, $t(146) = 9.77$, $p < .001$"

**papaja** currently provides methods for the following object classes:

| A-B              | D-L     | L-S               | S-Z              |
|:-----------------|:--------|:------------------|:-----------------|
| afex_aov         | default | lsmobj            | summary.aovlist  |
| anova            | emmGrid | manova            | summary.glht     |
| anova.lme        | glht    | merMod            | summary.glm      |
| Anova.mlm        | glm     | mixed             | summary.lm       |
| aov              | htest   | papaja_wsci       | summary.manova   |
| aovlist          | list    | summary_emm       | summary.ref.grid |
| BFBayesFactor    | lm      | summary.Anova.mlm |                  |
| BFBayesFactorTop | lme     | summary.aov       |                  |

### Create tables

`apa_table()` may be used to produce publication-ready tables in an R
Markdown document. For instance, you might want to report some condition
means (with standard errors).

``` r
library("dplyr")
npk |>
  group_by(N, P) |>
  summarise(mean = mean(yield), se = sd(yield) / sqrt(length(yield)), .groups = "drop") |>
  label_variables(N = "Nitrogen", P = "Phosphate", mean = "*M*", se = "*SE*") |>
  apa_table(caption = "Mean pea yield (with standard errors)")
```

Table 1. *Mean pea yield (with standard errors)*

| Nitrogen | Phosphate |  *M*  | *SE* |
|:--------:|:---------:|:-----:|:----:|
|    0     |     0     | 51.72 | 1.88 |
|    0     |     1     | 52.42 | 2.65 |
|    1     |     0     | 59.22 | 2.66 |
|    1     |     1     | 56.15 | 2.08 |

This is a fairly simple example, but `apa_table()` may be used to
generate [more complex tables](https://osf.io/s4968/).

`apa_table()`, of course, plays nicely with the output from
`apa_print()`. Thus, it is possible to conveniently report complete
regression tables, ANOVA tables, or the output from mixed-effects
models.

``` r
lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris) |>
  apa_print() |>
  apa_table(caption = "Iris regression table.")
```

Table 2. *Iris regression table.*

| Predictor    |   *b* |      95% CI      |   *t* | *df* | *p*     |
|:-------------|------:|:----------------:|------:|-----:|:--------|
| Intercept    |  1.04 |  \[0.51, 1.58\]  |  3.85 |  146 | \< .001 |
| Sepal Length |  0.61 |  \[0.48, 0.73\]  |  9.77 |  146 | \< .001 |
| Petal Width  |  0.56 |  \[0.32, 0.80\]  |  4.55 |  146 | \< .001 |
| Petal Length | -0.59 | \[-0.71, -0.46\] | -9.43 |  146 | \< .001 |

### Create figures

**papaja** further provides functions to create publication-ready plots.
For example, you can use `apa_barplot()`, `apa_lineplot()`, and
`apa_beeplot()` (or the general function `apa_factorial_plot()`) to
visualize the results of factorial study designs:

``` r
apa_beeplot(
  data = stroop_data
  , dv = "response_time"
  , id = "id"
  , factors = c("congruency", "load")
  , ylim = c(0, 800)
  , dispersion = wsci # within-subjects confidence intervals
  , conf.level = .99
  , las = 1
)
```

![Response times from a simulated Stroop experiment. Large dots
represent condition means, small dots represent individual participants’
mean response time. Error bars represent 99% within-subjects confidence
intervals.](README_files/figure-gfm/stroop-plot-1.png)

If you prefer `ggplot2`, try `theme_apa()`.

``` r
library("ggplot2")
library("ggforce")

p <- ggplot(
  stroop_data
  , aes(x = congruency, y = response_time, shape = load, fill = load)
) +
  geom_violin(alpha = 0.2, color = grey(0.6)) +
  geom_sina(color = grey(0.6)) +
  stat_summary(position = position_dodge2(0.95), fun.data = mean_cl_normal) +
  lims(y = c(0, max(stroop_data$response_time))) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_grey(start = 0.6, end = 1) +
  labs(
    x = "Congruency"
    , y = "Response time"
    , shape = "Cognitive load"
    , fill = "Cognitive load"
  )

p + theme_apa()
```

![](README_files/figure-gfm/stroop-ggplot-1.png)<!-- -->

### Usage without RStudio

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

## Getting help

[![StackOverflow
questions](https://img.shields.io/stackexchange/stackoverflow/t/papaja?label=Questions&logo=stackoverflow)](https://stackoverflow.com/questions/tagged/papaja)

For a comprehensive introduction to **papaja**, check out the current
draft of the [**papaja** manual](http://frederikaust.com/papaja_man/).
If you have a specific question that is not answered in the manual, feel
free to ask a question on Stack Overflow [using the **papaja**
tag](https://stackoverflow.com/questions/tagged/papaja). If you believe
you have found a bug or you want to request a new feature, [open an
issue](https://github.com/crsh/papaja/issues) on Github and provide a
[minimal complete verifiable
example](https://stackoverflow.com/help/mcve).

## Citation

Please cite **papaja** if you use it.

    Aust, F. & Barth, M. (2022). papaja: Prepare reproducible APA journal articles with R Markdown. R package version 0.1.0.9999. Retrieved from https://github.com/crsh/papaja

For convenience, you can [use
`cite_r()`](http://frederikaust.com/papaja_man/writing.html#citing-r-and-its-packages)
or copy the reference information returned by `citation('papaja')` to
your BibTeX file:

``` bibtex
@Manual{,
  title = {{papaja}: {Prepare} reproducible {APA} journal articles with {R Markdown}},
  author = {Frederik Aust and Marius Barth},
  year = {2022},
  note = {R package version 0.1.0.9999},
  url = {https://github.com/crsh/papaja},
}
```

## papaja in the wild

If you are interested in seeing how others are using **papaja**, you can
find a [collection of
papers](http://frederikaust.com/papaja_man/published-manuscripts.html)
and the corresponding R Markdown files in the manual.

If you have published a paper that was written with **papaja**, please
add the reference to the [public Zotero
group](https://www.zotero.org/groups/2202906/papaja) yourself or send us
to me.

## Computational reproducibility

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
**papaja** manual on [how to get started using **papaja** with Docker or
CodeOcean](http://frederikaust.com/papaja_man/tips-and-tricks.html#reproducible-software-environments)
and [our Docker workflow](https://github.com/crsh/papaja_docker)
tailored for easy use with **papaja**.

## Contribute

[![GitHub help wanted
issues](https://img.shields.io/github/issues/crsh/papaja/help%20wanted?logo=github&logoColor=%2523FFF)](https://github.com/crsh/papaja/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22)
[![GitHub documentation
issues](https://img.shields.io/github/issues/crsh/papaja/documentation?logo=github&logoColor=%2523FFF)](https://github.com/crsh/papaja/issues?q=is%3Aopen+is%3Aissue+label%3Adocumentation)

Like **papaja** and want to contribute? We highly appreciate any
contributions to the R package or its documentation. Take a look at the
[open issues](https://github.com/crsh/papaja/issues) if you need
inspiration. There are many additional analyses that we would like
`apa_print()` to support. Any new S3/S4-methods for this function are
always appreciated (e.g., `factanal`, `fa`, `lavaan`). For a primer on
adding new `apa_print()`-methods, see the getting-started-vignette:

``` r
vignette("extending_apa_print", package = "papaja")
```

Before working on a contribution, please review our brief [contributing
guidelines](https://github.com/crsh/papaja/blob/master/.github/CONTRIBUTING.md)
and [code of
conduct](https://github.com/crsh/papaja/blob/master/CODE_OF_CONDUCT.md).

## Related R packages

By now, there are a couple of R packages that provide convenience
functions to facilitate the reporting of statistics in accordance with
APA guidelines.

-   [**apa**](https://github.com/dgromer/apa): Format output of
    statistical tests in R according to APA guidelines
-   [**APAstats**](https://github.com/achetverikov/APAstats): R
    functions for formatting results in APA style and other stuff
-   [**apaTables**](https://github.com/dstanley4/apaTables): Create
    American Psychological Association (APA) Style Tables
-   [**rempsyc**](https://github.com/RemPsyc/rempsyc): Convenience
    functions for psychology
-   [**sigr**](https://github.com/WinVector/sigr): Concise formatting of
    significances in R

If you are looking for other journal article templates, you may be
interested in the [**rticles**](https://github.com/rstudio/rticles)
package.

## Package dependencies

![](README_files/figure-gfm/dep-plot-1.png)<!-- -->
