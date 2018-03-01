papaja: Prepare APA journal articles with R Markdown
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Build status](https://travis-ci.org/crsh/papaja.svg?branch=master)](https://travis-ci.org/crsh/papaja)

`papaja` is a R-package in the making including a [R Markdown](http://rmarkdown.rstudio.com/) template that can be used with (or without) [RStudio](http://www.rstudio.com/) to produce documents, which conform to the American Psychological Association (APA) manuscript guidelines (6th Edition). The package uses the LaTeX document class [apa6](http://www.ctan.org/pkg/apa6) and a .docx-reference file, so you can create PDF documents, or Word documents if you have to. Moreover, `papaja` supplies R-functions that facilitate reporting results of your analyses in accordance with APA guidelines.

**Note, at this point `papaja` is in active development and should be considered alpha. If you experience any problems, please [open an issue](https://github.com/crsh/papaja/issues) on Github.**

Examples
--------

Take a look at the [.Rmd](https://github.com/crsh/papaja/blob/master/example/example.Rmd) of the example manuscript in the folder `example` and the resulting [.pdf](https://raw.githubusercontent.com/crsh/papaja/master/example/example.pdf). The example document also contains some basic instructions.

Installation
------------

### Requirements

To use `papaja` you need to make sure the following software is installed on your computer:

-   [R](http://www.r-project.org/) (2.11.1 or later)
-   [RStudio](http://www.rstudio.com/) (0.98.932 or later) is optional; if you don't use RStudio, you need to install [pandoc](http://johnmacfarlane.net/pandoc/) using the [instructions for your operating system](https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md)
-   A [TeX](http://de.wikipedia.org/wiki/TeX) distribution (2013 or later; e.g., [MikTeX](http://miktex.org/) for Windows, [MacTeX](https://tug.org/mactex/) for Mac, obviously, or [TeX Live](http://www.tug.org/texlive/) for Linux).
    -   **Windows** users should use MikTex if possible. Currently, pandoc and the Windows version of Tex Live [don't seem to like each other](https://github.com/rstudio/rmarkdown/issues/6). Make sure you install the *complete*---not the basic---version.
    -   **Ubuntu 14.04** users need a few additional TeX packages for the document class `apa6` to work:

``` {sh}
sudo apt-get install texlive texlive-publishers texlive-fonts-extra texlive-latex-extra texlive-humanities lmodern
```

### Install papaja

Once all that is taken care of, install `papaja` from GitHub:

``` {r}
devtools::install_github("crsh/papaja")
```

How to use papaja
-----------------

Once `papaja` is installed, you can select the APA template when creating a new Markdown file through the RStudio menus.

![APA template selection](inst/images/template_selection.png)

If you want to add citations specify your BibTeX-file in the YAML front matter of the document (`bibliography: my.bib`) and you can start citing. If necessary, have a look at R Markdown's [overview of the citation syntax](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html). You may also be interested in [citr](github.com/crsh/citr), an R Studio addin to swiftly insert Markdown citations.

### Helper functions to report analyses

The functions `apa_print()` and `apa_table()` facilitate reporting results of your analyses. Take a look at the [.Rmd](https://github.com/crsh/papaja/blob/master/example/example.Rmd) of the example manuscript in the folder `example` and the resulting [.pdf](https://raw.githubusercontent.com/crsh/papaja/master/example/example.pdf).

Drop a supported analysis result, such as an `htest`- or `lm`-object, into `apa_print()` and receive a list of possible character strings that you can use to report the results of your analysis.

``` r
my_lm <- lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
apa_lm <- apa_print(my_lm)
```

One element of this list is `apa_lm$table` that, in the case of an `lm`-object, will contain a complete regression table. Pass `apa_lm$table` to `apa_table()` to turn it into a proper table in your PDF or Word document (remember to set the chunk option `results = "asis"`).

``` r
apa_table(apa_lm$table, caption = "Iris regression table.")
```

<!-- GitHub markdown doesn't support MathJax -->

------------------------------------------------------------------------

Table. *Iris regression table.*

| Predictor    |  *b*  |      95% CI      | *t(146)* |    *p*    |
|:-------------|:-----:|:----------------:|:--------:|:---------:|
| Intercept    |  1.04 |  \[0.51, 1.58\]  |   3.85   | &lt; .001 |
| Sepal Length |  0.61 |  \[0.48, 0.73\]  |   9.77   | &lt; .001 |
| Petal Width  |  0.56 |  \[0.32, 0.80\]  |   4.55   | &lt; .001 |
| Petal Length | -0.59 | \[-0.71, -0.46\] |   -9.43  | &lt; .001 |

------------------------------------------------------------------------

`papaja` currently provides methods for the following object classes:

| A-B           | B-L               | L-S               | S-Z              |
|:--------------|:------------------|:------------------|:-----------------|
| afex\_aov     | BFBayesFactorList | lm                | summary.glm      |
| anova         | BFBayesFactorTop  | lsmobj            | summary.lm       |
| Anova.mlm     | glht              | summary.Anova.mlm | summary.ref.grid |
| aov           | glm               | summary.aov       |                  |
| aovlist       | htest             | summary.aovlist   |                  |
| BFBayesFactor | list              | summary.glht      |                  |

### Plot functions

Be sure to also check out `apa_barplot()`, `apa_lineplot()`, and `apa_beeplot()` (or the general function `apa_factorial_plot()`) if you work with factorial designs:

``` r
apa_factorial_plot(
  data = npk
  , id = "block"
  , dv = "yield"
  , factors = c("N", "P", "K")
  , ylim = c(0, 80)
  , level = .34
  , las = 1
  , ylab = "Yield"
  , plot = c("swarms", "lines", "error_bars", "points")
)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

If you prefer creating your plots with `ggplot2` try `theme_apa()`.

### Using papaja without RStudio

Don't use RStudio? No problem. Use the `rmarkdown::render` function to create articles:

``` {r}
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

Contribute
----------

Like `papaja` and want to contribute? Take a look at the [open issues](https://github.com/crsh/papaja/issues) if you need inspiration. Other than that, there are many output objects from analysis methods that we would like `apa_print()` to support. Any new S3-methods for this function are always appreciated (e.g., `factanal`, `fa`, `lavaan`, `lmer`, or `glmer`).

Papers written with papaja
--------------------------

Although `papaja` is not yet on CRAN and is still undergoing a lot of changes, there are peer-reviewed publications that use it. If you have published a paper that was written with `papaja`, let me know and I will add it to this list.

### Journal publications

Stahl, C., Barth, M., & Haider, H. (2015). Distorted estimates of implicit and explicit learning in applications of the process-dissociation procedure to the SRT task. *Consciousness & Cognition*, 37, 27–43. doi: [10.1016/j.concog.2015.08.003](http://dx.doi.org/10.1016/j.concog.2015.08.003)

Aust, F., & Edwards, J. D. (2016). Incremental validity of Useful Field of View subtests for the prediction of Instrumental Activities of Daily Living. *Journal of Clinical and Experimental Neuropsychology*, 38, 497-515. doi: [10.1080/13803395.2015.1125453](http://dx.doi.org/10.1080/13803395.2015.1125453)

Stahl, C., Haaf, J., & Corneille, O. (2016). Subliminal Evaluative Conditioning? Above-Chance CS Identification May Be Necessary and Insufficient for Attitude Learning. *Journal of Experimental Psychology: General*, 145 (9), 1107-1131. doi: [10.1037/xge0000191](http://dx.doi.org/10.1037/xge0000191)

Stahl, C. & Heycke, T. (2016). Evaluative Conditioning with Simultaneous and Sequential Pairings Under Incidental and Intentional Learning Conditions. *Social Cognition*, 34, 382-412. doi: [10.1521/soco.2016.34.5.382](http://dx.doi.org/10.1521/soco.2016.34.5.382)

Papenberg, M., Willing, S. & Musch, J. (2017). Sequentially presented response options prevent the use of testwiseness cues in multiple-choice testing. *Psychological Test and Assessment Modeling*, 59, 245-266.

Heycke, T., Aust, F., & Stahl, C. (2017). Subliminal influence on preferences? A test of evaluative conditioning for brief visual conditioned stimuli using auditory unconditioned stimuli. *Royal Society Open Science*, 4, 160935. doi: [10.1098/rsos.160935](http://dx.doi.org/10.1098/rsos.160935) ([Data & R Markdown files](https://osf.io/cx5eh/))

McHugh, C., McGann, M., Igou, E. R., & Kinsella, E. L. (2017). Searching for Moral Dumbfounding: Identifying Measurable Indicators of Moral Dumbfounding. *Collabra: Psychology*, 3(1), 23. doi: [10.1525/collabra.79](http://doi.org/10.1525/collabra.79) ([Data & R Markdown files](https://osf.io/wm6vc/))

Haaf, J. M., & Rouder, J. N. (2017). Developing constraint in Bayesian mixed models. Psychological Methods, 22(4), 779-798. doi: [10.1037/met0000156](http://doi.org/10.1037/met0000156) ([R Markdown files](https://github.com/PerceptionAndCognitionLab/ctx-indiff))

Rouder, J. N., Haaf, J. M., & Aust, F. (2018). From theories to models to predictions: A Bayesian model comparison approach. *Communication Monographs*, 85(1), 41-56. doi: [10.1080/03637751.2017.1394581](https://doi.org/10.1080/03637751.2017.1394581)

Heycke, T., Gehrmann, S., Haaf, J. M., & Stahl, C. (2018). Of two minds or one? A registered replication of Rydell et al. (2006). Cognition and Emotion, 0(0), 1–20. doi: [10.1080/02699931.2018.1429389](http://doi.org/10.1080/02699931.2018.1429389) ([Data & R Markdown files](https://osf.io/c57sr/))

Sauer, S. (in press). Observation oriented modeling revised from a statistical point of view. *Behavior Research Methods*. doi: [10.3758/s13428-017-0949-8](https://doi.org/10.3758/s13428-017-0949-8) ([Data & R Markdown files](https://osf.io/6vhja/))

Aust, F., Haaf, J. M., & Stahl, C. (in press). A memory-based judgment account of expectancy-liking dissociations in evaluative conditioning. *Journal of Experimental Psychology: Learning, Memory, and Cognition*. ([Data & R Markdown files](https://osf.io/vnmby/))

### Preprints

Stahl, C., Henze, L., & Aust, F. (2016). False memory for perceptually similar but conceptually distinct line drawings. *PsyArXiv*. doi: [10.17605/OSF.IO/ZR7M8](http://dx.doi.org/10.17605/OSF.IO/ZR7M8) ([Data & R Markdown files](https://osf.io/jxm7z/))

Urry, H. L., Sifre, E., Song, J., Steinberg, H., Bornstein, M., Kim, J., … Andrews, M. (2017, March 13). Replication of Eskine, K. J., Kacinik, N. A., & Prinz, J. J. (2011) at Tufts University - Spring, 2017. Preprint retrieved from <https://osf.io/fu384/> ([Data & R Markdown files](https://osf.io/ddmkm))

Buchanan, E. M, & Scofield, J. E. (2017, August 25). Bulletproof Bias? Considering the Type of Data in Common Proportion of Variance Effect Sizes. Preprint retrieved from <https://osf.io/cs4vy/> ([Data & R Markdown files](https://osf.io/urd8q/))

Heycke, T., & Stahl, C. (2018). No Evaluative Conditioning Effects with Briefly Presented Stimuli. *PsyArXiv*. doi: [10.17605/OSF.IO/UJQ4G](http://dx.doi.org/10.17605/OSF.IO/UJQ4G) ([Data & R Markdown files](https://osf.io/3dn7e/))

Barth, M., Stahl, C., & Haider, H. (2018). Assumptions of the process-dissociation procedure are violated in sequence learning. *PsyArXiv*. doi: [10.17605/OSF.IO/P7UXN](http://dx.doi.org/10.17605/OSF.IO/P7UXN) ([Data & R Markdown files](https://github.com/methexp/pdl2))

Other related R packages
========================

By now, there are a couple of R packages that provide convenience functions to facilitate the reporting of statistics in accordance with APA guidelines.

-   [apa](https://github.com/dgromer/apa): Format output of statistical tests in R according to APA guidelines
-   [APAstats](https://github.com/achetverikov/APAstats): R functions for formatting results in APA style and other stuff
-   [apaTables](https://github.com/dstanley4/apaTables): Create American Psychological Association (APA) Style Tables
-   [pubprint](https://bitbucket.org/mutluyum/pubprint): This package takes the output of several statistical tests, collects the characteristic values and transforms it in a publish-friendly pattern
-   [schoRsch](https://cran.r-project.org/web/packages/schoRsch/index.html): Tools for Analyzing Factorial Experiments
-   [sigr](https://github.com/WinVector/sigr): Concise formatting of significances in R

Obviously, not all journals require manuscripts and articles to be prepared according to APA guidelines. If you are looking for other journal article templates, the following list of `rmarkdown`/`pandoc` packages and templates may be helpful.

-   [rticles](https://github.com/rstudio/rticles): LaTeX Journal Article Templates for R Markdown
-   [Michael Sachs' pandoc journal templates](https://github.com/sachsmc/pandoc-journal-templates): Pandoc templates for the major statistics and biostatistics journals

If you know of other packages and templates, drop us a note, so we can add them here.

Package dependencies
====================

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)
