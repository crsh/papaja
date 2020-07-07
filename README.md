papaja: Prepare APA journal articles with R Markdown
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build
status](https://travis-ci.org/crsh/papaja.svg?branch=master)](https://travis-ci.org/crsh/papaja)

`papaja` is an R package in the making including an [R
Markdown](http://rmarkdown.rstudio.com/) template that can be used with
(or without) [RStudio](http://www.rstudio.com/) to produce documents
which conform to the American Psychological Association (APA) manuscript
guidelines (6th Edition). The package uses the LaTeX document class
[apa6](http://www.ctan.org/pkg/apa6) and a .docx-reference file, so you
can create PDF documents, or Word documents if you have to. Moreover,
`papaja` supplies R-functions that facilitate reporting results of your
analyses in accordance with APA guidelines.

If you are looking for an in-depth introduction to `papaja`, check out
the current draft of the [manual](https://crsh.github.io/papaja_man/).

**`papaja` is in active development and should be considered alpha. If
you experience any problems, ask a question on Stack Overflow [using the
`papaja` tag](https://t.co/Z3auyUrbTa) or [open an
issue](https://github.com/crsh/papaja/issues) on Github.**

## Examples

Take a look at the [R
Markdown-file](https://github.com/crsh/papaja/blob/master/inst/example/example.Rmd)
of the example manuscript in the folder `example` and the resulting
[PDF](https://raw.githubusercontent.com/crsh/papaja/master/inst/example/example.pdf).
The example document also contains some basic instructions. For an
in-depth introduction to `papaja`, check out the current draft of the
[manual](https://crsh.github.io/papaja_man/).

## Installation

To use `papaja` you need either an up-to-date version of
[RStudio](http://www.rstudio.com/) or
[pandoc](http://johnmacfarlane.net/pandoc/). If you want to create PDF-
in addition to DOCX-documents you additionally need a
[TeX](http://de.wikipedia.org/wiki/TeX) distribution. If you have no use
for TeX beyond rendering R Markdown documents, I recommend you use
[TinyTex](https://yihui.name/tinytex/). TinyTex can be installed from
within R as
follows.

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
# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

# Install the stable development verions from GitHub
devtools::install_github("crsh/papaja")

# Install the latest development snapshot from GitHub
devtools::install_github("crsh/papaja@devel")
```

## How to use papaja

Once `papaja` is installed, you can select the APA template when
creating a new Markdown file through the RStudio menus.

![APA template selection](inst/images/template_selection.png)

If you want to add citations specify your BibTeX-file in the YAML front
matter of the document (`bibliography: my.bib`) and you can start
citing. If necessary, have a look at R Markdown’s [overview of the
citation
syntax](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html).
You may also be interested in [citr](https://github.com/crsh/citr), an R
Studio addin to swiftly insert Markdown citations.

### Helper functions to report analyses

The functions `apa_print()` and `apa_table()` facilitate reporting results of your analyses. Take a look at the [R Markdown-file](https://github.com/crsh/papaja/blob/master/inst/example/example.Rmd) of the example manuscript in the folder `example` and the resulting [PDF](https://raw.githubusercontent.com/crsh/papaja/master/inst/example/example.pdf).

Drop a supported analysis result, such as an `htest`- or `lm`-object,
into `apa_print()` and receive a list of possible character strings that
you can use to report the results of your
analysis.

``` r
my_lm <- lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
apa_lm <- apa_print(my_lm)
```

One element of this list is `apa_lm$table` that, in the case of an
`lm`-object, will contain a complete regression table. Pass
`apa_lm$table` to `apa_table()` to turn it into a proper table in your
PDF or Word document.

``` r
apa_table(apa_lm$table, caption = "Iris regression table.")
```

<!-- GitHub markdown doesn't support MathJax -->

-----

Table. *Iris regression table.*

| Predictor    |    *b* |      95% CI      | *t(146)* |     *p* |
| :----------- | -----: | :--------------: | -------: | ------: |
| Intercept    |   1.04 |  \[0.51, 1.58\]  |     3.85 | \< .001 |
| Sepal Length |   0.61 |  \[0.48, 0.73\]  |     9.77 | \< .001 |
| Petal Width  |   0.56 |  \[0.32, 0.80\]  |     4.55 | \< .001 |
| Petal Length | \-0.59 | \[-0.71, -0.46\] |   \-9.43 | \< .001 |

-----

`papaja` currently provides methods for the following object
classes:

| A-B                            | B-L                           | L-S                       | S-Z                           |
| :----------------------------- | :---------------------------- | :------------------------ | :---------------------------- |
| afex\_aov                      | BFBayesFactorTop<sup>\*</sup> | lm                        | summary.glht<sup>\*</sup>     |
| anova                          | default                       | lsmobj<sup>\*</sup>       | summary.glm                   |
| Anova.mlm                      | emmGrid<sup>\*</sup>          | manova                    | summary.lm                    |
| aov                            | glht<sup>\*</sup>             | summary\_emm<sup>\*</sup> | summary.ref.grid<sup>\*</sup> |
| aovlist                        | glm                           | summary.Anova.mlm         |                               |
| BFBayesFactor<sup>\*</sup>     | htest                         | summary.aov               |                               |
| BFBayesFactorList<sup>\*</sup> | list                          | summary.aovlist           |                               |

\* Not fully tested, don’t trust blindly\!

### Plot functions

Be sure to also check out `apa_barplot()`, `apa_lineplot()`, and
`apa_beeplot()` (or the general function `apa_factorial_plot()`) if you
work with factorial designs:

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

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

If you prefer creating your plots with `ggplot2` try `theme_apa()`.

### Using papaja without RStudio

Don’t use RStudio? No problem. Use the `rmarkdown::render` function to
create articles:

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

### Using papaja with CodeOcean

Seth Gree has kindly prepared a [minimal `papaja` example
capsule](https://codeocean.com/capsule/8183533/). If you want to use
`papaja` in your next CodeOcean project you can use this capsule as a
starting point.

## Getting help

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

Like `papaja` and want to contribute? Take a look at the [open
issues](https://github.com/crsh/papaja/issues) if you need inspiration.
Other than that, there are many output objects from analysis methods
that we would like `apa_print()` to support. Any new S3/S4-methods for
this function are always appreciated (e.g., `factanal`, `fa`, `lavaan`,
`lmer`, or `glmer`).

## Papers written with papaja

Please cite `papaja` if you use it (`citation('papaja')` will provide
the reference). Below are some peer-reviewed publications that used
`papaja`. If you have published a paper that was written with `papaja`,
you can add the reference to the [public Zotero
group](https://www.zotero.org/groups/2202906/papaja) yourself or send it
to me.

<div id="refs">

<div id="ref-aust_incremental_2016">

Aust, F., & Edwards, J. D. (2016). Incremental validity of Useful Field
of View subtests for the prediction of instrumental activities of daily
living. *Journal of Clinical and Experimental Neuropsychology*, *38*(5),
497–515. <https://doi.org/10.1080/13803395.2015.1125453>

</div>

<div id="ref-aust_memory-based_2018">

Aust, F., Haaf, J. M., & Stahl, C. (2018). A memory-based judgment
account of expectancy-liking dissociations in evaluative conditioning.
*Journal of Experimental Psychology: Learning, Memory, and Cognition*.
<https://doi.org/10/gdxv8n> (R Markdown and data files:
https://osf.io/vnmby/)

</div>

<div id="ref-aust_enhancing_2019">

Aust, F., & Stahl, C. (2019). The enhancing effect of caffeine on
mnemonic discrimination is at best small. *PsyArXiv*.
<https://doi.org/10/gf6jwz> (R Markdown and data files:
https://osf.io/p7f4m/)

</div>

<div id="ref-barrett_automating_2019">

Barrett, T. S., Borrie, S. A., & Yoho, S. E. (2019). Automating with
Autoscore: Introducing an R package for automating the scoring of
orthographic transcripts. *PsyArXiv*. <https://doi.org/10/gf4cqp> (R
Markdown and data files: https://osf.io/htqvr/)

</div>

<div id="ref-barth_assumptions_2018">

Barth, M., Stahl, C., & Haider, H. (2018). Assumptions of the
process-dissociation procedure are violated in implicit sequence
learning. *Journal of Experimental Psychology: Learning, Memory, and
Cognition*. <https://doi.org/10/gdxv8m> (R Markdown and data files:
https://github.com/methexp/pdl2)

</div>

<div id="ref-bartlett_no_2020">

Bartlett, J. E. (2020). No Difference in Trait-Level Attentional Bias
Between Daily and Non-Daily Smokers. *PsyArXiv*.
<https://doi.org/10/gg2c8f> (R Markdown and data files: osf.io/am9hd/)

</div>

<div id="ref-beaton_generalization_2018">

Beaton, D., Sunderland, K. M., Levine, B., Mandzia, J., Masellis, M.,
Swartz, R. H., … Strother, S. C. (2018). Generalization of the minimum
covariance determinant algorithm for categorical and mixed data types.
*bioRxiv*. <https://doi.org/10.1101/333005>

</div>

<div id="ref-bergmann_promoting_2018">

Bergmann, C., Tsuji, S., Piccinini, P. E., Lewis, M. L., Braginsky, M.,
Frank, M. C., & Cristia, A. (2018). Promoting Replicability in
Developmental Research Through Meta-analyses: Insights From Language
Acquisition Research. *Child Development*.
<https://doi.org/10.1111/cdev.13079> (R Markdown and data files:
https://osf.io/uhv3d/)

</div>

<div id="ref-bol_understanding_2018">

Bol, N., Dienlin, T., Kruikemeier, S., Sax, M., Boerman, S. C.,
Strycharz, J., … de Vreese, C. H. (2018). Understanding the Effects of
Personalization as a Privacy Calculus: Analyzing Self-Disclosure Across
Health, News, and Commerce Contexts†. *Journal of Computer-Mediated
Communication*, *23*(6), 370–388. <https://doi.org/10/gftcm6>

</div>

<div id="ref-buchanan_does_2018">

Buchanan, E., Foreman, R., Johnson, B., Pavlacic, J., Swadley, R., &
Schulenberg, S. (2018). Does the Delivery Matter? Examining
Randomization at the Item Level. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/p93df> (R Markdown and data files:
https://osf.io/gvx7s/)

</div>

<div id="ref-buchanan_perceived_2018">

Buchanan, E., Johnson, B., Miller, A., Stockburger, D., & Beauchamp, M.
(2018a). Perceived Grading and Student Evaluation of Instruction.
*PsyArXiv*. <https://doi.org/10.17605/osf.io/7x4uf> (R Markdown and data
files: https://osf.io/jdpfs/)

</div>

<div id="ref-buchanan_methods_2018">

Buchanan, E. M., & Scofield, J. E. (2018). Methods to detect low quality
data and its implication for psychological research. *Behavior Research
Methods*. <https://doi.org/10.3758/s13428-018-1035-6> (R Markdown and
data files: https://osf.io/x6t8a/)

</div>

<div id="ref-buchanan_bulletproof_2018">

Buchanan, E., & Scofield, J. (2018). Bulletproof Bias? Considering the
Type of Data in Common Proportion of Variance Effect Sizes. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/cs4vy> (R Markdown and data files:
https://osf.io/urd8q/)

</div>

<div id="ref-buchanan_n400s_2018">

Buchanan, E., Scofield, J., & Nunley, N. (2018b). The N400’s 3 As:
Association, Automaticity, Attenuation (and Some Semantics Too).
*PsyArXiv*. <https://doi.org/10.17605/osf.io/6w2se> (R Markdown and data
files: https://osf.io/h5sd6/)

</div>

<div id="ref-buchanan_extension_2018">

Buchanan, E., & Valentine, K. (2018). An Extension of the QWERTY Effect:
Not Just the Right Hand, Expertise and Typability Predict Valence
Ratings of Words. *PsyArXiv*. <https://doi.org/10.31219/osf.io/k7dx5> (R
Markdown and data files: https://osf.io/zs2qj/)

</div>

<div id="ref-buchanan_english_2018">

Buchanan, E., Valentine, K., & Maxwell, N. (2018c). English Semantic
Feature Production Norms: An Extended Database of 4,436 Concepts.
*PsyArXiv*. <https://doi.org/10.17605/osf.io/gxbf4> (R Markdown and data
files: https://osf.io/cjyzw/)

</div>

<div id="ref-buchanan_lab:_2018">

Buchanan, E., Valentine, K., & Maxwell, N. (2018d). The LAB: Linguistic
Annotated Bibliography. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/h3bwx> (R Markdown and data files:
https://osf.io/9bcws/)

</div>

<div id="ref-chen_does_2019">

Chen, S.-C., de Koning, B., & Zwaan, R. A. (2019). Does Object Size
Matter with Regard to the Mental Simulation of Object Orientation?
*Experimental Psychology*. <https://doi.org/10/ggfzxw>

</div>

<div id="ref-conigrave2020drinking">

Conigrave, J. H., Lee, K. K., Zheng, C., Wilson, S., Perry, J.,
Chikritzhs, T., … others. (2020). Drinking risk varies within and
between Australian Aboriginal and Torres Strait Islander samples: A
meta-analysis to identify sources of heterogeneity. *Addiction*.
<https://doi.org/10/ggsk3n>

</div>

<div id="ref-craddock_transcranial_2018">

Craddock, M., Klepousniotou, E., El-Deredy, W., Poliakoff, E., & Lloyd,
D. M. (2018). Transcranial alternating current stimulation at 10 Hz
modulates response bias in the Somatic Signal Detection Task. *bioRxiv*.
<https://doi.org/10.1101/330134>

</div>

<div id="ref-derringer_simple_2018">

Derringer, J. (2018). A simple correction for non-independent tests.
*PsyArXiv*. <https://doi.org/10/gdrbxc> (R Markdown and data files:
https://osf.io/re5w2/)

</div>

<div id="ref-faulkenberry_task_2018">

Faulkenberry, T. J., Cruise, A., & Shaki, S. (2018). Task instructions
modulate unit–decade binding in two-digit number representation.
*Psychological Research*. <https://doi.org/10/gdxv8k> (R Markdown and
data files: https://github.com/tomfaulkenberry/twodigittaskmanip)

</div>

<div id="ref-field_maternal_2020">

Field, A. P., Lester, K. J., Cartwright-Hatton, S., Harold, G. T., Shaw,
D. S., Natsuaki, M. N., … Leve, L. D. (2020). Maternal and paternal
influences on childhood anxiety symptoms: A genetically sensitive
comparison. *Journal of Applied Developmental Psychology*, *68*, 101123.
<https://doi.org/10/ggq38c> (R Markdown and data files:
https://osf.io/zgcg2/)

</div>

<div id="ref-flygare_adapted_2018">

Flygare, O., Andersson, E., Ringberg, H., Hellstadius, A.-C., Edbacken,
J., Enander, J., … Rück, C. (2018). Adapted cognitive behavior therapy
for obsessive compulsive disorder with co-occuring autism spectrum
disorder: A clinical effectiveness study. *PsyArXiv*.
<https://doi.org/10/gffjrb> (R Markdown and data files:
https://osf.io/gj87z/)

</div>

<div id="ref-garrison_familiarity_2020">

Garrison, H., Baudet, G., Breitfeld, E., Aberman, A., & Bergelson, E.
(2020). Familiarity plays a small role in noun comprehension at 12-18
months. *Infancy*. <https://doi.org/10/ggsnm2> (R Markdown and data
files: https://osf.io/pb2g6/)

</div>

<div id="ref-haaf_note_2018">

Haaf, J. M., Klaassen, F., & Rouder, J. (2018). A Note on Using Systems
of Orders to Capture Theoretical Constraint in Psychological Science.
*PsyArXiv*. <https://doi.org/10/gffjrf> (R Markdown and data files:
https://github.com/perceptionandcognitionlab/bf-order)

</div>

<div id="ref-haaf_and_2018">

Haaf, J. M., & Rouder, J. (2018). Some do and some don’t? Accounting for
variability of individual difference structures. *PsyArXiv*.
<https://doi.org/10.31234/osf.io/zwjtp> (R Markdown and data files:
https://github.com/perceptionandcognitionlab/ctx-mixture)

</div>

<div id="ref-haaf_developing_2017">

Haaf, J. M., & Rouder, J. N. (2017). Developing constraint in bayesian
mixed models. *Psychological Methods*, *22*(4), 779–798.
<https://doi.org/10.1037/met0000156> (R Markdown and data files:
https://github.com/perceptionandcognitionlab/ctx-indiff)

</div>

<div id="ref-hardwicke_data_2018">

Hardwicke, T. E., Mathur, M. B., MacDonald, K., Nilsonne, G., Banks, G.
C., Kidwell, M. C., … Frank, M. C. (2018). Data availability,
reusability, and analytic reproducibility: Evaluating the impact of a
mandatory open data policy at the journal *cognition*. *Royal Society
Open Science*, *5*(8), 180448. <https://doi.org/10/gdz63s> (R Markdown
and data files: https://osf.io/wn8fd/)

</div>

<div id="ref-hardwicke_mapping_2018">

Hardwicke, T., & Ioannidis. (2018). Mapping the Universe of Registered
Reports. *PsyArXiv*. <https://doi.org/10.31222/osf.io/fzpcy> (R Markdown
and data files: https://osf.io/7dpwb/)

</div>

<div id="ref-harms_making_2018">

Harms, C., & Lakens, D. (2018). Making ’Null Effects’ Informative:
Statistical Techniques and Inferential Frameworks. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/48zca> (R Markdown and data files:
https://osf.io/wptju/)

</div>

<div id="ref-heino_bayesian_2018">

Heino, M. T. J., Vuorre, M., & Hankonen, N. (2018). Bayesian evaluation
of behavior change interventions: A brief introduction and a practical
example. *Health Psychology and Behavioral Medicine*, *6*(1), 49–78.
<https://doi.org/10.1080/21642850.2018.1428102> (R Markdown and data
files: https://github.com/heinonmatti/baseline-visu)

</div>

<div id="ref-henderson_effect_2019">

Henderson, E. L., Vall’ee-Tourangeau, F., & Simons, D. J. (2019). The
Effect of Concrete Wording on Truth Judgements: A Preregistered
Replication and Extension of Hansen & Wänke (2010). *Collabra:
Psychology*, *5*. <https://doi.org/10/gf9h3x>

</div>

<div id="ref-heycke_contingency_2018">

Heycke, T. (2018, July). *Contingency Awareness in Evaluative
Conditioning: Investigations Using Subliminal Stimulus Presentations*
(text.thesis.doctoral). Universität zu Köln. Retrieved from
<http://www.uni-koeln.de/>

</div>

<div id="ref-heycke_subliminal_2017">

Heycke, T., Aust, F., & Stahl, C. (2017). Subliminal influence on
preferences? A test of evaluative conditioning for brief visual
conditioned stimuli using auditory unconditioned stimuli. *Royal Society
Open Science*, *4*(9), 160935. <https://doi.org/10.1098/rsos.160935>

</div>

<div id="ref-heycke_two_2018">

Heycke, T., Gehrmann, S., Haaf, J. M., & Stahl, C. (2018). Of two minds
or one? A registered replication of Rydell et al. (2006). *Cognition and
Emotion*, *32*(8), 1708–1727.
<https://doi.org/10.1080/02699931.2018.1429389>

</div>

<div id="ref-heycke_screen_2019">

Heycke, T., & Spitzer, L. (2019). Screen Recordings as a Tool to
Document Computer Assisted Data Collection Procedures. *Psychologica
Belgica*, *59*(1), 269–280. <https://doi.org/10/gf5t5c>

</div>

<div id="ref-heycke_no_2018">

Heycke, T., & Stahl, C. (2018). No evaluative conditioning effects with
briefly presented stimuli. *Psychological Research*.
<https://doi.org/10.1007/s00426-018-1109-1>

</div>

<div id="ref-heyman_can_2018">

Heyman, T., & Heyman, G. (2018). Can prediction-based distributional
semantic models predict typicality? *PsyArXiv*.
<https://doi.org/10.17605/osf.io/59xtd> (R Markdown and data files:
https://osf.io/nkfjy/)

</div>

<div id="ref-jordan_focus_2018">

Jordan, K., Buchanan, E., & Padfield, W. (2018). Focus on the Target:
The Role of Attentional Focus in Decisions about War. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/9fgu8> (R Markdown and data files:
https://osf.io/r8qp2/)

</div>

<div id="ref-kothe_retention_2019">

Kothe, E. J., & Ling, M. (2019). Retention of participants recruited to
a one-year longitudinal study via Prolific. *PsyArXiv*.
<https://doi.org/10.31234/osf.io/5yv2u> (R Markdown and data files:
https://osf.io/yjstk/)

</div>

<div id="ref-lakens_equivalence_2018">

Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence Testing
for Psychological Research: A Tutorial. *Advances in Methods and
Practices in Psychological Science*, *1*(2), 259–269.
<https://doi.org/10/gdj7s9> (R Markdown and data files:
https://osf.io/qamc6/)

</div>

<div id="ref-lewis_quantitative_2017">

Lewis, M., Braginsky, M., Tsuji, S., Bergmann, C., Piccinini, P. E.,
Cristia, A., & Frank, M. C. (2017). A Quantitative Synthesis of Early
Language Acquisition Using Meta-Analysis. *PsyArXiv*.
<https://doi.org/10.31234/osf.io/htsjm>

</div>

<div id="ref-maxwell_investigating_2018">

Maxwell, N., & Buchanan, E. (2018a). Investigating the Interaction
between Associative, Semantic, and Thematic Database Norms for Memory
Judgments and Retrieval. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/fcesn> (R Markdown and data files:
https://osf.io/y8h7v/)

</div>

<div id="ref-maxwell_modeling_2018">

Maxwell, N., & Buchanan, E. (2018b). Modeling Memory: Exploring the
Relationship Between Word Overlap and Single Word Norms when Predicting
Relatedness Judgments and Retrieval. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/qekad> (R Markdown and data files:
https://osf.io/j7qtc/)

</div>

<div id="ref-mchugh_searching_2017">

McHugh, C., McGann, M., Igou, E. R., & Kinsella, E. L. (2017). Searching
for Moral Dumbfounding: Identifying Measurable Indicators of Moral
Dumbfounding. *Collabra: Psychology*, *3*(1).
<https://doi.org/10.1525/collabra.79> (R Markdown and data files:
https://osf.io/wm6vc/)

</div>

<div id="ref-mchugh_reasons_2020">

McHugh, C., McGann, M., Igou, E. R., & Kinsella, E. L. (2020). Reasons
or rationalizations: The role of principles in the moral dumbfounding
paradigm. *Journal of Behavioral Decision Making*, bdm.2167.
<https://doi.org/10/ggf94x>

</div>

<div id="ref-moors_unconscious_2019">

Moors, P., & Hesselmann, G. (2019). Unconscious arithmetic: Assessing
the robustness of the results reported by Karpinski, Briggs, and Yale
(2018). *Consciousness and Cognition*, *68*, 97–106.
<https://doi.org/10/gftmrj>

</div>

<div id="ref-morin-lessard_selective_2019">

Morin-Lessard, E., Poulin-Dubois, D., Segalowitz, N., & Byers-Heinlein,
K. (2019). Selective attention to the mouth of talking faces in
monolinguals and bilinguals aged 5 months to 5 years. *PsyArXiv*.
<https://doi.org/10.31234/osf.io/5pkne> (R Markdown and data files:
https://osf.io/ikvyr/)

</div>

<div id="ref-nalborczyk_introduction_2019">

Nalborczyk, L., Batailler, C., Lœvenbruck, H., Vilain, A., & Bürkner,
P.-C. (2019). An Introduction to Bayesian Multilevel Models Using brms:
A Case Study of Gender Effects on Vowel Variability in Standard
Indonesian. *Journal of Speech, Language, and Hearing Research*,
*62*(5), 1225–1242. <https://doi.org/10.1044/2018_JSLHR-S-18-0006> (R
Markdown and data files: https://osf.io/dpzcb/)

</div>

<div id="ref-navarro_if_2020">

Navarro, D. (2020). If mathematical psychology did not exist we might
need to invent it: A comment on theory building in psychology.
*PsyArXiv*. <https://doi.org/10.31234/osf.io/ygbjp>

</div>

<div id="ref-papenberg_sequentially_2017">

Papenberg, M., Willing, S., & Musch, J. (2017). Sequentially presented
response options prevent the use of testwiseness cues in multiple-choice
testing. *Psychological Test and Assessment Modeling*, *59*(2), 245–266.
Retrieved from
<http://www.psychologie-aktuell.com/fileadmin/download/ptam/2-2017_20170627/06_Papenberg_.pdf>

</div>

<div id="ref-pavlacic_meta-analysis_2018">

Pavlacic, J., Buchanan, E., Maxwell, N., Hopke, T., & Schulenberg, S.
(2018). A Meta-Analysis of Expressive Writing on Positive Psychology
Variables and Traumatic Stress. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/u98cw> (R Markdown and data files:
https://osf.io/4mjqt/)

</div>

<div id="ref-peterka-bonetta_relationship_2019">

Peterka-Bonetta, J., Sindermann, C., Sha, P., Zhou, M., & Montag, C.
(2019). The relationship between Internet Use Disorder, depression and
burnout among Chinese and German college students. *Addictive
Behaviors*, *89*, 188–199. <https://doi.org/10/gd4rcw>

</div>

<div id="ref-pollet_how_2018">

Pollet, T. V., & Saxton, T. (2018). How diverse are the samples used in
the journals “Evolution & Human Behavior” and “Evolutionary Psychology”?
*PsyArXiv*. <https://doi.org/10.17605/osf.io/7h24p>

</div>

<div id="ref-robison_pupillometry_2018">

Robison, M. K., & Unsworth, N. (2018). Pupillometry tracks fluctuations
in working memory performance. *PsyArXiv*. <https://doi.org/10/gdz63r>
(R Markdown and data files: osf.io/vuw9h/)

</div>

<div id="ref-rouder_psychometrics_2018">

Rouder, J., & Haaf, J. M. (2018). A Psychometrics of Individual
Differences in Experimental Tasks. *PsyArXiv*.
<https://doi.org/10/gfdbw2> (R Markdown and data files:
https://github.com/perceptionandcognitionlab/ctx-reliability)

</div>

<div id="ref-rouder_optional_2019">

Rouder, J., & Haaf, J. M. (2019). Optional Stopping and the
Interpretation of The Bayes Factor. *PsyArXiv*.
<https://doi.org/10.31234/osf.io/m6dhw> (R Markdown and data files:
https://osf.io/uv456/)

</div>

<div id="ref-rouder_minimizing_2018">

Rouder, J., Haaf, J. M., & Snyder, H. K. (2018a). Minimizing Mistakes In
Psychological Science. *PsyArXiv*. <https://doi.org/10/gfdb27> (R
Markdown and data files:
https://github.com/perceptionandcognitionlab/lab-transparent)

</div>

<div id="ref-rouder_beyond_2017">

Rouder, J., Haaf, J. M., Stober, C., & Hilgard, J. (2017). Beyond
Overall Effects: A Bayesian Approach to Finding Constraints Across A
Collection Of Studies In Meta-Analysis. *PsyArXiv*.
<https://doi.org/10/gffjrd> (R Markdown and data files:
https://github.com/perceptionandcognitionlab/meta-planned)

</div>

<div id="ref-rouder_theories_2018">

Rouder, J. N., Haaf, J. M., & Aust, F. (2018b). From theories to models
to predictions: A Bayesian model comparison approach. *Communication
Monographs*, *85*(1), 41–56.
<https://doi.org/10.1080/03637751.2017.1394581>

</div>

<div id="ref-samaey_individual_2020">

Samaey, C., Wagemans, J., & Moors, P. (2020). Individual differences in
processing orientation and proximity as emergent features. *Vision
Research*, *169*, 12–24. <https://doi.org/10/ggnc6w> (R Markdown and
data files: https://osf.io/vgxja/)

</div>

<div id="ref-sauer_observation_2017">

Sauer, S. (2017). Observation oriented modeling revised from a
statistical point of view. *Behavior Research Methods*.
<https://doi.org/10.3758/s13428-017-0949-8> (R Markdown and data files:
https://osf.io/6vhja/)

</div>

<div id="ref-stahl_distorted_2015">

Stahl, C., Barth, M., & Haider, H. (2015). Distorted estimates of
implicit and explicit learning in applications of the
process-dissociation procedure to the SRT task. *Consciousness and
Cognition*, *37*, 27–43. <https://doi.org/10.1016/j.concog.2015.08.003>

</div>

<div id="ref-stahl_evaluative_2020">

Stahl, C., & Corneille, O. (2020). Evaluative conditioning in the
Surveillance paradigm is moderated by awareness exclusion criteria.
*PsyArXiv*. <https://doi.org/10.31234/osf.io/3xsbu> (R Markdown and data
files: https://osf.io/qs35v)

</div>

<div id="ref-stahl_subliminal_2016">

Stahl, C., Haaf, J., & Corneille, O. (2016a). Subliminal Evaluative
Conditioning? Above-Chance CS Identification May Be Necessary and
Insufficient for Attitude Learning. *Journal of Experimental Psychology:
General*, *145*, 1107–1131. <https://doi.org/10.1037/xge0000191>

</div>

<div id="ref-stahl_false_2016">

Stahl, C., Henze, L., & Aust, F. (2016b). False memory for perceptually
similar but conceptually distinct line drawings. *PsyArXiv*.
<https://doi.org/10.17605/osf.io/zr7m8> (R Markdown and data files:
https://osf.io/jxm7z/)

</div>

<div id="ref-stahl_evaluative_2016">

Stahl, C., & Heycke, T. (2016). Evaluative Conditioning with
Simultaneous and Sequential Pairings Under Incidental and Intentional
Learning Conditions. *Social Cognition*, *34*(5), 382–412.
<https://doi.org/10.1521/soco.2016.34.5.382>

</div>

<div id="ref-stevens_predicting_2018">

Stevens, J. R., & Soh, L.-K. (2018). Predicting similarity judgments in
intertemporal choice with machine learning. *Psychonomic Bulletin &
Review*, *25*(2), 627–635. <https://doi.org/10/gdfghk>

</div>

<div id="ref-urry_effect_2018">

Urry, H. L., Sifre, E., Song, J., Steinberg, H., Bornstein, M., Kim, J.,
… Andrews, M. (2018). Effect of Disgust on Judgments of Moral Wrongness:
A Replication of Eskine, Kacinik, and Prinz (2011). *At Tufts University
- Spring, 2017*. Retrieved from <https://osf.io/fu384/> (R Markdown and
data files: https://osf.io/ddmkm)

</div>

<div id="ref-valentine_beyond_2018">

Valentine, K., Buchanan, E., Scofield, J., & Beauchamp, M. (2018).
Beyond p-values: Utilizing Multiple Estimates to Evaluate Evidence.
*PsyArXiv*. <https://doi.org/10.17605/osf.io/9hp7y> (R Markdown and data
files: https://osf.io/u9hf4/)

</div>

<div id="ref-vuorre_curating_2018">

Vuorre, M., & Curley, J. P. (2018). Curating Research Assets: A Tutorial
on the Git Version Control System. *PsyArXiv*.
<https://doi.org/10.31234/osf.io/6tzh8> (R Markdown and data files:
https://github.com/mvuorre/reproguide-curate)

</div>

<div id="ref-xu_challenges_2019">

Xu, R., DeShon, R. P., & Dishop, C. R. (2019). Challenges and
Opportunities in the Estimation of Dynamic Models. *Organizational
Research Methods*, 109442811984263. <https://doi.org/10/gf3vbj>

</div>

<div id="ref-zhang_wandering_2020">

Zhang, H., Miller, K. F., Sun, X., & Cortina, K. S. (2020). Wandering
eyes: Eye movements during mind wandering in video lectures. *Applied
Cognitive Psychology*, acp.3632. <https://doi.org/10/ggjvfp>

</div>

<div id="ref-zhang_missing_2019">

Zhang, H., Qu, C., Miller, K. F., & Cortina, K. S. (2019). Missing the
joke: Reduced rereading of garden-path jokes during mind-wandering.
*Journal of Experimental Psychology: Learning, Memory, and Cognition*.
<https://doi.org/10/gf68nd>

</div>

<div id="ref-zhang_all-atom_2019">

Zhang, T., Hu, G., Yang, Y., Wang, J., & Zhou, Y. (2019). All-Atom
Knowledge-Based Potential for RNA Structure Discrimination Based on the
Distance-Scaled Finite Ideal-Gas Reference State. *Journal of
Computational Biology*. <https://doi.org/10/ggcp6w>

</div>

</div>

# Other related R packages

By now, there are a couple of R packages that provide convenience
functions to facilitate the reporting of statistics in accordance with
APA guidelines.

  - [apa](https://github.com/dgromer/apa): Format output of statistical
    tests in R according to APA guidelines
  - [APAstats](https://github.com/achetverikov/APAstats): R functions
    for formatting results in APA style and other stuff
  - [apaTables](https://github.com/dstanley4/apaTables): Create American
    Psychological Association (APA) Style Tables
  - [pubprint](https://bitbucket.org/mutluyum/pubprint): This package
    takes the output of several statistical tests, collects the
    characteristic values and transforms it in a publish-friendly
    pattern
  - [schoRsch](https://cran.r-project.org/web/packages/schoRsch/index.html):
    Tools for Analyzing Factorial Experiments
  - [sigr](https://github.com/WinVector/sigr): Concise formatting of
    significances in R

Obviously, not all journals require manuscripts and articles to be
prepared according to APA guidelines. If you are looking for other
journal article templates, the following list of `rmarkdown`/`pandoc`
packages and templates may be helpful.

  - [rticles](https://github.com/rstudio/rticles): LaTeX Journal Article
    Templates for R
    Markdown
  - [chi-proc-rmd-template](https://github.com/ulyngs/chi-proc-rmd-template):
    ACM CHI Proceedings R Markdown Template
  - [Michael Sachs’ pandoc journal
    templates](https://github.com/sachsmc/pandoc-journal-templates):
    Pandoc templates for the major statistics and biostatistics journals

If you know of other packages and templates, drop us a note, so we can
add them here.

<!-- # Package dependencies -->
