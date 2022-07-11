# papaja 0.1.1

- Maintenance update: Adjuted unit tests to avoid removal from CRAN

# papaja 0.1.0

Initial CRAN release

- Some improvements to documentation, i.e. documentation of return values, removed examples for internal functions (as per CRAN request), use `\donttest{}` instead of `\dontrun{}`.
- `add_equals()` is now an exported function.

# papaja 0.1.0.9999

### Template

- Fixed bugs that occurred when using unicode characters in an appendix file (reported by @mschrein, #446 and @kalenkovich, #353).
- Level 4 and 5 headings are now displayed correctly (reported by @jamesrrae, #292).
- E-mail addresses are no longer required metadata (requested by @sbw78, #437).
- Special characters are now displayed correctly in appendices (reported by @mschrein, #446)
- Theorems are now numbered correctly (reported by @shirdekel, #447).
- The Lua-filter that parses the document metadata has been improved to be more robust (see #225).
- Markdown formatting now works for `shorttitle` and `leftheader`.
- It is now possible to specify bibliography, appendix, and CSL files (reported by @shirdekel, #482).
- Appendices can and should now be specified using the standard **bookdown** syntax (see #496 by @pablobernabeu and #484 by @shirdekel)
- Abstract is no longer indented (reported by @AdrienWitt, #510)


### Existing functions

- `apa_print()`
    - Output now always contains a `$table` element (as suggested in #324 by @Kalif-Vaughn)
    - Column names of `$table` element are now standardized following the [**broom** glossary](https://www.tidymodels.org/learn/develop/broom/#glossary)) (e.g., `term`, `estimate`, `conf.int`, `statistic`, `df`, `df.residual`, `p.value`).
        - To maintain backwards compatibility, it is possible to access columns with aliases (for instance, it is possible to write `output$table$F` and obtain `output$table$statistic`, see #364).
        - Table columns are labelled (e.g., `"\\hat{\\eta}^2_G"`, `"$F$", "$p$`, ...) for creating tables via `apa_table()`.
    - The arguments `ci`, `conf_level`, and `args_confint` used for some methods are now depreceated and has been replaced by `conf.int`, which is now used consistently across methods.
    - Returned tables now consistently have a column for degrees of freedom if applicable. If the degrees of freedom are constant, the column can be removed and degrees of freedom added to the variable label of the test statistic column using the new function `transmute_df_into_label()`.
    - `apa_print.aov()` and friends
        -  Added a new argument `estimate` to ANOVA-methods, which accepts a character string (`"ges"`, `"pes"`, and `"es"`; primarily for backward compatibility), a data frame (with columns `estimate`, `conf.low`, and `conf.high`), or a function to determine which measure of effect size is to be reported. If a function is supplied, it should take the ANOVA object as input and return a data frame with columns `estimate`, `conf.low`, and `conf.high`. Currently, output from the [**effectsize**](https://CRAN.r-project.org/package=effectsize) package is supported. By default, if **effectsize** is installed, generalized eta-squared (point estimate and confidence interval) will be calculated. Otherwise, only the point estimate will be calculated as before.
        -  It is now ensured for all cases that names of reporting strings and ordering of terms in table is consistent (reported by @pthomasi)
        - Argument `es` is now deprecated, but because it is a partial match with `estimate`, old code using `es = "ges"`, `es = "pes"` or `es = "es"` will still work and yield the same results.
    - `apa_print.list()`: Added progress bars for bootstrapped model comparisons.
    - `apa_print.emmGrid()` and friends
        - Extended support for additional link functions and output, e.g. (`emmeans::joint_tests()`, #200, PR #476)
        - Adjusted *p* values, confidence intervals, and families of tests are now marked as such (see `?apa_print.emmGrid`; #200, PR #476)
        - Fixed wrong estimate labels for interaction contrasts (reported by @shirdekel, #456).
    - `apa_print.BFBayesFactor()` and friends were previously experimental and have now been implemented properly. In the process the arguments `scientific`, `min` and `max`, `auto_invert`, `ratio_subscript`, `escape`, `evidential_boost`, and `hdi` have been deprecated and the `apa_print.BFBayesFactorList()` has been removed for now. The new methods support all **BayesFactor** analyses and interval null hypotheses. They also grant users more flexibility to specify what information to report.
    - `apa_print.lm()`: The argument `observed_predictors` is now deprecated and has been replaced by `observed` to improve consistency of argument names across methods.
- `apa_num()` and `apa_p()` are now the preferred name for the function previously called `printnum()` and `printp()`.
- `variable_label()`, `variable_labels()`, and  `label_variables()` now reside in CRAN package [**tinylabels**](https://CRAN.r-project.org/package=tinylabels), which is automatically loaded when **papaja** is loaded.
- `cite_r()` now properly escapes semicolons (reported by @marklhc, #442)

### New functions

- `apa_print.lme()`, `apa_print.merMod()`, `apa_print.mixed()`
- `glue_apa_results()` and `add_glue_to_apa_results()`, see the new vignette on extending `apa_print()` (`vignette("extending_apa_print", package = "papaja")`)
- `apa_table.apa_results()`
- `apa_interval()`, `apa_confint()`, and `apa_hdint()`
- `apa_df()`
- `transmute_df_into_label()` and `df_into_label()`
### Misc

- papaja now depends on
    - **tinylabels**
    - **rmdfiltr** (>= 0.1.3; see #427 by @jrennstich)

# papaja 0.1.0.9997

### Template

- New author metadata field `role` automatically adds contributor ship roles (e.g. [CRedit](https://credit.niso.org/)) to the author note. #375
- `apa6_pdf()`
    - Now relies on a Lua-filter to process metadata, which should resolve many encoding related problems (e.g., reported by @arcaldwell49, @dcbrh, and @DominiqueMakowski among others; #350, #357, #360)
    - Numbering of title is now disabled when `numbersections: true`
- `apa6_docx()`
    - Minor improvements to typesetting (e.g., capitalize running head)
    - `docx_fixes.lua` now tolerates missing figure captions (reported by @jvcasillas, #365)
- `revision_letter_pdf()`
    - Defaults to APA6 citation style (also see #369 by @jvcasillas)
    - Adds word count filter

### Existing functions

- `apa_print()`
    - Fixed method dispatch that sometimes failed (#361, #250)
- `apa_print.list()`
    - Each column of a model comparison table used to have names, these have now been
      removed to enable unit testing across different R releases and improve consistency
      across output objects from `apa_print()`.
- `printnum.integer()`
    - Now always returns a `character`
- `printp()`
    - Fixed examples in documentation
- `variable_labels.data.frame()`
    - When trying to assign labels to columns that do not exist, as of now the function
      not only throws an error, but also returns the names of the missing columns.
- `wsci()`, `within_subjects_conf_int()`
    - Experimental: Returned object now has class `c("papaja_wsci", "data.frame")` and returns an
      attribute `means` for post processing (see new function `summary.papaja_wsci`).

### New functions

- `apa_print.manova()`
    - Experimental: Added an `apa_print` method for MANOVA models from the `stats` package.
- `summary.papaja_wsci()`
    - Experimental: Returns means and confidence limits calculated from the output
      of `wsci()` (suggested by @jstevens5, #362)
- `apa_print.papaja_wsci()`
- `quote_from_tex()`
- `remove_comments()`


### Misc

- **papaja** now depends on
    - `R` >= 3.6
    - `pandoc` >= 2.0
    - **rmdfiltr** (>= 0.1.2) explicitly
- New `label_quotes.lua` filter to insert excerpts in revision letter; also see `quote_from_tex()`

# papaja 0.1.0.9942

### Template

- Lua filters for `pandoc` 2.0 or later
    - New ampersand filter that's faster and robust to encoding issues (see #225 and #262)
    - Word count filter is automatically applied (see R Markdown tab, #63)
- LaTeX
    - Fixed bug that printed `NA` at the top of the appendix in `doc` mode (reported by @paul-grassl, #295)
    - Fixed bug that caused problems with special characters in appendices (reported by @AlexChristensen, #283)
    - In `jou` mode, tables now span both columns unless requested otherwise (reported by @saladspices, #52, also see #298 by @jstbcs)
    - Level 4 and 5 headings are now properly runin (reported by @jamesrrae, #292)
    - Abstract heading is no longer numbered, when setting `number_sections: true` (reported by @tcgriffith, #332)
    - Fixed LaTeX warning `Command \@parboxrestore  has changed.` (fix suggested by @CrumpLab, #336)
- Word
    - Overall improved template styles to reduce the need for manual fixes ()

### Existing functions

- `apa_table()`
    - Returns an object of class `knit_asis`; it's no longer necessary to set the chunk option `results = "asis"` for tables to be rendered (#280)
    - Column headings are repeated when long tables span multiple pages (suggested by @mischavk, #289)
    - Fixed bug that cause issues with the rendering of tables in Word documents (reported by @mdanka, #301)
    - In PDF files `&` in cells are automatically escaped
- `apa_print()`
    - Output is now of class `apa_results`
- `apa_print-htest()`
    - An estimate is now reported for 2x2 contingency tables
- `printnum.integer()`
    - Now prints a `zero_string` instead of `""` when `x` is `0` (suggested by @e-schott, #311)
    - Improved input validation fixes error when `NA`is passed (reported by @JanaJarecki, #317)
- `printnum.numeric()`
    - New `add_equals` argument (suggested by @mvuorre, #313)

### New functions

- `label_variable()`/`label_variables()`: Pipable methods to set variable labels
- `simple_codebook()`: Creates a simple code book from variable labels
- `apa_print.manova()`/`apa_print.summary.manova()`: Support for standard manova output


# papaja 0.1.0.9837

### Template

- LaTeX template
    - Complete rewrite of template as adaptation of `rmarkdown::pdf_document()` template to enable all options available in the standard template (e.g., A4 paper as requested by @wuschLOR, #193; #106)
    - Adds new `appendix` fields in YAML front matter (#168)
    - Enables markdown formatting in `abstract`, `author_note`, and `note` fields (#232)
    - Renames `lineno` fields in YAML front matter to `linenumbers`
    - Fixes bug that caused `citation_package` specification to be ignored (reported by @doomlab, #188)
    - Fixes encoding bug in `endfloat` package (reported by @TobiasKoeln, #133)
    - Adds `longtable` as `endfloat` environment (reported by @tormodb, #194)
    - Fixes option clash for `geometry` package (reported by @PenelopeLQ, #218)
- DOCX reference file
    - New style for tables
    - Applies non-bold style to abstract and references title
- Skeleton
    - Adds RNG seed for caching to first chunk
    - Includes Nature abstract template

### Existing functions

- `printnum.numeric()`
    - New option `use_math` to control whether to insert `$` into the output so that `Inf` or scientific notation is rendered correctly. Fixes bug that caused an error when printing confidence intervals for one-sided t-tests (#192)
- `apa_print.htest()`
    - Corrected incorrect sign in calculation of mean differences for t-tests (reported by @jstbcs)
- `apa_table()`
    - Is now an S3 generic function
    - New method to merge multiple tables in a list using table spanners (#62)
    - Adds new `font_size` option, which extends `small` (#223)
    - Fixed bug that caused digits to be applied by row rather than by column (reported by Matti Heino, #173)
    - `escape = TRUE` is now a visible (in documentation) default (reported by @baguadoramirez, #180)
    - Fixes error due to partial matching of `format` argument (reported by @mischavk, #223)
- `apa_factorial_plot()`
    - Fixed bug that caused `jit` parameter to be ignored (#177)
    - Adds new option `use` to improve and explicate handling of NAs (#190)
- `render_appendix()`
    - Preparations for `pandoc` 2.0 support (reported by @dhagmann, #204)
    - Reenables placement options for figures and tables in appendices (reported by @TobiasKoeln, #172)
    - Enabled rendering appendices in subdirectories (reported by @jvcasillas, #168)
    - Adds support for multiple bibliography files (reported by @jvcasillas, #168)
    - No longer fails when bibliography file specified in the YAML front matter is not found (#189)
    - Enforces UTF-8 encoding
- `apa6_docx()`/`apa6_word()`
    - Accepts custom DOCX reference file (suggested by [Flo](https://stackoverflow.com/questions/49326234/format-text-for-word-output-while-using-rmd-template))
- `apa6_pdf()`
    - Utilizes `raw_attributes` extension in `pandoc` > 2.0 (reported by @mischavk, #227)
- `cite_r()`
    - Omits version numbers if package is not installed
- `wsci()`
    - Prevents incorrect calculation of confidence intervals in the presence of implicitly or explicitly missing data (#191)

### New functions

- `apa6_docx()`

Experimental:

- `apa_print.emmGrid()`
- `apa_print.summary_emm()`
- `revision_letter_pdf()`


### Misc

- Improved support for labelled variables in `printnum()`-methods
- Renamed `labelled` class to `papaja_labelled` to avoid problems due to conflicting implementations of `labelled` in the **haven** and **Hmisc** packages (see tidyverse/haven#329; first reported by @andreifoldes, #199)
- Author affiliations can now also be provided without `id`
- New experimental revision letter template
- Fixes various encoding problems on Windows (reported by @SongchaoChen, #139, @peter1328, #159, and @mdingemanse, #209)
- Utilizes native R filter to fix in-text citations in `pandoc` > 2.0 (#186)



# papaja 0.1.0.9709

### Template

- Reference section is now placed at the end explicitly and allows users to add contents after the reference section. This enables proper appendices in DOCX format.
- Fixed bug that resulted in unwanted "x"s on first page of Word documents (reported by @sjystu, #174)
- Adds proper pagebreaks before abstract and first page of the introduction in Word documents
- Fixes bug in the inclusion of appendices that lead to LaTeX errors when tables were included (reported by @ericpgreen, #179)

### Existing functions

- Switched implementation of `variable_labels()` and related functions from S4 to S3 class. The transition was necessary because the S4 class was incompatible with at least one primitive function (#176). This change may unfortunately break backward compatibility with the previous release.
- `apa_table()`
    - Now supports for column spanners that span only one column
- `render_appendix()`
    - Now supports for the application of `pandoc-citeproc`

### New functions

- `printnum.integer()` (suggested by @LorcanKenny, #178)

### Misc

- **papaja** now depends on **bookdown** 0.6 to resolve compatibility issues with `pandoc` 2.0 (reported by @ericpgreen, #170)



# papaja 0.1.0.9655

### Template
- Improved generation of the author note (#126)
- Author note skeleton text clarified to stress the need for indentation (reported and fixed by @alexholcombe, #132)
- Name of author does not include an ampersand in the correspondence line of the author note (reported by @jacob-long, #110)
- Author note is now omitted if empty in Word
- Formats throw error if name, postal address or e-mail address of corresponding author is missing.
- New rendering option `mask` removes author name, affiliations, and author note from the title page (suggested by @mcbeem, #114)
- New rendering option `replace_ampersands` controls whether `ampersand_filter()` is applied during rendering (suggested by Julia Haaf, Jeff Rouder, and Anne Scheel)
- Rendering option `lang` removed because it is not compatible with current `pandoc` versions (reimplementation needed)

### Existing functions
- `apa_barplot()`
    - Now prints `ylab` on every plot as do `apa_lineplot()` and `apa_beeplot()` (#124)
- `theme_apa()`
    - Added support for right y-axis.
- `render_appendix()`
    - The function now removes the placement options of figures and tables in appendix (reported by @TobiasKoeln, #113)
- `inline_hook()`
    - Added support for `difftime` objects
- `r_refs()`
    - Now enforces UTF-8 encoding (reported by @JimGrange, #123)
- `apa_print.anova()` and related
    - Now correctly italicize MSE
    - Corrected bug in the calculation of eta^2 and denote as estimate using `^`.
- `within_subjects_conf_int()`
    - Calculates confidence intervals in presence of `NA`s (reported by @TobiasKoeln, #137)
- `apa_print()`
    - Returned tables are of class `apa_results_table`, column names are easy to address, and each column is labelled for pretty printing.
- `apa_table()`
    - Takes into account additional options passed to `format.args` parameter (reported by @lnalborczyk, #131)
- `apa_print.htest()`
    - Supports `oneway.test()` objects.
- `apa6_word()`
    - Enforces UTF-8 encoding while augmenting the YAML front matter (reported by @lindsayhinzman, #156)

### New functions

- `apa_factorial_plot()`
- `apa_factorial_plot.afex()`
- `apa_print.glm()`
- `apa_print.summary.glm()`
- `print.apa_results_table()`
- `variable_label()`

Experimental:

- `apa_print.BFBayesFactor()`
- `apa_print.BFBayesFactorTop()`
- `apa_print.BFBayesFactorList()`



# papaja 0.1.0.9492

### Template
- The ampersand filter is applied with `--vanilla` to avoid problems with customized `.Rprofile`s (reported by @awellis, #96)
- Fixed bug in `ampersand_filter()` regex, which caused some citations to be missed depending on preceding citations (reported by @awellis, #96, and @m-Py, #100)
- On windows the Batch file to apply the ampersand filter is now created at the current location and deleted on exit to avoid permission problems (reported by @stahl-c)
- Changed default graphics devices to `pdf` and `png` to improve compatibility and rendering speed; `postscript` and `tiff` are no longer used be default
- Author note is omitted if no corresponding author is defined and no author note text is provided (reported by @jacob-long, #108; fixed by @jacob-long, #109)

### Existing functions
- `apa_print.aov()`
    - Fixed bug that caused incorrect calculation of eta squared
    - MSE is now typeset in regular font rather than italic as per APA guidelines
- `apa_table()`
    - Added proper support for automated bookdown cross-referencing of tables: Chunk label is now automatically added as `\label{}` tag.
    - Now supports `na_string` and `digits` options
- `theme_apa()`
    - Now uses `ggplot::margin()` to set margins because `ggplot::unit()` is now deprecated.
    - Changed base size default from 14 to 12 and adjusted some margin and spacing defaults
- `r_refs()`
    - Fixed bug that caused some existing references to be added to the bib-file (#102)
- `cite_r()`
    - Fixed incorrect handling of `survival-book` reference

### New functions
- `apa_print.lsmobj()`
- `apa_print.summary.ref.grid()`
- `apa_print.glht()`
- `apa_print.summary.glht()`

### Misc
- New global option `papaja.na_string` with default value `"NA"` used by `apa_table()` and `printnum()`
- Updated example file and README


# papaja 0.1.0.9470

### Template
- The template now automatically invokes an R-based pandoc filter that replaces `&` with `and` in in-text citations to fully adhere to APA citation guidelines; (see #53)
- Initial chunk in template now is set to use chunk option `include = FALSE`.

### Existing functions
- `apa_table.latex()`
    - If `landscape = TRUE` tables are now always in `longtable` environments to improve interplay with the LaTeX package `endfloat` and correctly honoring the `figsintext` option in the YAML header (reported by @m-Py, #90)
    - Added clarification that the chunk option `results = "asis"` is required when `apa_table()` is called (requested by 	RMHogervorst, #93)
- `apa_print.lm()`
    - Calling `lm()` on a `data.frame` columns by using `$` resulted in regression term names that broke `apa_table()` (reported by @m-Py, #87)
- `render_appendix()`
    - Now builds on `knitr::knit_child()` to fix a bug causing figures not to render in the appendix; `render_appendix()` now has to be called within an R Markdown document. (reported by @fdabl, #70)
- `apa_*plot()`
    - Expressions can now be used as legend titles (see #94)



# papaja 0.1.0.9456

### Template
- `apa6_pdf` and `apa6_word` now adapt `pdf_document2()` and `word_document2()` from the **bookdown** package. This enables the use of **bookdown** cross-referencing syntax including automatically generated table and figure labels as detailed [here](https://bookdown.org/yihui/bookdown/cross-references.html) (see #2).
- LaTeX
    - If `numbersections: true` numbering now starts at the body of the text. The abstract is no longer a numbered heading (reported by @lnalborczyk, #85)
    - New option `footnotelist` to create list of footnotes at the end of the document (see #74)
- Word
    - Fixed bug that the wordcount set in the YAML header was not reported on the title page of Word documents (reported by @TobiasKoeln)

### Existing functions
- `apa_table.latex()`
    - Fixed bug causing longtables and and landscape tables to appear in text rather than at the end of the document when `figsintext: false` (reported by @m-Py, #64).
    - Fixed bug causing LaTeX to break when table contains `%`. All unescaped `%` are now automatically escaped even if `escape = FALSE` (reported by @m-Py, #66).

### Vignette
- Corrected various typos (reported by JooYoung Seo)


# papaja 0.1.0.9440

### Template
- Fixed bug in inline hook that omitted single numbers (reported by @hcp4715, #79)
- Chunk option default changed to `results = "markup"` (see #77)

### Existing functions
- `apa_barplot()` can calculate `ylim` when measure of dispersion cannot be calculated (#76)
- `apa_beeplot()` brightens data points instead of using transparency
- `apa_print()`
    - Renamed returned list elements to `statistic`, `estimate`, `full_result`, and `table` (#67)
    - All methods now return lists of the same structure (`table` may be `NULL`)
- `apa_print.lm()` has new option `observed_predictors` to calculate proper R^2 confidence intervals


### Misc
- MBESS package is now suggested and not imported



# papaja 0.1.0.9423

### Template
- Fixed a bug causing a LaTeX error in `jou` mode (reported by @saladspices, #48)
- Default `lang` is now `english`; fixed a pandoc-citeproc bug (reported by @mvuorre and @sebastiansauer, #56 and #57; fixed by @mvuorre, #58)
- Removed `author` field `department`, information can be supplied as part of the `address` field
- New option `author_note`; `note` is used to add additional information to the title page
- Changed heading "Sample" to "Participants"
- Fixed bug that supplied CSL file was ignored
- Proper author name concatination on title page (reported by Ulf Mertens)
- LaTeX
    - New options `figurelist` and `tablelist` to create lists of figure or table captions at the end of the document
    - Added `upgreek` package
    - Fixed bug causing `figsintext` to have no effect (reported by @mattsigal, #61)
    - Added page break before reference section (reported by @mattsigal, #60)
- Word
    - Fixed bug causing a deranged title page (reported by @mvuorre, #59)
    - Figure captions are now preceded by "Figure."

### Existing functions
- `apa_print()`
    - Improved type setting of predictors in tables
    - New supports `car::LeveneTest()` output
    - Dropped redundant df-column in `apa_print.lm()` table
    - Fixed bug causing the omission of R^2 confidence intervalls in `apa_print.lm()` when F-values were small
    - Fixed bug in `apa_print.list()` causing an error with only two models
    - Refactored `apa_print()`-family for ANOVA objects
        - Only df corrected for nonsphericity are reported with two decimals
        - Added indication of sphericity correction to `apa_print.Anova.mlm()` table
        - `print_anova()` rounds effect size estimates to three decimals
        - Can now calculate eta^2 effect sizes
        - New option `intercept` determines if intercept tests are included in the output
        - New option `mse` to report mean squared error terms (thanks to @TomHardwicke for reporting early bugs, #49)
- `apa_table()`
    - Dropped redundant `row_names` option
    - New option `stub_indents` adds title sections to indented rows
    - Renamed option `added_colnames` to `added_stub_head`
    - `apa_table.latex()`
        - New option `col_spanners` to add column spanners
        - New option `small` to decrease font size for large tables
        - Improvements to long and landscape tables
- `apa_barplot()`
    - Now works with white space in factor names
    - Improvements to automatic setting of `ylim`
    - Fixed bug that led to the incorrect drawing of user-specified colors
    - Only one legend is drawn for multiple facets
    - New option `args_arrows` to customize error bars
    - Invisibly returns means and measures of dispersion
- `cite_r()`
    - New options `pkgs` and `withhold` to black- or whitelist packages (#50)
    - Output retains structure if footnote = TURE but no bib-file is found
- `r_refs()`
    - Now handles packages with mulitple references (reported by @mattsigal, #65)
- `conf_int()`
    - Fixed bug that led to negative values

### New functions
- `apa_print.afex_aov()` (#47)
- `apa_print.list()` (#27)
- `wsci()`
- `apa_lineplot()`
- `apa_beeplot()` (thanks to @TobiasKoeln for reporting early bugs, #72)
- `theme_apa()`
- `render_appendix()`
- `fetch_zotero_refs()`
- `in_paren()`

### Dropped functions
- `fetch_web_refs()` evolved into a clone of `downloder::download()`
- `apa_prepare_doc()` is no longer needed

### Misc
- papaja sets global options `papaja.mse` and `papaja.sphericity_correction` among others
- Updated example manuscript
- Added vignette



# papaja 0.1.0.9054

### Template
- Added `wordcount` parameter to YAML header
- Changed YAML header structure to support proper author and affiliation information (requires updates of existing Rmd-files)
- LaTeX: Changed SVG device to PS
- Removed author field 'department' from the YAML header
- Added LaTeX fix for tightlist-bug in pandoc 1.14
- Fixed extra comma in automatically generated corresponding author line when no note is provided

### Existing functions
- `apa_print()`
    - Added `table` output-element to `apa_print.lm()` containing complete regression table
    - Refactored and tested `apa_print()`-family for ANOVA objects
    - Added `table` output-element to `apa_print()`-family for ANOVA objects containing complete ANOVA table
    - First experimental implementation of F-test model comparisons (`car::leveneTest` objects)
    - Estimates in `apa_print.htest()` and `apa_print.lm()` can be formatted by passing options to `printnum()`
- `apa_table()`
    - Added `midrules` option for `apa_table.latex()` and cleaned up LaTeX output
    - Fixed vertical space between table and table notes for `apa_table.latex()`
    - Bug fixes
- `r_refs()`
    - Now attempts to create references for packages used in cached chunks
    - Creates proper references based on citation file if available instead of generating default references from description file
- `apa_prepare_doc()`
    - Fixed bug when `lang` was not set in YAML header
- `apa_barplot()`
    - Fixed ignorance of user specified color for one factorial plots
    - Fixed default ylim choice to consider the lower bound of confidence intervals
- `apa_table()`
    - Removed redundant `row_name` parameter
    - Fixed escaped multicolumn-headings in `apa_table.latex()`

### New functions
- `apa_print.anova.mlm()`
- `apa_barplot()`
- `cite_r()`
- `fetch_web_refs()`

### Misc
- Set up proper package structure for automatic unit testing
- Created news file
- Fixed some typos in the example document



# papaja 0.1.0.9000

- Initial release on GitHub
