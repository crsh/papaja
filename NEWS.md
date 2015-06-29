# Pre release

Template:
- Removed author field 'department' from the YAML header
- Added LaTeX fix for tightlist-bug in pandoc 1.14
- Fixed extra comma in automatically generated corresponding author line when no note is provided

Exisiting functions:
- `apa_barplot()`
    - Fixed ignorance of user specified color for one factorial plots
    - Fixed default ylim choice to consider the lower bound of confidence intervals
- `apa_table()`
    - Removed redundant `row_name` parameter
    - Fixed escaped multicolumn-headings in `apa_table.latex()`
    
Misc:
- Fixed some typos in the example document

# papaja 0.1.0.9054

Template:
- Added `wordcount` parameter to YAML header
- Changed YAML header structure to support proper author and affiliation information (requires updates of existing Rmd-files)
- LaTeX: Changed SVG device to PS

Existing functions:
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

New functions:
- `apa_print.anova.mlm()`
- `apa_barplot()`
- `cite_r()`
- `fetch_web_refs()`

Misc:
- Set up proper package structure for automatic unit testing
- Created news file

# papaja 0.1.0.9000

- Initial release
