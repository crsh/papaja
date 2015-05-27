# papaja 0.1.0.9022

Template:
- Added `wordcount` parameter to YAML header
- Changed YAML header structure to support proper author and affiliation information (requires updates of existing Rmd-files)
- LaTeX: Changed SVG device to PS

Existing functions:
- `apa_print()`
    - Refactored and tested for different ANOVA objects
    - First experimental implementation of F-test model comparisons (e.g., `lm` objects)
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
- `apa_barplot()`
- `cite_r()`

Misc:
- Set up proper package structure for automatic unit testing
- Created news file

# papaja 0.1.0.9000

- Initial release
