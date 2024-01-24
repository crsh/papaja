#' Lookup Tables for Column Names and Variable Labels
#'
#' `apa_print()` converts many statistical output objects to output objects that
#' contain an `apa_results_table/data.frame` with standardized column names and
#' variable labels. For this purpose, it relies on the lookup tables provided
#' here.
#'
#' @rdname lookup_tables
# #' @examples
# #'   papaja:::lookup_names[["mean.of.x"]]
# #'   papaja:::lookup_labels[["mean.of.x"]]
#' @keywords internal

lookup_names <- c(
  # nuisance parameters
  "Sum.Sq"      = "sumsq"
  , "Sum.of.Sq" = "sumsq"
  , "Error.SS" = "sumsq_err"
  , "RSS"      = "sumsq_err"
  , "Mean.Sq" = "meansq"
  , "logLik"  = "loglik"
  , "AIC"     = "AIC"
  , "BIC"     = "BIC"
  , "npar"    = "n.parameters"
  , "alternative" = "alternative"
  , "Deviance"    = "deviance"
  , "Resid..Dev"  = "residual.deviance" # from broom
  # term
  , "Effect"     = "term"
  , "Term"       = "term"
  , "Predictor"  = "term"
  , "model.term" = "term"
  , "model"        = "model"
  # estimate
  , "Estimate"                = "estimate"
  , "estimate"                  = "estimate"
  , "mean.of.the.differences"   = "estimate"
  , "mean.difference"           = "estimate"
  , "cor"                       = "estimate"
  , "rho"                       = "estimate"
  , "tau"                       = "estimate"
  , "mean.of.x"                 = "estimate"
  , "mean"                      = "estimate"
  , "X.pseudo.median"           = "estimate"
  , "difference.in.location"    = "estimate"
  , "difference.in.means"       = "estimate"
  , "difference.in.proportions" = "estimate"
  , "coefficients"              = "estimate"
  , "delta"                     = "estimate"
  , "proportion"                = "estimate"
  , "p"                         = "estimate"
  # binomial test:
  , "probability.of.success"    = "estimate"
  # estimate from effectsize package
  , "Eta2"             = "estimate"
  , "Eta2_partial"     = "estimate"
  , "Eta2_generalized" = "estimate"
  , "Omega2"             = "estimate"
  , "Omega2_partial"     = "estimate"
  , "Omega2_generalized" = "estimate"
  , "Epsilon2"             = "estimate"
  , "Epsilon2_partial"     = "estimate"
  , "Epsilon2_generalized" = "estimate"
  # ----
  , "conf.int"   = "conf.int"
  , "hd.int"     = "conf.int"
  , "stderr"     = "std.error"
  , "std.err"    = "std.error"
  , "Std.Error"  = "std.error"
  , "Std..Error" = "std.error"
  , "sigma"      = "std.error"
  # multivariate.statistic
  , "Pillai"    = "multivariate.statistic"
  , "Wilks"     = "multivariate.statistic"
  , "Roy"       = "multivariate.statistic"
  , "Hotelling.Lawley" = "multivariate.statistic"
  # statistic
  , "t"         = "statistic"
  , "t.value"   = "statistic"
  , "tstat"     = "statistic"
  , "statistic" = "statistic"
  , "approx.F"  = "statistic"
  , "F.value"   = "statistic"
  , "F"         = "statistic"
  , "F.ratio"   = "statistic"
  , "LRT"       = "statistic"
  , "Chisq"     = "statistic"
  , "chisq"     = "statistic"
  , "X.squared" = "statistic"
  , "V"         = "statistic"
  , "W"         = "statistic"
  , "S"         = "statistic"
  , "T"         = "statistic"
  , "z"         = "statistic"
  , "z.value"   = "statistic"
  , "bf"        = "statistic"
  , "bf10"      = "statistic"
  , "bf01"      = "statistic"
  , "logbf"        = "statistic"
  , "logbf10"      = "statistic"
  , "logbf01"      = "statistic"
  , "Bartlett.s.K.2"          = "statistic"
  , "Bartlett.s.K.squared"    = "statistic"
  # df, df.residual
  , "multivariate.df1" = "multivariate.df"
  , "multivariate.df2" = "multivariate.df.residual"
  , "parameter"  = "df"
  , "df"         = "df"
  , "Df"         = "df"
  , "DF"         = "df"
  , "Chi.Df"     = "df"
  , "parameter1" = "df"
  , "parameter2" = "df.residual"
  , "df1" = "df"
  , "df2" = "df.residual"
  , "num.Df"     = "df"
  , "den.Df"     = "df.residual"
  , "NumDF"      = "df"
  , "DenDF"      = "df.residual"
  , "numDF"      = "df"
  , "denDF"      = "df.residual"
  , "num.df"     = "df"
  , "denom.df"   = "df.residual"
  , "den.df"     = "df.residual"
  , "Res.Df"     = "df.residual"
  , "Resid..Df"  = "df.residual"
  # p.value
  , "p.value"    = "p.value"
  , "Pr..Chisq." = "p.value"
  , "Pr..F."     = "p.value"
  , "Pr..PB."    = "p.value"
  , "Pr...t.."   = "p.value"
  , "Pr...z.."   = "p.value"
  , "pvalues"    = "p.value"
  , "error"      = "mcmc.error"
)



#' @rdname lookup_tables
#' @keywords internal

lookup_labels <- c(
  # nuisance parameters
  "Sum.Sq"      = "$\\mathit{SS}$"
  , "Sum.of.Sq" = "$\\mathit{SS}$"
  , "RSS"       = "$\\mathit{SS}_{\\mathrm{res}}$"
  , "Mean.Sq" = "$\\mathit{MS}$"
  , "logLik"  = "$\\ln L$"
  , "AIC"     = "$\\mathit{AIC}$"
  , "BIC"     = "$\\mathit{BIC}$"
  , "npar"    = "$k$"
  , "alternative" = "$\\mathcal{H}_1$"
  , "Deviance" = "$\\mathit{Dev}$"
  , "Resid..Dev" = "$\\mathit{Dev}_{\\mathrm{res}}$"
  # term
  , "Effect"     = "Effect"
  , "Term"       = "Term"
  , "Predictor"  = "Predictor"
  , "model.term" = "Term"
  , "model"        = "Model"
  , "contrast"   = "Contrast"
  # estimate
  , "cor"                       = "$r$"
  , "rho"                       = "$r_{\\mathrm{s}}$" # capital or small S???
  , "tau"                       = "$\\uptau$"
  , "mean.of.x"                 = "$M$"
  , "mean"                      = "$M$"
  , "X.pseudo.median"           = "$\\mathit{Mdn}^*$"
  , "mean.of.the.differences"   = "$M_D$"
  , "mean.difference"           = "$M_D$"
  , "difference.in.location"    = "$\\Delta \\mathit{Mdn}$"
  , "difference.in.means"       = "$\\Delta M$"
  , "difference.in.proportions" = "$\\Delta \\hat\\pi$"
  , "proportion"                = "$\\hat\\pi$"
  , "p"                         = "$\\hat\\pi$"
  , "probability.of.success"    = "$\\hat\\pi$"
  , "delta"                     = "$\\delta$"
  # estimate from effectsize package
  , "Eta2"             = "$\\hat{\\eta}^2$"
  , "Eta2_partial"     = "$\\hat{\\eta}^2_p$"
  , "Eta2_generalized" = "$\\hat{\\eta}^2_G$"
  , "Omega2"             = "$\\hat{\\omega}^2$"
  , "Omega2_partial"     = "$\\hat{\\omega}^2_p$"
  , "Omega2_generalized" = "$\\hat{\\omega}^2_G$"
  , "Epsilon2"             = "$\\hat{\\epsilon}^2$"
  , "Epsilon2_partial"     = "$\\hat{\\epsilon}^2_p$"
  , "Epsilon2_generalized" = "$\\hat{\\epsilon}^2_G$"
  # standard error
  , "stderr"     = "$\\mathit{SE}$"
  , "std.err"    = "$\\mathit{SE}$"
  , "Std.Error"  = "$\\mathit{SE}$"
  , "Std..Error" = "$\\mathit{SE}$"
  , "sigma"      = "$\\mathit{SE}$"
  # multivariate.statistic
  , "Pillai"           = "$V$"
  , "Wilks"            = "$\\Lambda$"
  , "Hotelling.Lawley" = "$T$"
  , "Roy"              = "$\\theta$"
  # statistic
  , "t"         = "$t$"
  , "tstat"     = "$t$"
  , "t.value"   = "$t$"
  , "F.value"   = "$F$"
  , "F"         = "$F$"
  , "F.ratio"   = "$F$"
  , "approx.F"  = "$F$"
  , "LRT"       = "$\\chi^2$"
  , "chisq"     = "$\\chi^2$"
  , "Chisq"     = "$\\chi^2$"
  , "X.squared" = "$\\chi^2$"
  , "W"         = "$W$"
  , "V"         = "$V$"
  , "S"         = "$S$"
  , "T"         = "$T$"
  , "z"         = "$z$"
  , "z.value"   = "$z$"
  , "bf"        = "$\\mathrm{BF}$"
  , "bf10"      = "$\\mathrm{BF}_{\\textrm{10}}$"
  , "bf01"      = "$\\mathrm{BF}_{\\textrm{01}}$"
  , "logbf"        = "$\\log \\mathrm{BF}$"
  , "logbf10"      = "$\\log \\mathrm{BF}_{\\textrm{10}}$"
  , "logbf01"      = "$\\log \\mathrm{BF}_{\\textrm{01}}$"
  , "Bartlett.s.K.2"          = "$K^2$"
  , "Bartlett.s.K.squared"    = "$K^2$"
  # df, df.residual
  , "multivariate.df" = "$\\mathit{df}$"
  , "multivariate.df.residual" = "$\\mathit{df}_{\\mathrm{res}}$"
  , "parameter"  = "$\\mathit{df}$"
  , "df"         = "$\\mathit{df}$"
  , "Df"         = "$\\mathit{df}$"
  , "DF"         = "$\\mathit{df}$"
  , "Chi.Df"     = "$\\mathit{df}$"
  , "parameter1" = "$\\mathit{df}$"
  , "parameter2" = "$\\mathit{df}_{\\mathrm{res}}$"
  , "df1"        = "$\\mathit{df}$"
  , "df2"        = "$\\mathit{df}_{\\mathrm{res}}$"
  , "num.Df"     = "$\\mathit{df}$"
  , "den.Df"     = "$\\mathit{df}_{\\mathrm{res}}$"
  , "NumDF"      = "$\\mathit{df}$"
  , "DenDF"      = "$\\mathit{df}_{\\mathrm{res}}$"
  , "numDF"      = "$\\mathit{df}$"
  , "denDF"      = "$\\mathit{df}_{\\mathrm{res}}$"
  , "num.df"     = "$\\mathit{df}$"
  , "denom.df"   = "$\\mathit{df}_{\\mathrm{res}}$"
  , "den.df"     = "$\\mathit{df}_{\\mathrm{res}}$"
  , "Res.Df"     = "$\\mathit{df}_{\\mathrm{res}}$"
  , "Resid..Dev" = "$\\mathit{df}_{\\mathrm{res}}$"
  # p.value
  , "p.value"     = "$p$"
  , "Pr...t.."    = "$p$"
  , "Pr...z.."    = "$p$"
  , "pvalues"     = "$p$"
  , "Pr..Chisq."  = "$p$"
  , "Pr..F."      = "$p$"
  , "Pr..PB."     = "$p$"
  , "adj.p.value" = "$p_\\mathrm{adj}$"
  , "error"       = "$\\pm\\%$"
)


#' Lookup Table for P Value/Confindence Interval Adjustment Names
#'
#' `apa_print()` converts many statistical output objects that include
#' inferential statistics adjusted for multiple comparisons. To make these
#' adjustments transparent the statistics get an index with the
#' corresponding name. This function returns the proper names for these indices.
#'
#' @rdname lookup_tables
# #' @examples
# #'   papaja:::lookup_adjust_names("fdr")
#' @keywords internal

lookup_adjust_names <- function(x) {
  switch(
    x
    , "holm"       = c(p.value = "Holm"            , conf.int = "Bonferroni")
    , "hochberg"   = c(p.value = "Hochberg"        , conf.int = "Bonferroni")
    , "hommel"     = c(p.value = "Hommel"          , conf.int = "Bonferroni")
    , "bonferroni" = c(p.value = "Bonferroni"      , conf.int = "Bonferroni")
    , "BH"         = c(p.value = "BH"              , conf.int = "Bonferroni")
    , "BY"         = c(p.value = "BY"              , conf.int = "Bonferroni")
    , "fdr"        = c(p.value = "FDR"             , conf.int = "Bonferroni")
    , "tukey"      = c(p.value = "Tukey"           , conf.int = "Tukey")
    , "scheffe"    = c(p.value = "Scheff\\'e"      , conf.int = "Scheff\\'e")
    , "sidak"      = c(p.value = "Sidak"           , conf.int = "Sidak")
    , "dunnettx"   = c(p.value = "Dunnett"         , conf.int = "Dunnett")
    , "mvt"        = c(p.value = "MV \\mathit{t}"  , conf.int = "MV \\mathit{t}")
    , "adj"
  )
}


#' Lookup Table for Generated Words and Phrases
#'
#' Some words and phrases used throughout a papaja manuscript are automatically
#' generated and need to vary when the locale of a document is changed. This
#' function returns the words and phrases by language.
#'
#' @param x Integer. Locale.
#' @keywords internal

localize <- function(x) {
  switch(
    x
    , list( # Default
      author_note = "Author note"
      , abstract = "Abstract"
      , keywords = "Keywords"
      , word_count = "Word count"
      , table = "Table"
      , figure = "Figure"
      , note = "Note"
      , correspondence = "Correspondence concerning this article should be addressed to "
      , email = "E-mail"
    )
    , german = list(
      author_note = "Anmerkung des Autors"
      , abstract = "Zusammenfassung"
      , keywords = "Schl\u00fcsselw\u00f6rter"
      , word_count = "Wortanzahl"
      , table = "Tabelle"
      , figure = "Abbildung"
      , note = "Anmerkung"
      , correspondence = "Schriftverkehr diesen Artikel betreffend sollte adressiert sein an "
      , email = "E-Mail"
    )
    , dutch = list(
      author_note = "Over de auteur"
      , abstract = "Samenvatting"
      , keywords = "Trefwoorden"
      , word_count = "Aantal woorden"
      , table = "Tabel"
      , figure = "Figuur"
      , note = "Opmerking"
      , correspondence = "Correspondentie betreffende dit artikel wordt geadresseerd aan "
      , email = "E-mail"
    )
  )
}
