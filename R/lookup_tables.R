#' Lookup Tables for Column Names and Variable Labels
#'
#' `apa_print()` converts many statistical output objects to output objects that
#' contain an `apa_results_table/data.frame` with standardized column names and
#' variable labels. For this purpose, it relies on the lookup tables provided
#' here.
#'
#' @rdname lookup_tables
#' @md
#' @keywords internal

lookup_names <- c(
  # nuisance parameters
  "Sum.Sq"    = "sumsq"
  , "Mean.Sq" = "meansq"
  , "logLik"  = "loglik"
  , "AIC"     = "AIC"
  , "BIC"     = "BIC"
  , "npar"    = "n.parameters"
  # term
  , "Effect"  = "term"
  , "Term"    = "term"
  # estimate
  , "estimate"                = "estimate"
  , "mean.of.the.differences" = "estimate"
  , "cor"                     = "estimate"
  , "rho"                     = "estimate"
  , "tau"                     = "estimate"
  , "mean.of.x"               = "estimate"
  , "X.pseudo.median"         = "estimate"
  , "difference.in.location"  = "estimate"
  , "difference.in.means"     = "estimate"
  , "estimate.difference.in.means" = "estimate"
  , "coefficients"            = "estimate"
  # ----
  , "conf.int" = "conf.int"
  , "stderr"   = "std.error"
  , "std.err"  = "std.error"
  , "sigma"    = "std.error"
  # multivariate.statistic
  , "Pillai"    = "multivariate.statistic"
  , "Wilks"     = "multivariate.statistic"
  , "Roy"       = "multivariate.statistic"
  , "Hotelling.Lawley" = "multivariate.statistic"
  # statistic
  , "t"         = "statistic"
  , "tstat"     = "statistic"
  , "statistic" = "statistic"
  , "approx.F"  = "statistic"
  , "F value"   = "statistic"
  , "F"         = "statistic"
  , "LRT"       = "statistic"
  , "Chisq"     = "statistic"
  , "chisq"     = "statistic"
  , "X.squared" = "statistic"
  , "V"         = "statistic"
  , "W"         = "statistic"
  , "S"         = "statistic"
  , "T"         = "statistic"
  , "z"         = "statistic"
  , "Bartlett.s.K.2"          = "statistic"
  , "Bartlett.s.K.squared"    = "statistic"
  # df, df1, df2
  , "multivariate.df1" = "multivariate.df1"
  , "multivariate.df2" = "multivariate.df2"
  , "parameter"  = "df"
  , "df"         = "df"
  , "Df"         = "df"
  , "Chi.Df"     = "df"
  , "parameter1" = "df1"
  , "parameter2" = "df2"
  , "num.Df"     = "df1"
  , "den.Df"     = "df2"
  , "NumDF"      = "df1"
  , "DenDF"      = "df2"
  , "parameter.num.df"   = "df1"
  , "parameter.denom.df" = "df2"
  # p.value
  , "p.value"    = "p.value"
  , "Pr..Chisq." = "p.value"
  , "Pr..F."     = "p.value"
  , "Pr..PB."    = "p.value"
  , "pvalues"    = "p.value"
)



#' @rdname lookup_tables
#' @keywords internal

lookup_labels <- c(
  # nuisance parameters
  "Sum.Sq"    = "$\\mathit{SS}$"
  , "Mean.Sq" = "$\\mathit{MS}$"
  , "logLik"  = "$\\ln L$"
  , "AIC"     = "$\\mathit{AIC}$"
  , "BIC"     = "$\\mathit{BIC}$"
  , "npar"    = "$k$"
  # term
  , "Effect"   = "Effect"
  , "Term"     = "Term"
  # estimate
  , "cor"                     = "$r$"
  , "rho"                     = "$r_{\\mathrm{s}}$" # capital or small S???
  , "tau"                     = "$\\uptau$"
  , "mean.of.x"               = "$M$"
  , "X.pseudo.median"         = "$\\mathit{Mdn}^*$"
  , "mean.of.the.differences" = "$M_d$"
  , "difference.in.location"  = "$\\mathit{Mdn}_d$"
  , "difference.in.means"     = "$\\Delta M$"
  # standard error
  , "stderr"   = "$\\mathit{SE}$"
  , "std.err"  = "$\\mathit{SE}$"
  , "sigma"    = "$\\mathit{SE}$"
  # multivariate.statistic
  , "Pillai"           = "$V$"
  , "Wilks"            = "$\\Lambda$"
  , "Hotelling.Lawley" = "$T$"
  , "Roy"              = "$\\theta$"
  # statistic
  , "t"         = "$t$"
  , "tstat"     = "$t$"
  , "F.value"   = "$F$"
  , "F"         = "$F$"
  , "approx.F"  = "$F$"
  , "LRT"       = "$\\chi^2$"
  , "chisq"     = "$\\chi^2$"
  , "Chisq"     = "$\\chi^2$"
  , "X.squared" = "$\\chi^2$"
  , "W"                       = "$W$"
  , "V"                       = "$V$"
  , "S"                       = "$S$"
  , "T"                       = "$T$"
  , "z"                       = "$z$"
  , "Bartlett.s.K.2"          = "$K^2$"
  , "Bartlett.s.K.squared"    = "$K^2$"
  # df, df1, df2
  , "multivariate.df1" = "$\\mathit{df}_1$"
  , "multivariate.df2" = "$\\mathit{df}_2$"
  , "parameter" = "$\\mathit{df}$"
  , "df"        = "$\\mathit{df}$"
  , "Df"        = "$\\mathit{df}$"
  , "Chi.Df"    = "$\\mathit{df}$"
  , "num.Df"    = "$\\mathit{df}_1$"
  , "den.Df"    = "$\\mathit{df}_2$"
  , "NumDF"     = "$\\mathit{df}_1$"
  , "DenDF"     = "$\\mathit{df}_2$"
  , "parameter.num.df"   = "$\\mathit{df}_1$"
  , "parameter.denom.df" = "$\\mathit{df}_2$"
  # p.value
  , "p.value"    = "$p$"
  , "Pr..Chisq." = "$p$"
  , "Pr..F."     = "$p$"
  , "Pr..PB."    = "$p$"
  , "pvalues"    = "$p$"
)
