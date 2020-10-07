
context("apa_print() for multcomp")

test_that(
  "glht() for multiple linear model"
  , {
    ### from the examples section
    lmod <- lm(Fertility ~ ., data = swiss)
    ### define coefficients of linear function directly
    K <- diag(length(stats::coef(lmod)))[-1,]
    rownames(K) <- names(stats::coef(lmod))[-1]
    K
    ### set up general linear hypothesis
    comparisons <- multcomp::glht(lmod, linfct = K)

    apa_comp <- apa_print(comparisons)
    expect_apa_results(
      apa_comp
      , labels = list(
        term        = "Term"
        , estimate  = "$b$"
        , conf.int  = "95\\% CI"
        , statistic = "$t(41)$"
        , p.value   = "$p$"
      )
    )

  }
)

test_that(
  "glht() for multiple comparisons procedure"
  , {
    amod <- aov(breaks ~ tension, data = warpbreaks)

    ### set up all-pair comparisons for factor `tension'
    ### using a symbolic description (`type' argument
    ### to `contrMat()')
    comparisons <- multcomp::glht(amod, linfct = multcomp::mcp(tension = "Tukey"))
    apa_comp2 <- apa_print(summary(comparisons))
    expect_apa_results(
      apa_comp2
      , labels = list(
        term        = "Term"
        , estimate  = "$b$"
        , conf.int  = "95\\% CI"
        , statistic = "$t(51)$"
        , p.value   = "$p$"
      )
    )
  }
)
