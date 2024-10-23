test_that(
  "apa_print() for within-subjects CIs"
  , {
    data <- npk
    variable_labels(data) <- c(N = "Nitrogen $N$")
    wsci_out <- wsci(data = data, id = "block", dv = "yield", factors = c("N", "P"), level = .98)

    apa_wsci <- apa_print(wsci_out, digits = 3L)

    # some expectations
    expect_apa_results(
      apa_wsci
      , labels = list(
        N = "Nitrogen $N$"
        , P = "P"
        , estimate = "$M$"
        , conf.int = "98\\% CI"
      )
    )
    expect_identical(
      names(apa_wsci$estimate)
      , c("N0_P0", "N0_P1", "N1_P0", "N1_P1")
    )
    expect_identical(
      apa_wsci$estimate$N0_P0
      , "$M = 51.717$, 98\\% CI $[46.603, 56.831]$"
    )
  }
)
