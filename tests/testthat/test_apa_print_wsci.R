test_that(
  "apa_print() for within-subjects CIs"
  , {
    data <- npk
    variable_labels(data) <- c(N = "Nitrogen $N$")
    x <- wsci(data = data, id = "block", dv = "yield", factors = c("N", "P"), level = .98)

    apa_x <- apa_print(x, digits = 3L)

    # some expectations
    # expect_apa_results(
    #  apa_x
    #  , labels = c("Nitrogen $N$", "P", "$M$", "98\\% CI")
    #)


  }
)
