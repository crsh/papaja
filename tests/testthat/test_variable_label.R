context("vaiable_label()")

test_that(
  "Variable labels: Labelling a data.frame"
  , {
    a <- 1:3
    b <- 4:6
    c <- data.frame(a, b)
    d <- assign_label(c, value = c("label1", "label2"))

    expect_equal(
      object = d
      , expected = structure(
        list(
          a = structure(
            1:3
            , label = "label1"
            , class = c(
              "integer"
              , "labelled"
              )
            )
          , b = structure(
            4:6
            , label = "label2"
            , class = c(
              "integer"
              , "labelled"
            )
          )
        )
        , .Names = c(
          "a"
          , "b"
        )
        , row.names = c(
          NA
          , -3L
        )
        , class = "data.frame"
      )
    )
  }
)

