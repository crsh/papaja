context("variable_label()")

test_that(
  "assign_label.data.frame()"
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
              "labelled"
              , "integer"
              )
            )
          , b = structure(
            4:6
            , label = "label2"
            , class = c(
              "labelled"
              , "integer"
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

test_that(
  "variable_label.data.frame() assignment statement"
  , {
    a <- as.numeric(1:3)
    b <- as.character(4:6)
    c <- data.frame(a, b, stringsAsFactors = FALSE)
    variable_label(c) <- c("label1", "label2")

    expect_equal(
      object = c
      , expected = structure(
        list(
          a = structure(
            1:3
            , label = "label1"
            , class = c(
              "labelled"
              , "numeric"
            )
          )
          , b = structure(
            as.character(4:6)
            , label = "label2"
            , class = c(
              "labelled"
              , "character"
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



context("methods for class 'labelled'")

test_that(
  "factor.labelled()"
  , {
    a <- factor(1:4, ordered = TRUE)
    variable_label(a) <- "label1"
    b <- factor.labelled(a, levels = 4:1)

    expect_equal(
      object = b
      , expected = structure(
        c(4L, 3L, 2L, 1L)
        , .Label = c("4", "3", "2", "1")
        , class = c("labelled", "ordered", "factor")
        , label = "label1"
      )
    )
  }
)


test_that(
  "droplevels.labelled()"
  , {
    a <- as.character(1:3)
    b <- as.character(4:6)
    c <- data.frame(a, b, stringsAsFactors = TRUE)
    variable_label(c) <- c("label1", "label2")
    d <- droplevels(c[2:3, ])

    expect_equal(
      object = d
      , expected = structure(
        list(
          a = structure(
            1:2
            , .Label = c("2", "3")
            , class = c("labelled", "factor")
            , label = "label1"
          )
          , b = structure(
            1:2
            , .Label = c("5", "6")
            , class = c("labelled", "factor")
            , label = "label2"
          )
        )
        , .Names = c("a", "b")
        , row.names = 2:3
        , class = "data.frame"
      )
    )
  }
)

test_that(
  "[.labelled"
  , {
    a <- as.numeric(1:3)
    b <- as.character(4:6)
    c <- data.frame(a, b, stringsAsFactors = FALSE)
    variable_label(c) <- c("label1", "label2")
    d <- c[1:2, ]

    expect_equal(
      object = d
      , expected = structure(
        list(
          a = structure(
            1:2
            , label = "label1"
            , class = c(
              "labelled"
              , "numeric"
            )
          )
          , b = structure(
            as.character(4:5)
            , label = "label2"
            , class = c(
              "labelled"
              , "character"
            )
          )
        )
        , .Names = c(
          "a"
          , "b"
        )
        , row.names = c(
          NA
          , -2L
        )
        , class = "data.frame"
      )
    )
  }
)
