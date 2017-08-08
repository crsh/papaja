context("variable_label()")

test_that(
  "assign_label.data.frame()"
  , {
    object_a <- 1:3
    object_b <- 4:6
    object_c <- data.frame(object_a, object_b)
    object_d <- assign_label(object_c, value = c("label1", "label2"))

    expect_equal(
      object = object_d
      , expected = structure(
        list(
          object_a = structure(
            1:3
            , label = "label1"
            , class = c(
              "labelled"
              , "integer"
              )
            )
          , object_b = structure(
            4:6
            , label = "label2"
            , class = c(
              "labelled"
              , "integer"
            )
          )
        )
        , .Names = c(
          "object_a"
          , "object_b"
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
    object_a <- as.numeric(1:3)
    object_b <- as.character(4:6)
    object_c <- data.frame(object_a, object_b, stringsAsFactors = FALSE)
    variable_label(object_c) <- c("label1", "label2")

    expect_equal(
      object = object_c
      , expected = structure(
        list(
          object_a = structure(
            1:3
            , label = "label1"
            , class = c(
              "labelled"
              , "numeric"
            )
          )
          , object_b = structure(
            as.character(4:6)
            , label = "label2"
            , class = c(
              "labelled"
              , "character"
            )
          )
        )
        , .Names = c(
          "object_a"
          , "object_b"
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
    object_a <- factor(1:4, ordered = TRUE)
    variable_label(object_a) <- "label1"
    object_b <- factor.labelled(object_a, levels = 4:1)

    expect_equal(
      object = object_b
      , expected = structure(
        c(4L, 3L, 2L, 1L)
        , .Label = c("4", "3", "2", "1")
        , class = c("labelled", "ordered", "factor")
        , label = "label1"
      )
    )
  }
)


# test_that(
#   "droplevels.labelled()"
#   , {
#     object_a <- factor(1:3)
#     object_b <- factor(4:6)
#     object_c <- data.frame(object_a, object_b, stringsAsFactors = TRUE)
#     variable_label(object_c) <- c("label1", "label2")
#     object_d <- droplevels(object_c[1:2, ])
#
#     expect_equal(
#       object = object_d
#       , expected = structure(
#         list(
#           object_a = structure(
#             1:2
#             , .Label = c("1", "2")
#             , class = c("labelled", "factor")
#             , label = "label1"
#           )
#           , object_b = structure(
#             1:2
#             , .Label = c("4", "5")
#             , class = c("labelled", "factor")
#             , label = "label2"
#           )
#         )
#         , .Names = c("object_a", "object_b")
#         , row.names = 1:2
#         , class = "data.frame"
#       )
#     )
#   }
# )

test_that(
  "[.labelled"
  , {
    object_a <- as.numeric(1:3)
    object_b <- as.character(4:6)
    object_c <- data.frame(object_a, object_b, stringsAsFactors = FALSE)
    variable_label(object_c) <- c("label1", "label2")
    object_d <- object_c[1:2, ]

    expect_identical(
      object = object_d
      , expected = structure(
        list(
          object_a = structure(
            structure(c(1, 2))
            , label = "label1"
            , class = c("labelled", "numeric")
          )
          , object_b = structure(
            structure(c("4", "5"))
            , label = "label2"
            , class = c("labelled", "character")
          )
        )
        , .Names = c(
          "object_a"
          , "object_b"
        )
        , row.names = 1:2
        , class = "data.frame"
      )
    )
  }
)
