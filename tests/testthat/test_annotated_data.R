context("annotated_data")

test_that(
  "droplevels,annotated_factor-method"
  , {
    anno <- new("vector_annotation", label = "lab", unit = "un")
    object_1 <- new("annotated_factor", 1:4, levels = letters[1:10], annotation = anno, label = "lab")
    object_2 <- droplevels(object_1, exclude = "b")
    object_3 <- droplevels(object_1)

    # test case with excluded levels
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_factor"
        , c(1L, NA_integer_, 2:3)
        , levels = letters[c(1, 3, 4)]
        , annotation = anno
        , label = "lab"
      )
    )

    # test standard case
    expect_identical(
      object = object_3
      , expected = new(
        "annotated_factor"
        , 1:4
        , levels = letters[1:4]
        , annotation = anno
        , label = "lab"
      )
    )
  }
)


# show
# relevel
# rep
# annotate
