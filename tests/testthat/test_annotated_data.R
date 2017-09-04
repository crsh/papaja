context("annotated_data")

test_that(
  "droplevels,annotated_factor-method"
  , {
    anno <- new("vector_annotation", label = "lab", unit = "un")
    object_1 <- new("annotated_factor", 1:4, levels = letters[1:10], annotation = anno, label = "lab")
    object_2 <- droplevels(object_1, exclude = "b")
    object_3 <- droplevels(object_1)

    object_4 <- droplevels.factor(x = factor(letters[1:4], levels = letters[1:10]), exclude = "b")

    # test case with excluded levels
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_factor"
        , object_4
        , levels = levels(object_4) # base behavior changed recently, so just check consistency with currently installed version
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


test_that(
  "show-methods for annotated_vector and annotated_factor"
  , {
    object_1 <- new("annotated_integer", .Data = 1:4)
    object_2 <- new("annotated_factor", .Data = 1:4, levels = letters[1:5])

    expect_identical(
      object = capture.output(show(object_1))
      , expected = c("An object of formal class \"annotated_integer\":", "[1] 1 2 3 4")
    )
    expect_identical(
      object = capture.output(show(object_2))
      , expected = c("An object of formal class \"annotated_factor\":", "[1] a b c d", "Levels:  a b c d e")
    )
  }
)



test_that(
  "rep-methods for annotated_vector and annotated-factor"
  , {
    anno <- new("vector_annotation", label = "label1", unit = "unit1")
    object_1 <- rep(new("annotated_numeric", 1:4+.1, annotation = anno, label = "label1"), each = 2, times = 3)
    object_2 <- rep(new("annotated_factor", .Data = 1:2, levels = letters[1:6], annotation = anno, label = "label1"), each = 2, times = 3)

    expect_identical(
      object = object_1
      , expected = new("annotated_numeric", rep(1:4+.1, each = 2, times = 3), label = "label1", annotation = anno)
    )
    expect_identical(
      object = object_2
      , expected = new("annotated_factor", .Data = rep(1:2, each = 2, times = 3), levels = letters[1:6], annotation = anno, label ="label1")
    )
  }
)

# relevel
# rep
# annotate
