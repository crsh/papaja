context("annotated_data")

test_that(
  "droplevels,annotated_factor-method"
  , {
    anno <- new("vector_annotation", label = "lab", unit = "un")
    object_1 <- new("annotated_factor", .Data = 1:4, levels = letters[1:10], annotation = anno, label = "lab")
    object_2 <- droplevels(object_1, exclude = "b")
    object_3 <- droplevels(object_1)

    object_4 <- droplevels.factor(x = factor(letters[1:4], levels = letters[1:10]), exclude = "b")

    # test case with excluded levels
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_factor"
        , object_4 # base behavior changed recently, so just check consistency with currently installed version
        , annotation = anno
        , label = "lab"
      )
    )

    # test standard case
    expect_identical(
      object = object_3
      , expected = new(
        "annotated_factor"
        , .Data = 1:4
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
      , expected = c("Variable label     : ", "Unit of measurement: ", "[1] 1 2 3 4")
    )
    expect_identical(
      object = capture.output(show(object_2))
      , expected = c("Variable label     : ", "Unit of measurement: ", "[1] a b c d", "Levels:  a b c d e")
    )
  }
)


test_that(
  "relevel,annotated_factor,character/integer-method"
  , {
    object_1 <- new("annotated_factor", .Data = 1:3, levels = letters[1:3], label = "label1", annotation = new("vector_annotation", label = "label1", unit = "unit1"))
    object_2 <- relevel(object_1, ref = c("c", "b"))
    object_3 <- new("annotated_factor", .Data = 3:1, levels = letters[3:1], label = "label1", annotation = new("vector_annotation", label = "label1", unit = "unit1"))
    object_4 <- relevel(object_1, ref = 2L)
    object_5 <- new("annotated_factor", .Data = c(2, 1, 3), levels = letters[c(2, 1, 3)], label = "label1", annotation = new("vector_annotation", label = "label1", unit = "unit1"))

    expect_identical(
      object = object_2
      , expected = object_3
    )
    expect_identical(
      object = object_4
      , expected = object_5
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

test_that(
  "levels<-,annotated_factor-method"
  , {
    annotation <- new(
      "vector_annotation"
      , label = "label1"
      , unit = "unit1"
    )

    object_1 <- new(
      "annotated_factor"
      , .Data = 1:3
      , levels = letters[1:3]
      , annotation = annotation
      , label = annotation@label
    )

    levels(object_1) <- letters[4:9]

    object_2 <- new(
      "annotated_factor"
      , .Data = 1:3
      , levels = letters[4:9]
      , annotation = annotation
      , label = "label1"
    )

    expect_identical(
      object = object_1
      , expected = object_2
    )
  }
)

test_that(
  "names<-, annotated_vector-method"
  , {
    # Construct a standard annotated_integer
    annotation <- new("vector_annotation", label = "label1", unit = "kg")
    object_1 <- new("annotated_integer", .Data = 6:4, label = annotation@label, annotation = annotation)
    object_2 <- object_1
    object_3 <- object_1
    object_4 <- object_1
    # If full names are supplied, are they properly assigned
    names(object_2) <- letters[3:1]
    # If partial names are supplied, are NAs generated properly
    names(object_3) <- c(NA_character_, "b")
    # If NULL-names are assigned, vector should keep class annotated_integer
    names(object_4) <- NULL

    expect_identical(
      object = object_2
      , expected = new(
        "annotated_named_integer"
        , .Data = 6:4
        , names = letters[3:1]
        , label = "label1"
        , annotation = annotation
      )
    )
    expect_identical(
      object = object_3
      , expected = new(
        "annotated_named_integer"
        , .Data = 6:4
        , names = c(NA_character_, "b", NA_character_)
        , label = "label1"
        , annotation = annotation
      )
    )
    expect_identical(
      object = object_4
      , expected = object_1
    )
  }
)

test_that(
  "as,annotated_vector,annotated_named_vector-methods"
  , {
    # Prepare
    annotation <- new(
      "vector_annotation"
      , label = "label1"
      , unit = "kg"
    )

    # Coercion from unnamed to named
    object_1 <- new(
      "annotated_integer"
      , .Data = 6:8
      , label = "label1"
      , annotation = annotation
    )

    object_2 <- as(object_1, "annotated_named_integer")

    # Coercion from named to unnamed
    object_3 <- new(
      "annotated_named_integer"
      , .Data = 2:5
      , names = letters[1:4]
      , label = "label1"
      , annotation = annotation
    )
    object_4 <- as(object_3, "annotated_integer")

    # Expectations
    expect_identical(
      object = object_2
      , expected = new(
        "annotated_named_integer"
        , .Data = 6:8
        , label = "label1"
        , annotation = annotation
      )
    )

    expect_identical(
      object = object_4
      , expected = new(
        "annotated_integer"
        , .Data = 2:5
        , label = "label1"
        , annotation = annotation
      )
    )
  }
)
