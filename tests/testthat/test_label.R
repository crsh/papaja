context("variable_label() replacement methods")


test_that(
  "variable_label<-,vector-method"
  , {
    object_1 <-1:4
    variable_label(object_1) <- "label_1"

    expect_equal(
      object = object_1
      , expected = new(
        "annotated_integer"
        , .Data = 1:4
        , label = "label_1"
        , annotation = new(
          "vector_annotation"
          , label = "label_1"
          , unit = character(0)
        )
      )
    )
  }
)

test_that(
  "variable_label<-,annotated_vector-method"
  , {
    object_1 <-new("annotated_integer", 1:4)
    variable_label(object_1) <- "label_1"

    expect_equal(
      object = object_1
      , expected = new(
        "annotated_integer"
        , .Data = 1:4
        , label = "label_1"
        , annotation = new(
          "vector_annotation"
          , label = "label_1"
          , unit = character(0)
        )
      )
    )
  }
)

test_that(
  "variable_label<-,factor-method"
  , {
    object_1 <- factor(letters[1:4])
    variable_label(object_1) <- "label_1"

    expect_equal(
      object = object_1
      , expected = new(
        "annotated_factor"
        , .Data = 1:4
        , label = "label_1"
        , annotation = new(
          "vector_annotation"
          , label = "label_1"
          , unit = character(0)
        )
        , levels = letters[1:4]
        , .S3Class = "factor"
      )
    )
  }
)

test_that(
  "variable_label<-,annotated_factor-method"
  , {
    object_1 <- new(
      "annotated_factor"
      , .Data = 1:4
      , label = "label1"
      , annotation = new(
        "vector_annotation"
        , label = "label1"
        , unit = "cm"
      )
      , levels = letters[1:4]
      , .S3Class = "factor"
    )
    variable_label(object_1) <- "label_1"

    expect_equal(
      object = object_1
      , expected = new(
        "annotated_factor"
        , .Data = 1:4
        , label = "label_1"
        , annotation = new(
          "vector_annotation"
          , label = "label_1"
          , unit = "cm"
        )
        , levels = letters[1:4]
        , .S3Class = "factor"
      )
    )
  }
)

test_that(
  "variable_label,data.frame-method"
  , {
    object <- data.frame(a = 1:4, b = 5:8)
    expect_error(
      variable_label(object) <- c("not_in_data" = "test")
      , "Some requested columns could not be found in data.frame."
    )
    expect_error(
      variable_label(object) <- "a"
      , "Assigned label is required to be a named character vector."
    )
  }
)



context("tex_conv borderline cases")

test_that(
  "tex_conv() borderline cases"
  , {
    object_1 <- expression(test)
    object_2 <- tex_conv(object_1)
    object_3 <- "a"
    object_4 <- tex_conv(object_3, latex2exp = FALSE)

    expect_identical(
      object = object_2
      , expected = object_1[[1]]
    )
    expect_identical(
      object = object_4
      , expected = as.expression(object_3)[[1]]
    )
  }
)

