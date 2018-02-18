context("variable_label() replacement methods")


test_that(
  "variable_label<-.default"
  , {
    object_1 <-1:4
    variable_label(object_1) <- "label_1"

    expect_identical(
      object = object_1
      , expected = structure(
        1:4
        , label = "label_1"
        , class = c("labelled", "integer")
      )
    )
  }
)



test_that(
  "variable_label<-.data.frame"
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
    variable_label(object) <- c("a" = "A beautiful test label.")

    expect_identical(
      object = object
      , expected = data.frame(
        a = structure(
          1:4
          , label = "A beautiful test label."
          , class = c("labelled", "integer")
        )
        , b = 5:8
      )
    )

  }
)

context("variable_label() extraction methods")

test_that(
  "variable_label.labelled-method"
  , {
    object <- 1:10
    class(object) <- c("labelled", "integer")
    attr(object, "label") <- "label1"
    variable_label(object)
    expect_identical(
      object = variable_label(object)
      , expected = "label1"
    )
  }
)

test_that(
  "droplevels.labelled-method"
  , {
    x <- factor(letters[1:4], levels = letters[1:10])
    variable_label(x) <- "Test me!"
    x <- droplevels(x, exclude = "d")

    expect_identical(
      object = x
      , expected = structure(
        factor(c(letters[1:3], NA), levels = letters[1:3])
        , label = "Test me!"
        , class = c("labelled", "factor")
      )
    )
  }
)

test_that(
  "[.labelled-method"
  , {
    x <- factor(letters[1:4], levels = letters[1:10])
    variable_label(x) <- "Test me!"
    y <- x[1:3]

    expect_identical(
      object = y
      , expected = structure(
        1:3
        , .Label = letters[1:10]
        , class = c("labelled", "factor")
        , label = "Test me!"
      )
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

