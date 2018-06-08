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
        , class = c("papaja_labelled", "integer")
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
          , class = c("papaja_labelled", "integer")
        )
        , b = 5:8
      )
    )

  }
)

context("variable_label() extraction methods")

test_that(
  "variable_label.papaja_labelled-method"
  , {
    object <- 1:10
    class(object) <- c("papaja_labelled", "integer")
    attr(object, "label") <- "label1"

    expect_identical(
      object = variable_label(object)
      , expected = "label1"
    )
  }
)

context("methods for class .papaja_labelled")

test_that(
  "droplevels.papaja_labelled-method"
  , {
    # only check consistency, as base behavior has changed recently
    x <- factor(letters[1:4], levels = letters[1:10])
    variable_label(x) <- "Test me!"
    x <- droplevels(x, exclude = "d")

    y <- factor(letters[1:4], levels = letters[1:10])
    y <- droplevels(y, exclude = "d")
    variable_label(y) <- "Test me!"

    expect_identical(
      object = x
      , expected = y
    )
  }
)

test_that(
  "[.papaja_labelled-method, [[.papaja_labelled-method"
  , {
    x <- factor(letters[1:4], levels = letters[1:10])
    variable_label(x) <- "Test me!"
    y <- x[1:3]
    z <- x[[2]]

    expect_identical(
      object = y
      , expected = structure(
        1:3
        , .Label = letters[1:10]
        , class = c("papaja_labelled", "factor")
        , label = "Test me!"
      )
    )

    expect_identical(
      object = z
      , expected = structure(
        2L
        , .Label = letters[1:10]
        , class = c("papaja_labelled", "factor")
        , label = "Test me!"
      )
    )

    expect_identical(variable_label(y), "Test me!")
    expect_identical(class(y), c("papaja_labelled", "factor"))
    expect_identical(levels(y), letters[1:10])
  }
)



test_that(
  "rep.papaja_labelled-method"
  , {
    o1 <- 1:3
    variable_label(o1) <- "Test me!"
    o1 <- rep(o1, 2)
    o2 <- rep(1:3, 2)
    variable_label(o2) <- "Test me!"

    expect_identical(
      object = o1
      , expected = o2
    )
  }
)

test_that(
  "relevel.papaja_labelled-method"
  , {
    o1 <- factor(letters[1:3], levels = letters[1:3])
    variable_label(o1) <- "Test me!"
    o1 <- relevel(o1, "b")

    expect_identical(
      object = o1
      , expected = structure(c(2L, 1L, 3L), .Label = c("b", "a", "c"), class = c("papaja_labelled", "factor"), label = "Test me!")
    )
    # Check all attributes separately, as sometimes expect_identical seems to be sloppy
    expect_identical(variable_label(o1), expected = "Test me!")
    expect_identical(levels(o1), expected = c("b", "a", "c"))
    expect_identical(class(o1), expected = c("papaja_labelled", "factor"))
  }
)




