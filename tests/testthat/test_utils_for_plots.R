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
