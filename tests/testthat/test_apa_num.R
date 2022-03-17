context("apa_num()")

test_that(
  "Single digit"
  , {
    apa_num <- apa_num(12.1)
    expect_is(apa_num, "character")
    expect_equal(length(apa_num), 1)
    expect_null(dim(apa_num))

    expect_equal(apa_num, "12.10")

    expect_equal(apa_num(1/3, digits = 5), "0.33333")

    expect_warning(apa_num(-1.3, gt1 = FALSE))
    expect_equal(apa_num(-1/3, gt1 = TRUE), "-0.33")
    expect_equal(apa_num(-1/3, gt1 = FALSE), "-.33")

    expect_equal(apa_num(0, zero = TRUE), "0.00")
    expect_equal(apa_num(0, zero = FALSE), "< 0.01")

    expect_equal(apa_num(-0.0001), "0.00")

    expect_equal(apa_num(c(NA, "a")), c("NA", "a"))
    expect_equal(apa_num(c(NA, "b"), na_string = "-"), c("-", "b"))
  }
)

test_that(
  "Vector"
  , {
    to_print <- c(12.1, 9, 0.1, 1, -0.0003)
    expect_warning(apa_num(to_print, gt1 = FALSE))

    apa_num <- apa_num(to_print)
    expect_is(apa_num, "character")
    expect_equal(length(apa_num), 5)
    expect_null(dim(apa_num))

    expect_identical(apa_num, c("12.10", "9.00", "0.10", "1.00", "0.00"))

    expect_identical(apa_num(to_print, digits = 1), c("12.1", "9.0", "0.1", "1.0", "0.0"))
    expect_identical(apa_num(to_print, digits = c(1, 2)), c("12.1", "9.00", "0.1", "1.00", "0.0"))

    expect_identical(apa_num(to_print, gt1 = c(T, T, F, T, F)), c("12.10", "9.00", ".10", "1.00", ".00"))

    to_print[1] <- 0
    expect_identical(apa_num(to_print, zero = c(T, F, F)), c("0.00", "9.00", "0.10", "1.00", "> -0.01"))

    expect_equal(apa_num(c(1, 2, NA)), c("1.00", "2.00", "NA"))

    to_print <- c(1, 3)
    variable_label(to_print) <- "pretty label"
    apa_num <- apa_num(to_print)

    expect_identical(
      apa_num
      , expected = structure(
        c("1.00", "3.00")
        , label = "pretty label"
        , class = c("tiny_labelled", "character")
      )
    )

  }
)

test_that(
  "Matrix and data.frame"
  , {
    to_print <- matrix(c(12.1, 9, 0.1, 1, -0.0003))
    expect_warning(apa_num(to_print, gt1 = FALSE))
    # expect_error(apa_num(as.data.frame(to_print)))

    # Matrix
    m_num <- matrix(rep(c(0.0001, 123, 0.1, -12), 2), ncol = 2)
    colnames(m_num) <- paste0("Col", 1:2)
    apa_num <- apa_num(m_num)
    expect_is(apa_num, "matrix")
    expect_equal(dim(apa_num), c(4, 2))

    m_correct <- matrix(rep(c("0.00", "123.00", "0.10", "-12.00"), 2), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_identical(apa_num, m_correct)

    apa_num <- apa_num(m_num, digits = c(2, 3), margin = 2)
    m_correct <- matrix(c("0.00", "123.00", "0.10", "-12.00", "0.000", "123.000", "0.100", "-12.000"), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_identical(apa_num, m_correct)

    apa_num <- apa_num(m_num, digits = c(2, 3), margin = 1)
    m_correct <- matrix(c("0.00", "123.000", "0.10", "-12.000", "0.00", "123.000", "0.10", "-12.000"), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_identical(apa_num, m_correct)

    expect_identical(apa_num(matrix(c(1, 2, NA, 4), ncol = 2)), matrix(c("1.00", "2.00", "NA", "4.00"), ncol = 2))


    # Data.frame
    df_num <- as.data.frame(m_num, stringsAsFactors = FALSE)
    colnames(df_num) <- paste0("Col", 1:2)
    apa_num <- apa_num(df_num)
    expect_is(apa_num, "data.frame")
    expect_equal(dim(apa_num), c(4, 2))

    df_correct <- as.data.frame(matrix(rep(c("0.00", "123.00", "0.10", "-12.00"), 2), ncol = 2), stringsAsFactors = FALSE)
    colnames(df_correct) <- colnames(df_num)
    expect_equivalent(apa_num, df_correct)

    apa_num <- apa_num(df_num, digits = c(2, 3))
    df_correct <- as.data.frame(matrix(c("0.00", "123.00", "0.10", "-12.00", "0.000", "123.000", "0.100", "-12.000"), ncol = 2), stringsAsFactors = FALSE)
    colnames(df_correct) <- colnames(m_num)
    expect_equivalent(apa_num, df_correct)

    apa_num <- apa_num(df_num, digits = c(2, 3), margin = 1)
    df_correct <- as.data.frame(matrix(c("0.00", "123.000", "0.10", "-12.000", "0.00", "123.000", "0.10", "-12.000"), ncol = 2), stringsAsFactors = FALSE)
    colnames(df_correct) <- colnames(m_num)
    expect_equivalent(apa_num, df_correct)

    expect_equivalent(apa_num(as.data.frame(matrix(c(1, 2, NA, 4), ncol = 2))), as.data.frame(matrix(c("1.00", "2.00", "NA", "4.00"), ncol = 2), stringsAsFactors = FALSE))
  }
)

test_that(
  "Input validation"
  , {
    expect_error(apa_num(x = NULL), "The parameter 'x' is NULL.")

    expect_error(apa_num(1, digits = NA), "The parameter 'digits' is NA.")
    expect_error(apa_num(1, digits = "A"), "The parameter 'digits' must be of class 'numeric'.")
    expect_error(apa_num(1, digits = 1.1), "The parameter 'digits' must be an integer.")
    expect_error(apa_num(1, digits = Inf), "The parameter 'digits' must be finite.")

    expect_error(apa_num(1, gt1 = NULL), "The parameter 'gt1' is NULL.")
    expect_error(apa_num(1, gt1 = NA), "The parameter 'gt1' is NA.")
    expect_error(apa_num(1, gt1 = 1), "The parameter 'gt1' must be of class 'logical'.")
    expect_error(apa_num(1, gt1 = "A"), "The parameter 'gt1' must be of class 'logical'.")

    expect_error(apa_num(1, zero = NULL), "The parameter 'zero' is NULL.")
    expect_error(apa_num(1, zero = NA), "The parameter 'zero' is NA.")
    expect_error(apa_num(1, zero = 1), "The parameter 'zero' must be of class 'logical'.")
    expect_error(apa_num(1, zero = "A"), "The parameter 'zero' must be of class 'logical'.")

    expect_error(apa_num(1, na_string = NULL), "The parameter 'na_string' is NULL.")
    expect_error(apa_num(1, na_string = NA), "The parameter 'na_string' is NA.")
    expect_error(apa_num(1, na_string = 3), "The parameter 'na_string' must be of class 'character'.")
  }
)


context("apa_p()")

test_that(
  "Single numeric"
  , {
    expect_error(apa_p(-1/3))
    expect_error(apa_p(2))

    expect_is(apa_p(0.1), "character")
    expect_equal(length(apa_p(0.1)), 1)
    expect_null(dim(apa_p(0.1)))

    expect_equal(apa_p(0.1), ".100")
    expect_equal(apa_p(1/3), ".333")
    expect_equal(apa_p(2/3), ".667")
    expect_equal(apa_p(0.0001), "< .001")
    expect_equal(apa_p(0), "< .001")
    expect_equal(apa_p(1), "> .999")
  }
)

test_that(
  "Vector"
  , {
    pvals <- c(0.0001, 0.049, 0.1, 1, -0.3)
    expect_error(apa_p(pvals))

    pvals <- apa_p(pvals[-5])
    expect_is(pvals, "character")
    expect_equal(length(pvals), 4)
    expect_null(dim(pvals))

    expect_identical(pvals, c("< .001", ".049", ".100", "> .999"))
  }
)

test_that(
  "Matrix and data.frame"
  , {
    pvals <- matrix(c(0.0001, 0.049, 0.1, 1, -0.3))
    expect_error(apa_p(pvals))
    expect_error(apa_p(as.data.frame(pvals)))
  }
)

test_that(
  "Integers"
  , {
    expect_equal(apa_num(0L, numerals = FALSE), "no")
    expect_equal(apa_num(1L, numerals = FALSE), "one")
    expect_equal(apa_num(10L, numerals = FALSE), "ten")
    expect_equal(apa_num(13L, numerals = FALSE), "thirteen")
    expect_equal(apa_num(50L, numerals = FALSE), "fifty")
    expect_equal(apa_num(20L, numerals = FALSE), "twenty")
    expect_equal(apa_num(23L, numerals = FALSE), "twenty-three")
    expect_equal(apa_num(100L, numerals = FALSE), "one hundred")
    expect_equal(apa_num(139L, numerals = FALSE), "one hundred and thirty-nine")
    expect_equal(apa_num(1001L, numerals = FALSE), "one thousand and one")
    expect_equal(apa_num(2020L, numerals = FALSE), "two thousand and twenty")
    expect_equal(apa_num(3871L, numerals = FALSE), "three thousand, eight hundred and seventy-one")
    expect_equal(apa_num(c(10000000L, 3L), numerals = FALSE), c("ten million", "three"))

    expect_equal(apa_num(100L, numerals = FALSE, capitalize = TRUE), "One hundred")
  }
)

test_that(
  "Lists"
  , {
    expect_equal(apa_num(list(1.323, 1)), list("1.32", "1.00"))
  }
)

test_that(
  "Input validation"
  , {
    expect_error(apa_p(NULL), "The parameter 'x' is NULL.")
    expect_error(apa_p("A"), "The parameter 'x' must be of class 'numeric'.")
    expect_error(apa_p(1.01), "The parameter 'x' must be between 0 and 1.")
    expect_error(apa_p(-0.01), "The parameter 'x' must be between 0 and 1.")

    expect_error(apa_p(1, na_string = NULL), "The parameter 'na_string' is NULL.")
    # expect_error(apa_p(1, na_string = NA), "The parameter 'na_string' is NA.")
    expect_error(apa_p(1, na_string = 3), "The parameter 'na_string' must be of class 'character'.")
    expect_error(apa_df(npk$yield, digits = c(3, 2)), "The parameter `digits` must be of length 1 or equal to length of `x`.")
  }
)

test_that(
  "Scientific typesetting"
  , {
    scientific_style <- apa_num(0.00125, format = "e", digits = 2)
    nonscientific_style <- apa_num(0.00125, format = "f", digits = 2)

    expect_identical(
      scientific_style
      , "$1.25 \\times 10^{-3}$"
    )
    expect_identical(
      nonscientific_style
      , "0.00"
    )
  }
)

test_that(
  "NA handling"
  , {

    expect_identical(
      apa_num(c(NA, 1), gt1 = FALSE)
      , c("NA", "> .99")
    )
    expect_warning(
      apa_num(c(NA, 1.5), gt1 = FALSE)
      , regexp = "You specified gt1 = FALSE, but passed absolute value(s) that exceed 1."
      , fixed = TRUE
    )
  }
)
