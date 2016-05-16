context("printnum()")

test_that(
  "Single digit"
  , {
    apa_num <- printnum(12.1)
    expect_is(apa_num, "character")
    expect_equal(length(apa_num), 1)
    expect_null(dim(apa_num))

    expect_equal(apa_num, "12.10")

    expect_equal(printnum(1/3, digits = 5), "0.33333")

    expect_warning(printnum(-1.3, gt1 = FALSE))
    expect_equal(printnum(-1/3, gt1 = TRUE), "-0.33")
    expect_equal(printnum(-1/3, gt1 = FALSE), "-.33")

    expect_equal(printnum(0, zero = TRUE), "0.00")
    expect_equal(printnum(0, zero = FALSE), "< .01")

    expect_equal(printnum(-0.0001), "0.00")

    expect_equal(printnum(NA), "")
    expect_equal(printnum(NA, na_string = "-"), "-")
  }
)

test_that(
  "Vector"
  , {
    to_print <- c(12.1, 9, 0.1, 1, -0.0003)
    expect_warning(printnum(to_print, gt1 = FALSE))

    apa_num <- printnum(to_print)
    expect_is(apa_num, "character")
    expect_equal(length(apa_num), 5)
    expect_null(dim(apa_num))

    expect_identical(apa_num, c("12.10", "9.00", "0.10", "1.00", "0.00"))

    expect_identical(printnum(to_print, digits = 1), c("12.1", "9.0", "0.1", "1.0", "0.0"))
    expect_identical(printnum(to_print, digits = c(1, 2)), c("12.1", "9.00", "0.1", "1.00", "0.0"))

    expect_identical(printnum(to_print, gt1 = c(T, T, F, T, F)), c("12.10", "9.00", ".10", "1.00", ".00"))

    to_print[1] <- 0
    expect_identical(printnum(to_print, zero = c(T, F, F)), c("0.00", "9.00", "0.10", "1.00", "< .01"))

    expect_equal(printnum(c(1, 2, NA)), c("1.00", "2.00", ""))
  }
)

test_that(
  "Matrix and data.frame"
  , {
    to_print <- matrix(c(12.1, 9, 0.1, 1, -0.0003))
    expect_warning(printnum(to_print, gt1 = FALSE))
    # expect_error(printnum(as.data.frame(to_print)))

    # Matrix
    m_num <- matrix(rep(c(0.0001, 123, 0.1, -12), 2), ncol = 2)
    colnames(m_num) <- paste0("Col", 1:2)
    apa_num <- printnum(m_num)
    expect_is(apa_num, "matrix")
    expect_equal(dim(apa_num), c(4, 2))

    m_correct <- matrix(rep(c("0.00", "123.00", "0.10", "-12.00"), 2), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_identical(apa_num, m_correct)

    apa_num <- printnum(m_num, digits = c(2, 3), margin = 2)
    m_correct <- matrix(c("0.00", "123.00", "0.10", "-12.00", "0.000", "123.000", "0.100", "-12.000"), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_identical(apa_num, m_correct)

    apa_num <- printnum(m_num, digits = c(2, 3), margin = 1)
    m_correct <- matrix(c("0.00", "123.000", "0.10", "-12.000", "0.00", "123.000", "0.10", "-12.000"), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_identical(apa_num, m_correct)

    expect_identical(printnum(matrix(c(1, 2, NA, 4), ncol = 2)), matrix(c("1.00", "2.00", "", "4.00"), ncol = 2))


    # Data.frame
    df_num <- as.data.frame(m_num)
    colnames(df_num) <- paste0("Col", 1:2)
    apa_num <- printnum(df_num)
    expect_is(apa_num, "data.frame")
    expect_equal(dim(apa_num), c(4, 2))

    df_correct <- as.data.frame(matrix(rep(c("0.00", "123.00", "0.10", "-12.00"), 2), ncol = 2))
    colnames(df_correct) <- colnames(df_num)
    expect_equivalent(apa_num, df_correct)

    apa_num <- printnum(df_num, digits = c(2, 3), margin = 2)
    df_correct <- as.data.frame(matrix(c("0.00", "123.00", "0.10", "-12.00", "0.000", "123.000", "0.100", "-12.000"), ncol = 2))
    colnames(df_correct) <- colnames(m_num)
    expect_equivalent(apa_num, df_correct)

    apa_num <- printnum(df_num, digits = c(2, 3), margin = 1)
    df_correct <- as.data.frame(matrix(c("0.00", "123.000", "0.10", "-12.000", "0.00", "123.000", "0.10", "-12.000"), ncol = 2))
    colnames(df_correct) <- colnames(m_num)
    expect_equivalent(apa_num, df_correct)

    expect_equivalent(printnum(as.data.frame(matrix(c(1, 2, NA, 4), ncol = 2))), as.data.frame(matrix(c("1.00", "2.00", "", "4.00"), ncol = 2)))
  }
)

test_that(
  "Input validation"
  , {
    expect_error(printnum(x = NULL), "The parameter 'x' is NULL.")

    expect_error(printnum(1, digits = NA), "The parameter 'digits' is NA.")
    expect_error(printnum(1, digits = "A"), "The parameter 'digits' must be of class 'numeric'.")
    expect_error(printnum(1, digits = 1.1), "The parameter 'digits' must be an integer.")
    expect_error(printnum(1, digits = 1:2), " The parameter 'digits' must be of length 1.")
    expect_error(printnum(1, digits = Inf), "The parameter 'digits' must be finite.")

    expect_error(printnum(1, gt1 = NULL), "The parameter 'gt1' is NULL.")
    expect_error(printnum(1, gt1 = NA), "The parameter 'gt1' is NA.")
    expect_error(printnum(1, gt1 = 1), "The parameter 'gt1' must be of class 'logical'.")
    expect_error(printnum(1, gt1 = "A"), "The parameter 'gt1' must be of class 'logical'.")
    expect_error(printnum(1, gt1 = c(TRUE, FALSE)), "The parameter 'gt1' must be of length 1.")

    expect_error(printnum(1, zero = NULL), "The parameter 'zero' is NULL.")
    expect_error(printnum(1, zero = NA), "The parameter 'zero' is NA.")
    expect_error(printnum(1, zero = 1), "The parameter 'zero' must be of class 'logical'.")
    expect_error(printnum(1, zero = "A"), "The parameter 'zero' must be of class 'logical'.")
    expect_error(printnum(1, zero = c(TRUE, FALSE)), "The parameter 'zero' must be of length 1.")

    expect_error(printnum(1, margin = NULL), "The parameter 'margin' is NULL.")
    expect_error(printnum(1, margin = NA), "The parameter 'margin' is NA.")
    expect_error(printnum(1, margin = "A"), "The parameter 'margin' must be of class 'numeric'.")
    expect_error(printnum(1, margin = 1.1), "The parameter 'margin' must be an integer.")
    expect_error(printnum(1, margin = 1:2), "The parameter 'margin' must be of length 1.")
    expect_error(printnum(1, margin = 0), "The parameter 'margin' must be between 1 and 2.")
    expect_error(printnum(1, margin = 3), "The parameter 'margin' must be between 1 and 2.")

    expect_error(printnum(1, na_string = NULL), "The parameter 'na_string' is NULL.")
    expect_error(printnum(1, na_string = NA), "The parameter 'na_string' is NA.")
    expect_error(printnum(1, na_string = letters[1:2]), "The parameter 'na_string' must be of length 1.")
    expect_error(printnum(1, na_string = 3), "The parameter 'na_string' must be of class 'character'.")
  }
)


context("printnumber()")

test_that(
  "Input validation"
  , {
    expect_equivalent(printnumber(NA), "")
    expect_equivalent(printnumber(NA, na_string = "foo"), "foo")

    expect_error(printnumber(1, digits = NA), "The parameter 'digits' is NA.")
    expect_error(printnumber(1, digits = "A"), "The parameter 'digits' must be of class 'numeric'.")
    expect_error(printnumber(1, digits = 1.1), "The parameter 'digits' must be an integer.")
    expect_error(printnumber(1, digits = 1:2), "The parameter 'digits' must be of length 1.")
    expect_error(printnumber(1, digits = Inf), "The parameter 'digits' must be finite.")

    expect_error(printnumber(1, gt1 = NULL), "The parameter 'gt1' is NULL.")
    expect_error(printnumber(1, gt1 = NA), "The parameter 'gt1' is NA.")
    expect_error(printnumber(1, gt1 = 1), "The parameter 'gt1' must be of class 'logical'.")
    expect_error(printnumber(1, gt1 = "A"), "The parameter 'gt1' must be of class 'logical'.")
    expect_error(printnumber(1, gt1 = c(TRUE, FALSE)), "The parameter 'gt1' must be of length 1.")

    expect_error(printnumber(1, zero = NULL), "The parameter 'zero' is NULL.")
    expect_error(printnumber(1, zero = NA), "The parameter 'zero' is NA.")
    expect_error(printnumber(1, zero = 1), "The parameter 'zero' must be of class 'logical'.")
    expect_error(printnumber(1, zero = "A"), "The parameter 'zero' must be of class 'logical'.")
    expect_error(printnumber(1, zero = c(TRUE, FALSE)), "The parameter 'zero' must be of length 1.")

    expect_error(printnumber(1, na_string = NULL), "The parameter 'na_string' is NULL.")
    expect_error(printnumber(1, na_string = NA), "The parameter 'na_string' is NA.")
    expect_error(printnumber(1, na_string = letters[1:2]), "The parameter 'na_string' must be of length 1.")
    expect_error(printnumber(1, na_string = 3), "The parameter 'na_string' must be of class 'character'.")

    expect_warning(printnumber(2, gt1 = FALSE))
  }
)


context("printp()")

test_that(
  "Single numeric"
  , {
    expect_error(printp(-1/3))
    expect_error(printp(2))

    expect_is(printp(0.1), "character")
    expect_equal(length(printp(0.1)), 1)
    expect_null(dim(printp(0.1)))

    expect_equal(printp(0.1), ".100")
    expect_equal(printp(1/3), ".333")
    expect_equal(printp(2/3), ".667")
    expect_equal(printp(0.0001), "< .001")
    expect_equal(printp(0), "< .001")
    expect_equal(printp(1), "> .999")
  }
)

test_that(
  "Vector"
  , {
    pvals <- c(0.0001, 0.049, 0.1, 1, -0.3)
    expect_error(printp(pvals))

    pvals <- printp(pvals[-5])
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
    expect_error(printp(pvals))
    expect_error(printp(as.data.frame(pvals)))
  }
)

test_that(
  "Input validation"
  , {
    expect_error(printp(NULL), "The parameter 'x' is NULL.")
    expect_error(printp(NA), "The parameter 'x' is NA.")
    expect_error(printp("A"), "The parameter 'x' must be of class 'numeric'.")
    expect_error(printp(1.01), "The parameter 'x' must be between 0 and 1.")
    expect_error(printp(-0.01), "The parameter 'x' must be between 0 and 1.")

    expect_error(printp(1, na_string = NULL), "The parameter 'na_string' is NULL.")
    expect_error(printp(1, na_string = NA), "The parameter 'na_string' is NA.")
    expect_error(printp(1, na_string = letters[1:2]), "The parameter 'na_string' must be of length 1.")
    expect_error(printp(1, na_string = 3), "The parameter 'na_string' must be of class 'character'.")
  }
)
