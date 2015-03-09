context("printnum()")

test_that(
  "Single digit"
  , {
    apa_num <- printnum(12.1)
    expect_that(apa_num, is_a("character"))
    expect_that(length(apa_num), equals(1))
    expect_that(dim(apa_num), is_null())

    expect_that(apa_num, equals("12.10"))

    expect_that(printnum(1/3, digits = 5), equals("0.33333"))

    expect_that(printnum(-1.3, gt1 = FALSE), gives_warning())
    expect_that(printnum(-1/3, gt1 = TRUE), equals("-0.33"))
    expect_that(printnum(-1/3, gt1 = FALSE), equals("-.33"))

    expect_that(printnum(0, zero = TRUE), equals("0.00"))
    expect_that(printnum(0, zero = FALSE), equals("< .01"))

    expect_that(printnum(-0.0001), equals("0.00"))

    expect_that(printnum(NA), equals(""))
    expect_that(printnum(NA, na_string = "-"), equals("-"))
  }
)

test_that(
  "Vector"
  , {
    to_print <- c(12.1, 9, 0.1, 1, -0.0003)
    expect_that(printnum(to_print, gt1 = FALSE), gives_warning())

    apa_num <- printnum(to_print)
    expect_that(apa_num, is_a("character"))
    expect_that(length(apa_num), equals(5))
    expect_that(dim(apa_num), is_null())

    expect_that(apa_num, is_identical_to(c("12.10", "9.00", "0.10", "1.00", "0.00")))

    expect_that(printnum(to_print, digits = 1), is_identical_to(c("12.1", "9.0", "0.1", "1.0", "0.0")))
    expect_that(printnum(to_print, digits = c(1, 2)), is_identical_to(c("12.1", "9.00", "0.1", "1.00", "0.0")))

    expect_that(printnum(to_print, gt1 = c(T, T, F, T, F)), is_identical_to(c("12.10", "9.00", ".10", "1.00", ".00")))

    to_print[1] <- 0
    expect_that(printnum(to_print, zero = c(T, F, F)), is_identical_to(c("0.00", "9.00", "0.10", "1.00", "< .01")))

    expect_that(printnum(c(1, 2, NA)), equals(c("1.00", "2.00", "")))
  }
)

test_that(
  "Matrix and data.frame"
  , {
    to_print <- matrix(c(12.1, 9, 0.1, 1, -0.0003))
    expect_that(printnum(to_print, gt1 = FALSE), gives_warning())
    expect_that(printnum(as.data.frame(to_print)), throws_error())

    # Matrix
    m_num <- matrix(rep(c(0.0001, 123, 0.1, -12), 2), ncol = 2)
    colnames(m_num) <- paste0("Col", 1:2)
    apa_num <- printnum(m_num)
    expect_that(apa_num, is_a("matrix"))
    expect_that(dim(apa_num), equals(c(4, 2)))

    m_correct <- matrix(rep(c("0.00", "123.00", "0.10", "-12.00"), 2), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_that(apa_num, is_identical_to(m_correct))

    apa_num <- printnum(m_num, digits = c(2, 3), margin = 2)
    m_correct <- matrix(c("0.00", "123.00", "0.10", "-12.00", "0.000", "123.000", "0.100", "-12.000"), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_that(apa_num, is_identical_to(m_correct))

    apa_num <- printnum(m_num, digits = c(2, 3), margin = 1)
    m_correct <- matrix(c("0.00", "123.000", "0.10", "-12.000", "0.00", "123.000", "0.10", "-12.000"), ncol = 2)
    colnames(m_correct) <- colnames(m_num)
    expect_that(apa_num, is_identical_to(m_correct))

    expect_that(printnum(matrix(c(1, 2, NA, 4), ncol = 2)), is_identical_to(matrix(c("1.00", "2.00", "", "4.00"), ncol = 2)))


    # Data.frame
    df_num <- as.data.frame(m_num)
    colnames(df_num) <- paste0("Col", 1:2)
    apa_num <- printnum(df_num)
    expect_that(apa_num, is_a("data.frame"))
    expect_that(dim(apa_num), equals(c(4, 2)))

    df_correct <- as.data.frame(matrix(rep(c("0.00", "123.00", "0.10", "-12.00"), 2), ncol = 2))
    colnames(df_correct) <- colnames(df_num)
    expect_that(apa_num, is_equivalent_to(df_correct))

    apa_num <- printnum(df_num, digits = c(2, 3), margin = 2)
    df_correct <- as.data.frame(matrix(c("0.00", "123.00", "0.10", "-12.00", "0.000", "123.000", "0.100", "-12.000"), ncol = 2))
    colnames(df_correct) <- colnames(m_num)
    expect_that(apa_num, is_equivalent_to(df_correct))

    apa_num <- printnum(df_num, digits = c(2, 3), margin = 1)
    df_correct <- as.data.frame(matrix(c("0.00", "123.000", "0.10", "-12.000", "0.00", "123.000", "0.10", "-12.000"), ncol = 2))
    colnames(df_correct) <- colnames(m_num)
    expect_that(apa_num, is_equivalent_to(df_correct))

    expect_that(printnum(as.data.frame(matrix(c(1, 2, NA, 4), ncol = 2))), is_equivalent_to(as.data.frame(matrix(c("1.00", "2.00", "", "4.00"), ncol = 2))))
  }
)

test_that(
  "Input validation"
  , {
    expect_error(printnum(x = NULL), "The parameter 'x' is NULL.")

    expect_error(printnum(1, digits = NULL), "The parameter 'digits' is NULL.")
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
    expect_that(printnumber(NA), is_equivalent_to(""))
    expect_that(printnumber(NA, na_string = "foo"), is_equivalent_to("foo"))

    expect_error(printnumber(1, digits = NULL), "The parameter 'digits' is NULL.")
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

    expect_that(printnumber(2, gt1 = FALSE), gives_warning())
  }
)


context("printp()")

test_that(
  "Single numeric"
  , {
    expect_that(printp(-1/3), throws_error())
    expect_that(printp(2), throws_error())

    expect_that(printp(0.1), is_a("character"))
    expect_that(length(printp(0.1)), equals(1))
    expect_that(dim(printp(0.1)), is_null())

    expect_that(printp(0.1), equals(".100"))
    expect_that(printp(1/3), equals(".333"))
    expect_that(printp(2/3), equals(".667"))
    expect_that(printp(0.0001), equals("< .001"))
    expect_that(printp(0), equals("< .001"))
    expect_that(printp(1), equals("> .999"))
  }
)

test_that(
  "Vector"
  , {
    pvals <- c(0.0001, 0.049, 0.1, 1, -0.3)
    expect_that(printp(pvals), throws_error())

    pvals <- printp(pvals[-5])
    expect_that(pvals, is_a("character"))
    expect_that(length(pvals), equals(4))
    expect_that(dim(pvals), is_null())

    expect_that(pvals, is_identical_to(c("< .001", ".049", ".100", "> .999")))
  }
)

test_that(
  "Matrix and data.frame"
  , {
    pvals <- matrix(c(0.0001, 0.049, 0.1, 1, -0.3))
    expect_that(printp(pvals), throws_error())
    expect_that(printp(as.data.frame(pvals)), throws_error())
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

    expect_error(printp(1, na_string=NULL), "The parameter 'na_string' is NULL.")
    expect_error(printp(1, na_string=NA), "The parameter 'na_string' is NA.")
    expect_error(printp(1, na_string=letters[1:2]), "The parameter 'na_string' must be of length 1.")
    expect_error(printp(1, na_string=3), "The parameter 'na_string' must be of class 'character'.")
  }
)
