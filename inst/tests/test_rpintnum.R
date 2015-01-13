library("testthat")
source("../../R/printnum.R")



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

    m_pvals <- matrix(rep(c(0.0001, 0.049, 0.1, 1), 2), ncol = 2)
    pvals <- printp(m_pvals)
    expect_that(pvals, is_a("matrix"))
    expect_that(dim(pvals), equals(c(4, 2)))
    expect_that(pvals, is_identical_to(matrix(rep(c("< .001", ".049", ".100", "> .999"), 2), ncol = 2)))

    df_pvals <- as.data.frame(m_pvals)
    pvals <- printp(df_pvals)
    expect_that(pvals, is_a("data.frame"))
    expect_that(dim(pvals), equals(c(4, 2)))
    expect_that(pvals, is_equivalent_to(as.data.frame(matrix(rep(c("< .001", ".049", ".100", "> .999"), 2), ncol = 2))))
  }
)
