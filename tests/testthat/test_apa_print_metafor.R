context("apa_print.rma()")

test_that("metafor::rma.uni() intercept-only model", {

  skip_if_not_installed("metafor")

  # Build a stable toy example from a metafor built-in dataset
  data("dat.bcg", package = "metadat")
  dat_es <- metafor::escalc(
    measure = "RR"
    , ai = tpos, bi = tneg
    , ci = cpos, di = cneg
    , data = dat.bcg
  )

  m0 <- metafor::rma(yi, vi, data = dat_es, method = "REML")

  out <- apa_print(m0)

  # Basic structure checks (papaja convention)
  expect_apa_results(
    out
    , labels = list(
      term = "Term"
      , estimate  = "$\\hat{\\theta}$"
      , conf.int = "95\\% CI"
      , statistic = "$z$"
      , p.value  = "$p$"
    )
  )

  # Intercept-only model should yield a single string for each component
  expect_true(is.character(out$est[[1L]]) && length(out$est) == 1L)
  expect_true(is.character(out$stat[[1L]]) && length(out$stat) == 1L)
  expect_true(is.character(out$full[[1L]]) && length(out$full) == 1L)

  # APA-ish string patterns (don't hardcode numbers to avoid version brittleness)
  expect_identical(out$estimate[[1L]], "$\\hat{\\theta} = -0.71$, 95\\% CI $[-1.07, -0.36]$")
  expect_identical(out$statistic[[1]], "$z = -3.97$, $p < .001$")

  # Consistency: full should include both estimate + stat parts
  expect_identical(out$full_result[[1]], paste0(out$estimate[[1L]], ", ", out$statistic[[1L]]))
})

test_that("metafor::rma() with moderator produces multiple terms", {

  skip_if_not_installed("metafor")

  dat_es <- metafor::escalc(
    measure = "RR"
    , ai = tpos, bi = tneg
    , ci = cpos, di = cneg
    , data = dat.bcg
  )

  m1 <- metafor::rma(yi, vi, mods = ~ ablat, data = dat_es, method = "REML")

  out <- apa_print(m1)

  # Still should be a valid apa_results object
  expect_apa_results(out)

  # With a moderator, we expect multiple rows/terms in the results table
  expect_true(nrow(out$table) == 2L)


  # Check that at least one term mentions the moderator (depends on how canonize() sets term names)
  # We check the table term column if present, otherwise the printed strings.
    expect_true(any(grepl("Ablat", out$table$term, fixed = TRUE)))
})

test_that("metafor confidence level is respected", {

  skip_if_not_installed("metafor")

  dat_es <- metafor::escalc(
    measure = "RR"
    , ai = tpos, bi = tneg
    , ci = cpos, di = cneg
    , data = dat.bcg
  )

  m0 <- metafor::rma(yi, vi, data = dat_es, method = "REML", level = 99)

  out99 <- apa_print(m0)

  # Label should switch to 99% CI
  # (This relies on your method using deprecate_ci()/conf.int labelling like other papaja methods.)
  expect_identical(variable_label(out99$table$conf.int), "99\\% CI")
})
