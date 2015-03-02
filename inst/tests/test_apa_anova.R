library("testthat")
source("../../R/apa_print.R")
source("../../R/printnum.R")
source("../../R/utils.R")



test_that(
  "Levene test"
  , {
    levene_test <- car::leveneTest(conformity ~ fcategory * partner.status, data = car::Moore)

    levene_test_output <- apa_stat(levene_test)
    expect_that(levene_test_output, is_a("character"))
    expect_that(levene_test_output, equals(""))
  }
)
