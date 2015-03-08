library("testthat")
source("../../R/apa_print.R")
source("../../R/printnum.R")
source("../../R/utils.R")


context("apa_print() for ANOVA")

datafilename <- "http://personality-project.org/r/datasets/R.appendix1.data"
ow_data <- read.table(datafilename, header = TRUE)

ow_aov <- aov(Alertness ~ Dosage, data = ow_data)
ow_aov_summary <- summary(ow_anova)


test_that(
  "Levene test"
  , {
    levene_test <- car::leveneTest(conformity ~ fcategory * partner.status, data = car::Moore)

    levene_test_output <- apa_stat(levene_test)
    expect_that(levene_test_output, is_a("character"))
    expect_that(levene_test_output, equals(""))
  }
)
