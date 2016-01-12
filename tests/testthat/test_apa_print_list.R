context("apa_print() for lists of objects")

# Model comparison

test_that(
  "lm model comparison"
  , {
    baseline <- lm(formula = Fertility ~ ., data = swiss)
    model_1 <- update(baseline, formula = . ~ . - Examination)
    model_2 <- update(model_1, formula = . ~ . - Agriculture)
    model_comp <- anova(baseline, model_1, model_2)

    model_comp_output <- apa_print(model_comp, models = list(baseline, model_1, model_2), boot_samples = 1000)

    expect_that(model_comp_output, is_a("list"))
    expect_that(length(model_comp_output), equals(4))
    expect_that(model_comp_output$stat$model2, equals("$F(1, 41) = 1.03$, $p = .315$"))

    #     drop1(baseline, test = "F")
    #     drop1(baseline, test = "Chisq")
  }
)
