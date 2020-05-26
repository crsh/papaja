# context("apa_print() for hierarchical linear models")
#
# test_that(
#   "Fixed-effects summaries and model comparisons."
#   , {
#     testthat::skip_on_cran()
#     model_lme4 <- lme4::lmer(formula = yield ~ N + (1|block), data = npk)
#     # model2_lme4 <- lme4::lmer(formula = yield ~ N + P + (1|block), data = npk)
#     model_lmerTest <- lmerTest::as_lmerModLmerTest(model_lme4)
#
#     # glm <- lme4::glmer(formula = 1/yield ~ N + (1|block), data = npk, family = inverse.gaussian(link = "1/mu^2"))
#     apa_lme4 <- apa_print(model_lme4)
#     # apa2_lme4 <-apa_print(model2_lme4)
#     apa_lmerTest <- apa_print(model_lmerTest)
#     apa_lmerTest_in_paren <- apa_print(model_lmerTest, in_paren = TRUE)
#
#     testthat::expect_identical(
#       object = apa_lmerTest$estimate
#       , expected = list(
#         Intercept = "$b = 52.07$, 95\\% CI $[48.17$, $55.97]$"
#         , N1 = "$b = 5.62$, 95\\% CI $[1.92$, $9.31]$"
#       )
#     )
#     testthat::expect_identical(
#       object = apa_lmerTest$statistic
#       , expected = list(
#         Intercept = "$t(8.17) = 27.06$, $p < .001$"
#         , N1 = "$t(17.00) = 3.06$, $p = .007$"
#       )
#     )
#     testthat::expect_identical(
#       object = apa_lmerTest$full_result
#       , expected = list(
#         Intercept = "$b = 52.07$, 95\\% CI $[48.17$, $55.97]$, $t(8.17) = 27.06$, $p < .001$"
#         , N1 = "$b = 5.62$, 95\\% CI $[1.92$, $9.31]$, $t(17.00) = 3.06$, $p = .007$"
#       )
#     )
#
#     testthat::expect_identical(
#       object = apa_lme4$estimate
#       , expected = apa_lmerTest$estimate
#     )
#     testthat::expect_identical(
#       object = apa_lme4$statistic
#       , expected = list(
#         Intercept = "$t = 27.06$"
#         , N1 = "$t = 3.06$"
#       )
#     )
#     testthat::expect_identical(
#       object = apa_lme4$full_result
#       , expected = list(
#         Intercept = "$b = 52.07$, 95\\% CI $[48.17$, $55.97]$, $t = 27.06$"
#         , N1 = "$b = 5.62$, 95\\% CI $[1.92$, $9.31]$, $t = 3.06$"
#       )
#     )
#
#     expect_identical( # in_paren
#       , object = apa_lmerTest_in_paren$full_result$N1
#       , expected = "$b = 5.62$, 95\\% CI $[1.92$, $9.31]$, $t[17.00] = 3.06$, $p = .007$"
#     )
#
#     ranova_out <- lmerTest::ranova(model_lmerTest)
#     testthat::expect_error(
#       apa_print(ranova_out)
#       , "Single-term deletions are not supported, yet.\nVisit https://github.com/crsh/papaja/issues to request support."
#     )
#   }
# )
#
# test_that(
#   "Type-3 tables from afex::mixed"
#   , {
#     set.seed(42L)
#     captured <- capture_output({
#       mixed_KR  <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "KR")
#       mixed_S   <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "S")
#       mixed_PB  <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "PB", args_test = list(nsim = 50))
#       mixed_LRT <- afex::mixed(formula = yield ~ N * P + (1|block), data = npk, method = "LRT")
#     })
#     #
#     apa_KR  <- apa_print(mixed_KR)
#     apa_S   <- apa_print(mixed_S)
#     apa_PB  <- apa_print(mixed_PB)
#     apa_LRT <- apa_print(mixed_LRT)
#
#     expect_identical(
#       object = apa_KR$statistic
#       , expected = list(
#         N = "$F(1, 15.00) = 9.04$, $p = .009$"
#         , P = "$F(1, 15.00) = 0.40$, $p = .536$"
#         , N_P = "$F(1, 15.00) = 1.02$, $p = .329$"
#       )
#     )
#     expect_identical( # KR gives same results as S in this special case
#       object = apa_S$statistic
#       , expected = apa_KR$statistic
#     )
#     expect_identical(
#       object = apa_PB$statistic
#       , expected = list(
#         N = "$\\Chi^2 = 8.49$, $p = .039$"
#         , P = "$\\Chi^2 = 0.48$, $p = .549$"
#         , N_P = "$\\Chi^2 = 1.18$, $p = .255$"
#       )
#     )
#     expect_identical(
#       object =apa_LRT$statistic
#       , expected = list(
#         N = "$\\Chi^2(1) = 8.49$, $p = .004$"
#         , P = "$\\Chi^2(1) = 0.48$, $p = .491$"
#         , N_P = "$\\Chi^2(1) = 1.18$, $p = .277$"
#       )
#     )
#
#     expect_identical(
#       object = colnames(apa_KR$table)
#       , expected = c("Effect", "F", "df1", "df2", "p")
#     )
#     expect_identical(
#       object = colnames(apa_S$table)
#       , expected = c("Effect", "F", "df1", "df2", "p")
#     )
#     expect_identical(
#       object = colnames(apa_PB$table)
#       , expected = c("Effect", "chisq", "p")
#     )
#     expect_identical(
#       object = colnames(apa_LRT$table)
#       , expected = c("Effect", "chisq", "df", "p")
#     )
#
#   }
# )
