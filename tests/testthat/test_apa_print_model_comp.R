context("apa_print.list()")

test_that(
  "Linear regression"
  , {

    mod1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
    mod2 <- update(mod1, formula = . ~ . + Petal.Length)
    mod3 <- update(mod2, formula = . ~ . + Petal.Width)

    # No bootstrapped Delta R^2 CI
    model_comp <- apa_print(list(Baseline = mod1, Length = mod2, Both = mod3), boot_samples = 0)

    expect_is(model_comp, "list")
    expect_equal(length(model_comp), 4)
    expect_equal(names(model_comp), c("stat", "est", "full", "table"))

    # stat
    expect_is(model_comp$stat, "list")
    expect_equal(length(model_comp$stat), 2)
    expect_equal(names(model_comp$stat), c("Length", "Both"))
    expect_is(model_comp$stat$Length, "character")
    expect_is(model_comp$stat$Both, "character")

    expect_equal(model_comp$stat$Length, "$F(1, 148) = 853.31$, $p < .001$")
    expect_equal(model_comp$stat$Both, "$F(1, 148) = 19.04$, $p < .001$")

    # est
    expect_is(model_comp$est, "list")
    expect_equal(length(model_comp$est), 2)
    expect_equal(names(model_comp$est), c("Length", "Both"))
    expect_is(model_comp$est$Length, "character")
    expect_is(model_comp$est$Both, "character")

    expect_equal(model_comp$est$Length, "$\\Delta R^2 = .83$")
    expect_equal(model_comp$est$Both, "$\\Delta R^2 = .02$")

    # full
    expect_is(model_comp$full, "list")
    expect_equal(length(model_comp$full), 2)
    expect_equal(names(model_comp$full), c("Length", "Both"))
    expect_is(model_comp$full$Length, "character")
    expect_is(model_comp$full$Both, "character")

    expect_equal(model_comp$full$Length, paste(model_comp$est$Length, model_comp$stat$Length, sep = ", "))
    expect_equal(model_comp$full$Both, paste(model_comp$est$Both, model_comp$stat$Both, sep = ", "))

    # table
    correct_table <- structure(
      list(Baseline = structure(c("$6.53$ $[5.58$, $7.47]$", "$-0.22$ $[-0.53$, $0.08]$", "", "", "$.01$ $[0.00$, $0.06]$", "2.07", "2", "148", ".152", "371.99", "381.02", "", "", "", "", "", "", ""), .Names = c(NA, NA, NA, NA, "$R^2$ [90\\% CI]", "$F$", "$df_1$", "$df_2$", "$p$", "$AIC$", "$BIC$", "$\\Delta R^2$", "$F$ ", "$df_1$ ", "$df_2$ ", "$p$ ", "$\\Delta AIC$", "$\\Delta BIC$")), Length = structure(c("$2.25$ $[1.76$, $2.74]$", "$0.60$ $[0.46$, $0.73]$", "$0.47$ $[0.44$, $0.51]$", "", "$.84$ $[0.80$, $0.86]$", "386.39", "3", "147", "< .001", "101.03", "113.07", "$.83$", "853.31", "1", "148", "< .001", "-270.97", "-267.96"), .Names = c(NA, NA, NA, NA, "$R^2$ [90\\% CI]", "$F$", "$df_1$", "$df_2$", "$p$", "$AIC$", "$BIC$", "$\\Delta R^2$", "$F$ ", "$df_1$ ", "$df_2$ ", "$p$ ", "$\\Delta AIC$", "$\\Delta BIC$")), Both = structure(c("$1.86$ $[1.36$, $2.35]$", "$0.65$ $[0.52$, $0.78]$", "$0.71$ $[0.60$, $0.82]$", "$-0.56$ $[-0.81$, $-0.30]$", "$.86$ $[0.82$, $0.88]$", "295.54", "4", "146", "< .001", "84.64", "99.70", "$.02$", "19.04", "1", "148", "< .001", "-16.38", "-13.37"), .Names = c(NA, NA, NA, NA, "$R^2$ [90\\% CI]", "$F$", "$df_1$", "$df_2$", "$p$", "$AIC$", "$BIC$", "$\\Delta R^2$", "$F$ ", "$df_1$ ", "$df_2$ ", "$p$ ", "$\\Delta AIC$", "$\\Delta BIC$"))), .Names = c("Baseline", "Length", "Both"), row.names = c("Intercept", "Sepal_Width", "Petal_Length", "Petal_Width", "$R^2$ [90\\% CI]", "$F$", "$df_1$", "$df_2$", "$p$", "$AIC$", "$BIC$", "$\\Delta R^2$", "$F$ ", "$df_1$ ", "$df_2$ ", "$p$ ", "$\\Delta AIC$", "$\\Delta BIC$"), class = "data.frame")

    expect_is(model_comp$table, "data.frame")
    expect_equal(colnames(model_comp$table), c("Baseline", "Length", "Both"))
    expect_identical(model_comp$table, correct_table)


    # Test omission of incomplete model names & CI
    incomplete_names <- apa_print(list(mod1, Length = mod2, Both = mod3), boot_samples = 0)

    ## stat
    expect_is(incomplete_names$stat, "list")
    expect_equal(length(incomplete_names$stat), 2)
    expect_equal(names(incomplete_names$stat), c("model2", "model3"))

    ## est
    expect_is(incomplete_names$est, "list")
    expect_equal(length(incomplete_names$est), 2)
    expect_equal(names(incomplete_names$est), c("model2", "model3"))

    ## full
    expect_is(incomplete_names$full, "list")
    expect_equal(length(incomplete_names$full), 2)
    expect_equal(names(incomplete_names$full), c("model2", "model3"))

    ## table
    expect_is(incomplete_names$table, "data.frame")
    expect_equal(colnames(incomplete_names$table), paste("Model", 1:3))

    incomplete_names2 <- apa_print(list(mod1, mod2, mod3), boot_samples = 0)
    expect_identical(incomplete_names, incomplete_names2)


    # Bootstrapped Delta R^2 CI
    skip_on_cran() # The bootstrapping is computationally too expensive
    model_comp_boot <- apa_print(list(Baseline = mod1, Length = mod2, Both = mod3), boot_samples = 10000)

    expect_equal(model_comp_boot$est$Length, "$\\Delta R^2 = .83$, 90\\% CI $[.75$, $.87]$")
    expect_equal(model_comp_boot$est$Both, "$\\Delta R^2 = .02$, 90\\% CI $[.00$, $.04]$")

    model_comp_boot2 <- apa_print(list(Baseline = mod1, Length = mod2, Both = mod3), boot_samples = 10000, ci = 0.5)

    expect_equal(model_comp_boot2$est$Length, "$\\Delta R^2 = .83$, 50\\% CI $[.80$, $.84]$")
    expect_equal(model_comp_boot2$est$Both, "$\\Delta R^2 = .02$, 50\\% CI $[.01$, $.03]$")
  }
)
