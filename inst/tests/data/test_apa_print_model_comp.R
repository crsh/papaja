mod1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
mod2 <- update(mod1, formula = . ~ . + Petal.Length)
mod3 <- update(mod2, formula = . ~ . + Petal.Width)

x <- anova(mod1, mod2, mod3)
