data(iris)
head(iris)

fit1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

summary(fit1)
