library(xtable)

fit1 <- lm(mpg ~ cyl + disp, mtcars)
fit2 <- lm(mpg ~ am * vs, mtcars)

summary(fit1)

fit_table1 <- xtable(fit1)
fit_table2 <- xtable(fit2)

print(fit_table1, type="html", file="fit_table1.html")
print(fit_table2, type="html", file="fit_table2.html")

library(stargazer)

stargazer(fit1, type="html",
          dep.var.labels = "mpg",
          covariate.labels = c("cyl", "disp"), out="models1.html")
