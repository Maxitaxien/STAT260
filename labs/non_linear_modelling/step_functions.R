library(ISLR2)
attach(Wage)

# we use the cut function to divide into several areas
print(table(cut(age, 4))) # automatically picks cutpoints
# could also specify our own using breaks
fit = lm(wage ~ cut(age, 4), data=Wage)

print(coef(summary(fit)))

agelims = range(age)
age.grid = seq(from=agelims[1], to=agelims[2])
preds = predict(fit, newdata=list(age=age.grid),se=TRUE)
plot(age, wage, cex=.5, col="darkgrey")
lines(age.grid, preds$fit, lwd=2, col="blue")