library(ISLR2)
library(leaps) # lib for best subset selection
cols = names(Hitters)

print(dim(Hitters))
print(sum(is.na(Hitters))) # 59 missing datapoints
# Drop them!

Hitters = na.omit(Hitters)

# Fit subset selection models:
regfit.full = regsubsets(Salary ~., Hitters)
#print(summary(regfit.full)) # shows model sizes and for each size whether a feat is include by "*"

# We can choose to have more than 8 features by modifying the default argument
regfit.full = regsubsets(Salary ~ ., data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)

print(names(reg.summary)) # shows name of statistics returned

print(reg.summary$rsq) # R^2 increases monotonically as more variables are included - property of R^2
# So this is not necessarily a good selection criteria.
# But adjusted R^2 is usable

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

max_idx = which.max(reg.summary$adjr2)
# Points puts points on the already created plot
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
points(max_idx, reg.summary$adjr2[max_idx], col="red", cex=2, pch=20)

plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
min_idx = which.min(reg.summary$cp)
points(min_idx, reg.summary$cp[min_idx], col="red", cex=2, pch=20)
min_idx = which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of variables", ylab="BIC", type="l")
points(min_idx, reg.summary$bic[min_idx], col="red", cex=2, pch=20)


# The regsubsets function also has a built in plot:
# lot(regfit.full, scale = "adjr2") for instance

# Let's just choose the model with the lowest BIC: number 6:
print(coef(regfit.full, min_idx)) # that is, 6 variables used

