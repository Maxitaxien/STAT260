library(ISLR2)
library(boot)

set.seed(1)

# print(head(Default))
# print(summary(Default))

# a)
log.fit = glm(default ~ income + balance, data=Default, family="binomial")
s = summary(log.fit)
print(s$coefficients[, "Std. Error"])
# Summary yields the following standard errors for the estimates:
# (Intercept)       income      balance
# 4.347564e-01 4.985167e-06 2.273731e-04

# b)
boot.fn = function(Default, index) {
    log.fit = glm(default ~ income + balance, data=Default, subset=index, family="binomial")
    return(summary(log.fit)$coefficients[c("income", "balance"), "Estimate"])
}

# c)
boot_model = boot(Default, statistic=boot.fn, R=100)
income_estimate = sd(boot_model$t[, 1])
balance_estimate = sd(boot_model$t[, 2])
print(income_estimate)
print(balance_estimate)
# Income std-error estimate: 4.186088e-06
# Balance std-error estimate: 0.0002226242 = 2.2226242e-04