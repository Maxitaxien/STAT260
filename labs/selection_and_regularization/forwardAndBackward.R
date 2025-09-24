library(ISLR2)
library(leaps)

regfit.fwd = regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="forward")
print(summary(regfit.fwd))
regfit.bwd = regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="backward")
print(summary(regfit.bwd))

# Quite similar, but for instance model 7 is different.