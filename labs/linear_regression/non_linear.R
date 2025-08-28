lm.fit2 = lm(medv ~ lstat + I(lstat^2), data=Boston)

# I function is a formula object for wrapping
print(summary(lm.fit2))

# anova function for quantifying improvement in quadratic value over linear
lm.fit = lm(medv ~ lstat)
print(anova(lm.fit, lm.fit2))
# performs hypothesis test: h0 = they fit data equally well, h1 = full model is superior
# here, p-val is almost zero and F-stat is high - so it is superior
par(mfrow = c(2, 2))
plot(lm.fit2)

# 5th order using poly command
lm.fit5 = lm(medv ~ poly(lstat, 5))
print(summary(lm.fit5))