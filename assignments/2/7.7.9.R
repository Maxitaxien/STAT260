library(ISLR2)
attach(Boston)

set.seed(1)
# a)
fit = lm(nox ~ poly(dis, 3), data=Boston)
# print(fit) 

# Making a grid of dis to predict based on
# Inspired by examples given on p. 313 in the book
dislims = range(dis)
dis.grid = seq(from=dislims[1], to=dislims[2])
preds = predict(fit, newdata=list(dis=dis.grid))

plot(dis, nox, col="blue")
lines(dis.grid, preds, col="red")

# b)
N = 10
RSS = numeric(N)
par(mfrow=c(2, 5))

for (i in 1:N) {
    fit = lm(nox ~ poly(dis, i), data=Boston)
    
    # use grid prediction for plotting
    preds = predict(fit, newdata=list(dis=dis.grid))
    plot(dis, nox, col="blue")
    lines(dis.grid, preds, col="red")
    title(main=paste("Degree = ", i))

    # use all data for prediction RSS
    RSS[i] = sum((nox - predict(fit, Boston))^2)
}

print(RSS)

# c) (Using CV)
library(boot)

cv.errors = numeric(N)
for (i in 1:N) {
    fit = glm(nox ~ poly(dis, i), data=Boston) 
    # 10-fold CV, extract bias-corrected estimate and store
    cv.errors[i] = cv.glm(Boston, fit, K=10)$delta[2]
}

degree.best = which.min(cv.errors) # find index with minimum value
print(degree.best) # reports polynomial degree "4" as best

# d)
library(splines)
# Choose knots automatically
# bs() places knots at quantiles
fit.spline = lm(nox ~ bs(dis, df=4), data=Boston)
knot = attr(bs(dis, df=4), "knots")
print(fit.spline)

# With 4 df, there is only 1 degree of freedom left, so only one knot is placed
# at the 50% quantile.
print(knot) # 3.20745
print(knot == median(dis)) # TRUE

# Plot:
par(mfrow=c(1, 1))
preds = predict(fit.spline, newdata=list(dis=dis.grid))
plot(dis, nox, col="blue")
lines(dis.grid, preds, col="red")

# e)
# DFs in a range of 4 -> 8
df_s = 4
df_e = 8
# Amount of fits:
M = df_e - df_s + 1
RSS = numeric(M)


par(mfrow=c(1, 5))
for (df in df_s:df_e) {
    fit.spline = lm(nox ~ bs(dis, df=df), data=Boston)

    # use grid prediction for plotting
    preds = predict(fit.spline, newdata=list(dis=dis.grid))
    plot(dis, nox, col="blue")
    lines(dis.grid, preds, col="red")
    title(main=paste("DF = ", df))

    preds = predict(fit.spline, Boston)
    print(length(nox))
    print(length(preds))
    RSS[df - df_s + 1] = sum((nox - preds)^2)
}

print(RSS)


# f)
# Performing 10-fold CV
cv.errors = numeric(df_e - df_s)
for (df in df_s:df_e) {
    fit.spline = glm(nox ~ bs(dis, df=df), data=Boston)
    # 10-fold CV, extract bias-corrected estimate and store
    cv.errors[df - df_s + 1] = cv.glm(Boston, fit.spline, K=10)$delta[2]
}

degree.s.best = which.min(cv.errors)
# print(cv.errors)
print(degree.s.best) # Gives number 2 (df = 5) as best model.