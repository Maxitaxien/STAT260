library(boot)

# a)
set.seed(1)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

# n = 100 (100 samples), p = 1 (only predictor is x)
# we also add random noise

# b)
plot(x, y)
# There is a clear quadratic relationship, not surprising since
# we generated y from a combination of x and x^2

# c)
run_loocv = function(k, data) {
    cv.error = rep(0, k)
    for (i in 1:k) {
        glm.fit = glm(y ~ poly(x, i), data=data)
        print(summary(glm.fit))
        cv.error[i] = cv.glm(data, glm.fit)$delta[1]
    }

    return(cv.error)
}

data = data.frame(x, y)

cv.error = run_loocv(4, data) 
print(cv.error) # best for degree 2 - 3 and 4 may be overfitting in the CV

# d) 
set.seed(69)
cv.error = run_loocv(4, data)
print(cv.error)
# Exactly the same! Loocv is deterministic - always tests for everything except observation i for each obs

# e) The 2nd degree - just as expected. One might think that the higher polynomial models are more flexible and give better results,
# and this is true if we train on all the data and evaluate. But when training on everything except one datapoint,
# this means that the models can perform more poorly on the final held-out datapoint, causing a worse performance.

# f)
# Examining the summaries, coef 1 and 2 (x^1 and x^2) are given high statistical significance, but not x^3 and x^4.
# This is consistent with the conclusion from e).