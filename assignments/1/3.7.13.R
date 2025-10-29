set.seed(1)

# a)
x = rnorm(100, 0, 1)

# b)
eps = rnorm(100, 0, 0.25)

# c)
y = -1 + 0.5*x + eps

# The vector y has length 100.
# B0 is the constant intercept: -1
# B1 is the coefficient of X: 0.5

# d)
plot(x, y)
# We observe a linear relationship between x and y.
# There is a little bit of noise (which we introduced through eps)
# but the overall trend is linear. Unsurprising as we constructed y
# to be a linear function of x.

# e)
lm.fit = lm(y ~ x)
print(summary(lm.fit))
# The summary reveals a B0 estimate of -1.00942, quite close
# to the true value of -1, but slightly adjusted because of the noise.

# The B1 estimate is 0.49973, which is also quite close.
# Both B0 and B1 are statistially significant.

# f)
abline(lm.fit, col="blue")
legend(
    "topleft", 
    legend=c("True X vs Y", "Linear Regression Fit"),
    col=c("black", "blue"),
    pch=c(19, NA),
    lty=c(NA, 1)
)

# g)
lm.fit2 = lm(y ~ x + I(x^2))
print(summary(lm.fit2))
# The B2 coefficient associated with x^2 has a P-value of 0.164
# Therefore, it is not statistically significant,
# and there is little evidence that it improves model fit
# (although residual standard error is slightly lower, 
# the model will not generalize better)

# h)
# Setting up a function to make steps a-f generalizable:
generate_and_fit = function(noise_level) {
    # Generate data, some plots and 
    # return fitted model.
    x = rnorm(100, 0, 1)
    eps = rnorm(100, 0, noise_level)
    y = -1 + 0.5*x + eps

    # open new plotting window
    dev.new()
    plot(x, y)
    lm.fit = lm(y ~ x)
    abline(lm.fit, col="blue")
    legend(
    "topleft", 
    legend=c("True X vs Y", "Linear Regression Fit"),
    col=c("black", "blue"),
    pch=c(19, NA),
    lty=c(NA, 1)
    )
    return (lm.fit)
}

lm.fit.less = generate_and_fit(0.01)
print(summary(lm.fit.less))
# With less noise, the estimates are more accurate
# (B0 = -0.9995155 and B1 = 0.5010622)
# We see that this model fits the data almost perfectly,
# with a residual standard error of 0.009906.

# i)
lm.fit.more = generate_and_fit(0.5)
print(summary(lm.fit.more))
# More noise makes our estimate more variable and estimates less accurate
# (B0 = -1.02373, B1 = 0.46253)
# Attempting to fit the noisy data gives
# a poorer residual standard error of 0.4835

# j)
# Assuming confidence levels of .95
conflevel = 0.95
print(confint(lm.fit, level = conflevel))
print(confint(lm.fit.less, level = conflevel))
print(confint(lm.fit.more, level = conflevel))

# Rounded to three decimal points:
# Original model: 
# B0: [-1.058, -0.961]
# B1: [0.446, 0.553]

# Less noise:
# B0: [-1.001, -0.998]
# B1: [0.499, 0.503]

# More noise:
# B0: [-1.112, -0.928]
# B1: [0.380, 0.545]


# Comments: 
# A more noisy dataset gives a more unsure estimate of the parameters.
# This is because with repeated sampling the coefficient estimates will
# vary more, which means that the 95% confidence interval must
# be broader for more noisy data.