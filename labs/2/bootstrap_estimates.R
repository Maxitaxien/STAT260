library(boot)
library(bootstrap)

print(dim(law))
print(summary(law))

set.seed(1)


# Part one: Estimating standard error with correlation
B = 5000 # amount of samples for bootstrap

boot_fn_corr = function(data, idx) {
    cor(data$LSAT[idx], data$GPA[idx])
}
boot_corr = boot(law, statistic=boot_fn_corr, R=B)
# boot_corr$t0 is the correlation computed once on the full dataset, no resampling
# boot_corr$t is the bootstrap replicates from sampling

boot_se = sd(boot_corr$t)
print(boot_corr)
print(boot_se)

# Part two: Bias estimate:
boot_bias = mean(boot_corr$t) - boot_corr$t0
print(boot_bias)

# Part three: CI's
# Need to make nested boostrap for t-distribution
boot_fn_corr_nested = function(data, idx) {
    corr_outer = cor(data$LSAT[idx], data$GPA[idx])
    n = length(data$LSAT)

    B_inner = 200
    corr_inner = replicate(B_inner, {
        idx_inner = sample(seq_len(n), size=n, replace=TRUE)
        cor(data$LSAT[idx_inner], data$GPA[idx_inner])
    })

    se_outer = sd(corr_inner)

    c(corr_outer, se_outer)
}

set.seed(1)
B_outer <- 1000  # outer bootstrap replicates
boot_corr_nested <- boot(law, statistic = boot_fn_corr_nested, R = B_outer, sim = "ordinary", stype = "i")

# Now get CIs
cis = boot.ci(boot_corr_nested, type = c("basic", "norm", "perc", "stud"))
print(cis)