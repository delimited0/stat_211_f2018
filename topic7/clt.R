# illustration of CLT ----
# 30 draws from population per sample
n <- 30
exps <- rexp(n)
hist(exps, breaks = "FD")
mean(exps)

# 1000 draws from sampling distribution
exp_means <- replicate(1000, {
  exps <- rexp(n)
  mean(exps)
})

x <- seq(min(exp_means), max(exp_means), .01)
hist(exp_means, probability = TRUE, breaks = "FD")
lines(x, dnorm(x, 1, sqrt(1 / n)))
legend("topright", legend = c("Limit"), lty = 1)

# another distribution ----
n <- 30
betas <- rbeta(n, .2, .2)
hist(betas, breaks = 10)
mean(betas)
m <- .5
v <- .2^2 / (.4^2 * 1.4)

beta_means <- replicate(1000, {
  betas <- rbeta(n, .2, .2)
  mean(betas)
})

x <- seq(min(beta_means), max(beta_means), .01)
hist(beta_means, probability = TRUE, breaks = "FD")
lines(x, dnorm(x, m, sqrt(v / n)))
legend("topright", legend = c("Limit"), lty = 1)



