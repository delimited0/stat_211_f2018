# plotting pmfs

# binomial
n <- 10
p <- .7
x <- 1:10

plot(x, dbinom(x, size = 10, prob = p), type = "p", pch = 19) # plot dots
lines(x, dbinom(x, size = 10, prob = p), type = "h") # draw lines to dots

# geometric
p <- .7
x <- 1:20

plot(x, dgeom(x, prob = p), pch = 19)
lines(x, dgeom(x, prob = p), type = "h")

# poisson
x <- 1:15
rate <- 10
plot(x, dpois(x, lambda = rate), pch = 19)
lines(x, dpois(x, lambda = rate), type = "h")


