# R as a calculator; 
# everything your TI-84 can do R can do better

# solve the quadratic equation 3x^2 + 2x - 10 = 0

# how many would enter values into a calculator:
(-2 + sqrt(4  - 4 * 3 * (-10))) / (2 * 3)
(-2 - sqrt(4  - 4 * 3 * (-10))) / (2 * 3)


# save intermediate components, it's more readable
a <- 3
b <- 2
c <- -10

twoA <- 2 * a
fourAC <- 4 * a * c
sqrtB4AC <- sqrt(b^2 - fourAC)

(-b + sqrtB4AC) / twoA
(-b - sqrtB4AC) / twoA


# to solve a new equation, 2x^2 + 3x - 5 = 0, reset a, b, and c
a <- 2
b <- 3
c <- -5

twoA <- 2 * a
fourAC <- 4 * a * c
sqrtB4AC <- sqrt(b^2 - fourAC)

(-b + sqrtB4AC) / twoA
(-b - sqrtB4AC) / twoA


# better yet, write a function to compute roots
quadratic_solve <- function(a, b, c) {
  r1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  r2 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
  return(
    c(r1, r2)
  )
}

quadratic_solve(3, 2, -10)
quadratic_solve(2, 3, -5)


# work with me ----------------------------------------------------------------

# optimize the function x^4 * (1 - x)^2

# function value
x <- 2
A <- x^4
B <- (1 - x)^2
A * B

# derivative
dA <- 4 / x
dB <- - 2 / (1 - x)
dA + dB

binomial_func <- function(x, k, n) {
  x ^ k * (1 - x) ^ (n - k)
}

# can use symbolic differentation
D(expression(log(x^4) + log((1-x)^2)), name = "x")


# sum up the function (1/3)^k * (1 - (1/3))^(5-k) 
# for values of k in c(0, 1, 2, 3)

foo <- 3





