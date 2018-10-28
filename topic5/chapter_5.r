####
#### Suppose I use Top Hat to collect responses to a collection of 4-choice multiple 
#### choice questions over the course of a semester. For each question, there will be a 
#### probability distribution, specific to this class, for the responses to each 
#### question. Suppose a student randomly picks his or her answers, or just selects the 
#### same value each time. Can I detect this?
####

n <- 100
p <- 150

## Create answer distributions (randomly).
pp <- matrix(NA, nrow = p, ncol = 4)
a_prob <- runif(p, 0, 1)
b_prob <- c_prob <- d_prob <- numeric(p)
for(i in 1:p) {
  b_prob[i] <- runif(1, 0, 1 - a_prob[i])
  c_prob[i] <- runif(1, 0, 1 - (a_prob[i] + b_prob[i]))
  d_prob[i] <- 1 - (a_prob[i] + b_prob[i] + c_prob[i])
  
  pp[i, ] <- c(a_prob[i], b_prob[i], c_prob[i], d_prob[i])
}

## Simulated realizations for the class: 98 students answering authentically, 1 student 
## picking 'a' as their choice each time, and 1 student randomly picking their choices.
Y <- matrix(NA, nrow = n, ncol = p)
Y[99, ] <- rep(1, p)
Y[100, ] <- sample(1:4, p, replace = TRUE)
for(i in 1:p) {
  Y[1:98, i] <- apply(rmultinom(98, 1, pp[i, ]), 2, which.max)
}

## Test whether each student is being authentic in their answers.
