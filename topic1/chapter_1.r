####
#### An illustration of probability as a long-run phenomenon.
####

## Flip a coin once.
sample(c(0, 1), size = 1)

##
## Flip a coin 5000 times.
##

x <- sample(c(0, 1), size = 5000, replace = TRUE)
x[1:10]

## What proportion of flips were heads?
table(x)
sum(x) / 5000
mean(x)

## How did the proportion change over the course of all flips?
p <- cumsum(x) / 1:5000

plot(1:5000, p, xlab = "Number of Flips", ylab = "Proportion", type = "l")
abline(0.5, 0, lty = 2, col = "red")

####
#### Steph Curry (point guard for the Golden State Warriors in the National Basketball 
#### Association) has made about 90% of his free throws in the NBA. Suppose that the 
#### probability he makes any given free throw equals 0.90. What might we expect to see
#### in a sequence of 10 free throw shots?
####

## Simulate 10 free throws (do it many times). Note that sometimes he hits all 10, 
## but every once in a while he misses 3, 4, or even more, just by chance. 
sample(c(0, 1), size = 10, replace = TRUE, prob = c(0.10, 0.90))

## Let's repeat the above 5000 times and keep track of the number of free throws (out of 
## 10) that he makes.
x <- numeric(5000)
for(i in 1:5000) {
  x[i] <- sum(sample(c(0, 1), size = 10, replace = TRUE, prob = c(0.10, 0.90)))
}

## Notice the *distribution* of outcomes. Most of the time, he makes 8 or more out of 10. 
## It is not that unusual for him to make only 7 out of 10. It is rare (but it happens) 
## that he makes 6 or fewer.
table(x)

## Now consider a rookie who in his first and only game so far made 6 out of 10 free 
## throws. Based on this sample of observations, what would we estimate his shot making 
## probability to be? Suppose the coach only wants players who can make at least 85% of 
## their free throws on his team. Is it plausible that the rookie really makes 85% of his 
## shots and just got unlucky in this game? Let's simulate 5000 games, assuming his 
## probability is indeed 0.85.
x <- numeric(5000)
for(i in 1:5000) {
  x[i] <- sum(sample(c(0, 1), size = 10, replace = TRUE, prob = c(0.15, 0.85)))
}

## If the rookie's probability really were 0.85, it would be rare (less than 10 times out 
## of 100) for him to make 6 or fewer out of 10 shots. 
table(x)


