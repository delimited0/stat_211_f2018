####
#### You meet a colleague to discuss business over multiple games of billiards. You have 
#### never played billiards with this person before, and you do not have much confidence 
#### in your billiards skills.
####

## These are the possible values of p that we are going to consider.
p <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
p <- seq(from = 0.0, to = 1.0, by = 0.1)

## These are the probabilities we subjectively assign to each possible value of p, our 
## *prior* distribution.
pri <- c(0.00, 0.10, 0.25, 0.20, 0.15, 0.10, 0.05, 0.05, 0.05, 0.05, 0.00)
plot(p, pri, type = "h", lwd = 4, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))

## The likelihood after winning the first 3 games (WWW) is the product of the 
## corresponding 3 Bernoulli pmf values (the 'joint probability of seeing WWW', assuming 
## the games are independent of one another and that the probability p is the same for 
## each game).
lik_1 <- p ^ 3 * (1 - p) ^ 0

## The posterior distribution after winning the first 3 games.
pos_1 <- lik_1 * pri / sum(lik_1 * pri)

plot(p, pri, type = "b", ylim = c(0, 0.4), ylab = "probability",
     lwd = 4, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
lines(p, pos_1, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5), 
      type = "b", lwd = 4)

## Prior and posterior probability that you are a better player.
sum(pri[p > 0.5])
sum(pos_1[p > 0.5])

## Now suppose you lose the next 2 games. The likelihood and posterior after observing 
## now 5 games: WWWLL.
lik_2 <- p ^ 3 * (1 - p) ^ 2
pos_2 <- lik_2 * pri / sum(lik_2 * pri)
sum(pos_2[p > 0.5])

lines(p, pos_2, col = rgb(red = 0, green = 1, blue = 0, alpha = 0.5), 
      type = "b", lwd = 4)
# legend(0.2, 0.4, legend = c("prior: Pr(p > 0.5) = 0.2", 
#   "posterior_1 (WWW): Pr(p > 0.5 | WWW) = 0.75", 
#   "posterior_2 (WWWLL): Pr(p > 0.5 | WWWTT) = 0.31"), 
#   lwd = rep(3, 2), col = c("red", "blue", "green"))

## Equivalent to a sequential analysis, where we use 'pos_1' as our prior (having now 
## seen the first 3 games as data) and compute the likelihood of just the most recent 2 
## losses.
pri_seq <- pos_1
lik_2_seq <- p ^ 0 * (1 - p) ^ 2
pos_2_seq <- lik_2_seq * pri_seq / sum(lik_2_seq * pri_seq)
cbind(pos_2, pos_2_seq)

##
## Now suppose we replace our crude initial prior (allowing only 11 possible values for 
## p) with a continuous prior distribution. In particular, the Beta distribution, with 
## parameters alpha and beta, is suitable for use as a prior for a probability, since it 
## only assigns non-zero probabilities in the interval [0, 1]. As shown elsewhere, with a 
## Binomial likelihood and a Beta prior, the posterior distribution is itself a Beta 
## distribution, with parameters that have been updated to reflect the observed data.
##

## The Beta(1, 1) is the continuous version of the 'uniform' distribution.
curve(dbeta(x, 1, 1))
curve(dunif(x, 0, 1))

## The Beta(6, 6) would reflect increased prior belief that you are an average (p = 0.5) 
## player.
curve(dbeta(x, 6, 6))

## To roughly mimic our original pessimistic prior, we might use Beta(1.8, 6).
curve(dbeta(x, 6, 1.8))

## With observed data X = 3 from a Binomial(n = 5, p) likelihood, and with a Beta(1.8, 6) 
## prior for p, the posterior distribution of p is Beta(alpha + x, beta + n - x), or 
## Beta(4.8, 8).
curve(dbeta(x, 6, 1.8), col = "red", ylab = "f(x)")
curve(dbeta(x, 4.8, 8), col = "blue", add = TRUE)
legend(0.7, 2.5, legend = c("prior", "posterior (WWWLL)"), lwd = rep(2, 2), 
  col = c("red", "blue"), bty = "n")

