####
#### In the 2015 NFL season, the Houston Texans won 9 of their 16 games. Do we believe 
#### they were truly an above-average team, or might they just be average or worse, with 
#### their above-average record being due to luck?
####
#### http://query.nytimes.com/gst/fullpage.html?res=9A06EFD91E30F93BA15753C1A96F9C8B63
####

p_hat <- 9 / 16

##
## Suppose p were really equal to 0.5, and the Texans were an average team, expected to 
## win 50% of their games *in the long run*. Considering the 16 games in the 2015 season 
## as a "random sample" from the "population" that consists of all possible games played 
## by the 2015 Texans, what might other random samples of 16 games look like?
##

## Simulate 1,000,000 16-game season win totals, assuming p = 0.5.
N <- 1e6
n <- 16
p_0 <- 0.5
y <- rbinom(N, n, p_0)

## A substantial proportion of the time, even though p = 0.5, we would expect to see 9 
## (or more) wins just by chance.
hist(y)
mean(y >= 9)

#* Shade in the portion of the histogram that corresponds to 16-game season win totals of 
#* 9 or more games.
hh <- hist(y, probability = TRUE)
brks <- hh$breaks
dnst <- hh$density
polygon(c(9, 9, 10, 10), c(0, dnst[9 + 1], dnst[9 + 1], 0), col = "red")
for(i in 10:15)
  polygon(rep(i:(i + 1), each = 2), c(0, rep(dnst[i + 1], 2), 0), col = "red")

##
## While we can't know for certain what the true value of p is, we might search for a 
## range of plausible values about which we can be "highly confident" to have captured 
## the truth. One deceptively-simple technique for doing this is called the "bootstrap."
## To bootstrap, we treat our observed sample (in this case, 16 wins or losses) as if it 
## were the population of all possible games played and repeatedly take random samples 
## from it. By inspecting the resulting simulated 16-game win totals, we can make 
## probabilistic statements with respect to the likely value of p. 
##

## Texans' 2015 wins and losses.
texans_2015 <- c(0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1)

B <- 250
p_boot <- numeric(B)
for(b in 1:B) {
  ## Take bootstrap sample and record the win proportion.
  p_boot[b] <- sum(sample(texans_2015, replace = TRUE)) / 16
}

hist(p_boot)

## Suppose we were to use the interval between the 2.5th and 97.5th percentiles, within 
## which 95% of p_hat estimates were observed (by simulation), as an interval estimate 
## for p. 
quantile(p_boot, c(0.025, 0.975))

## Now, pick a possible value for p; say, p = p_hat = 0.5625. Watch what happens when we 
## use the bootstrap as a data-analytic strategy. Try varying 'desired_confidence'.
M <- 1000
p_hat_M <- contains_truth <- numeric(M)
ci_M <- matrix(numeric(M * 2), nrow = M)
desired_confidence <- 0.95
qq <- (1 - desired_confidence) / 2 * c(1, -1) + c(0, 1)
for(i in 1:M) {
  ## Simulate a single 16-game season.
  y <- rbinom(16, 1, p_hat)
  p_hat_M[i] <- sum(y) / 16
  
  ## Use bootstrap to compute "confidence interval".
  p_boot_sim <- numeric(B)
  for(b in 1:B)
    p_boot_sim[b] <- sum(sample(y, replace = TRUE)) / 16
  ci_M[i, ] <- quantile(p_boot_sim, c(qq[1], qq[2]))
  contains_truth[i] <- p_hat >= ci_M[i, 1] & p_hat <= ci_M[i, 2]
}

mean(contains_truth)

## A picture to illustrate what happens with a 95% CI.
ii <- sample(c(sample((1:M)[contains_truth == 1], 95, replace = FALSE), 
  sample((1:M)[contains_truth == 0], 5, replace = FALSE)))
plot(c(0, 1), c(1, 100), xlab = "", ylab = "Simulated Bootstrap-Based 95% C.I.", 
  type = "n")
for(i in 1:100)
  lines(ci_M[ii[i], ], rep(i, 2), col = c("red", "green")[contains_truth[ii[i]] + 1])
lines(rep(p_hat, 2), c(-10, 110), lty = 2)

##
## Another way to do "statistical inference" is to test a particular claim about the 
## value of p. For example, we might ask the following question: *Assuming that* p is 
## really equal to 0.5, and the Texans are an average team, how likely would it be for 
## us to see 9 or more wins out of 16 games? If outcomes like the one we observed (9 wins 
## or more) are very unlikely under the assumption that p = 0.5, then we can confidently 
## reject that assumption and conclude that the truth is that p > 0.5.
##

## In this case, we can use the binomial probability function to compute the probability 
## we want exactly. Compare this to the proportion of simulated realizations from the 
## beginning of this script (simulated under the scenario in which p = 0.5) that gave 9 
## or more wins. The probability, 0.4018097, is called a "p-value" for testing the "null 
## hypothesis" that p = 0.5 against the "alternative hypothesis" that p > 0.5. A common 
## threshold for deciding whether to reject the null hypothesis is that the p-value is
## less than 0.05 (or 0.1 or 0.01, depending on the situation and the preferences of the 
## investigator). In this case, a 9 win season (or better) is quite likely for an average
## team, and we do not have sufficient evidence to conclude the Texans' were above 
## average in the 2015 season.
1 - pbinom(8, 16, 0.5)

## The above 'exact test' is equivalent to using 'binom.test'.
binom.test(9, 16, 0.5, "greater")

## Similar to how we used the bootstrap above to compute a confidence interval for p, we 
## can use simulation to approximate a p-value. It turns out that hypothesis testing has 
## a direct relationship with confidence intervals. In particular, if the null hypothesis 
## value of p is not contained in a 95% confidence interval for p, then we know that the 
## p-value is no bigger than 0.05. Similarly, if the hypothesized value of p is not 
## contained in a 99% confidence interval, we know the p-value is no bigger than 0.01.
##
## Note in this case that, since our alternative hypothesis is "one-sided" (an 
## alternative hypothesis that p *does not equal* the null value (call it p_0) would be 
## "two-sided", supported by evidence that p > p_0 *or* that p < p_0), the confidence 
## interval we require is itself "one-sided" (basically, a lower bound on p). We can 
## obtain such an interval via bootstrap by computing an appropriate single percentile 
## from the bootstrap distribution of season win totals. In this case, because the 
## interval does contain p_0 = 0.5, we "fail to reject" the null hypothesis. 
quantile(p_boot, 0.05)

## A confidence interval *and* a p-value is ideal, as, while use of a confidence interval 
## enables an hypothesis test, a p-value is a specific quantification of our confidence 
## with respect to the null hypothesis. 
##
## We can also use simulation to approximate an actual p-value. To do this, we need to 
## first force the null hypothesis to be true. This is because the p-value is defined as 
## the probability, in this case, of 9 or more wins *when p = p_0 = 0.5*. By centering 
## our observed sample (the Texans' win and loss record in 2015) to have mean 0.5, we 
## force the null to be true while appropriately preserving the sampling variability.
B <- 250
texans_2015_0 <- texans_2015 - mean(texans_2015) + 0.5
p_boot_0 <- numeric(B)
for(b in 1:B) 
  p_boot_0[b] <- mean(sample(texans_2015_0, replace = TRUE))

p_value <- mean(p_boot_0 >= p_hat)
p_value

## Now, as we did above, pick a possible value for p; say, p = p_0 = 0.5. Watch what 
## happens when we use the bootstrap as a data-analytic strategy for testing the null 
## hypothesis that p = 0.5 vs. the alternative hypothesis that p > 0.5. Try varying 
## 'desired_confidence'.
M <- 1000
p_0 <- 0.5
p_hat_M <- p_value_M <- rej_null <- numeric(M)
desired_confidence <- 0.8
for(i in 1:M) {
  ## Simulate a single 16-game season.
  y <- rbinom(16, 1, p_0)
  p_hat_M[i] <- sum(y) / 16
  
  ## Use bootstrap to compute p-value and reject the null hypothesis if the p-value is 
  ## less than the appropriate threshold.
  p_boot_0 <- numeric(B)
  y_0 <- y - mean(y) + 0.5
  for(b in 1:B)
    p_boot_0[b] <- sum(sample(y_0, replace = TRUE)) / 16
  p_value_M[i] <- mean(p_boot_0 >= p_hat_M[i])
  
  ## Make decision.
  rej_null[i] <- p_value_M[i] < (1 - desired_confidence)
}

mean(rej_null)

## Repeat above, now applying it to a range of possible values of p.
M <- 500
p_seq <- seq(from = 0.01, to = 0.99, by = 0.02)
p_hat_M <- p_value_M <- rej_null <- numeric(M)
rej_prob <- numeric(length(p_seq))
desired_confidence <- 0.95
for(j in 1:length(p_seq)) {
  cat(".")

  for(i in 1:M) {
    ## Simulate a single 16-game season.
    y <- rbinom(16, 1, p_seq[j])
    p_hat_M[i] <- sum(y) / 16
  
    ## Use bootstrap to compute p-value and reject the null hypothesis if the p-value is 
    ## less than the appropriate threshold.
    p_boot_seq <- numeric(B)
    y_0 <- y - mean(y) + 0.5
    for(b in 1:B)
      p_boot_seq[b] <- sum(sample(y_0, replace = TRUE)) / 16
    p_value_M[i] <- mean(p_boot_seq >= p_hat_M[i])
  
    ## Make decision.
    rej_null[i] <- p_value_M[i] < (1 - desired_confidence)
  }
  
  rej_prob[j] <- mean(rej_null)
}

## What we get is an estimated (using simulation) "power curve," or the function that 
## describes how the probability we reject the null hypothesis varies as a function of 
## the true value of p. For example, if the null hypothesis were *true*, so that p = 0.5, 
## then the probability we *incorrectly* reject the null appears to be a little less than 
## 0.05 when reject on the basis of whether a p-value is less than 0.05. In general, it 
## turns out that testing a null hypothesis at "significance level" equal to one minus 
## our 'desired_confidence' above can be expected to result in an erroneous rejection of 
## the null (a "Type I error") with probability no larger than 'desired_confidence'. As 
## the true value of p increases, our "power" to detect that it does not in actuality 
## equal 0.5 increases, tapering off at one. As the true value of p *decreases*, the 
## rejection probability tapers off to zero, which means that we are less likely to 
## incorrectly conclude that p > 0.5 if p is in actuality *less than* 0.05, as compared 
## to if p = 0.5 (the null case), which seems sensible.
plot(p_seq, rej_prob, type = "l", xlab = "True value of p", 
  ylab = "Rejection proportion", main = "Estimated power curve for bootstrap test")
axis(1, at = 0.5, col = "red")
abline(1 - desired_confidence, 0, lty = 2)
axis(4, at = 1 - desired_confidence, col = "red")

##
## Bayesian inference. Suppose that p can only take the values 0, 1 / 16, 2 / 16, ..., 1.
##

## Bernoulli *and* Binomial likelihoods: equivalent.

## Uniform prior.

## Non-uniform prior.

## Improper priors.

## Credible interval for p.

## Credible interval for p / (1 - p).



