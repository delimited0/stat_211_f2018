####
#### NFL example. Consider a team that wins 9 of its 16 games in a season. 
####

## Proportion of games won.
9 / 16

## Let p be the proportion of games the team would win out of a very large number of 
## games (thousands, say). For example, suppose p = 0.5. That is, the team can be 
## expected to win half of the games they play. We can simulate the results of a season 
## as follows (0 means loss, 1 means win). Run the following 4 lines of code multiple 
## times to see multiple simulated seasons.
p <- 0.9

season <- sample(c(0, 1), size = 16, replace = TRUE, prob = c(1 - p, p))
win_total <- sum(season)
season
win_total

## Now let's repeat the above simulation 5000 times and record the season win total each 
## time. How likely would it be to observe 9 wins in a single given season, if the true 
## long-run proportion of games won is p = 0.5?
win_total <- numeric(5000)
for(i in 1:5000) {
  season <- sample(c(0, 1), size = 16, replace = TRUE, prob = c(1 - p, p))
  win_total[i] <- sum(season)
}

## Here are the frequencies (counts) for all possible win totals.
table(win_total)

## Here are the corresponding proportions. Thus, if p = 0.5, we can expect the team to 
## win 9 or more games in about 40% of all seasons. It would actually be more common to 
## observe 8 wins than 9 wins in a season. Other season totals (e.g., 6, 7, and 10) would 
## be expected in more than 10% of seasons. Every once in a while, by chance, the team 
## would win 12 or more games in a season. Similarly, every once in a while the team 
## would win 4 or fewer. All while p = 0.5. 
table(win_total) / 5000
sum(win_total >= 9) / 5000
