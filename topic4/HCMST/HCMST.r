####
#### How Couples Meet and Stay Together (HCMST), The Interuniversity Consortium for 
#### Political and Social Research (ICPSR). Wave 1 collected in 2009, Wave 2 in 2010, 
#### Wave 3 in 2011, and Wave 4 in 2013. United States.
####
#### Here, we will consider only the 3,009 individuals surveyed who were characterized as 
#### being in a relationship of some kind. For exploratory purposes, we will investigate 
#### patterns and illustrate some statistical techniques. Note, however, that a 
#### sophisticated sampling method was used by the study directors and that sampling 
#### weights must be applied before meaningful inferences on the more general population 
#### of interest can be attempted. We will discuss this issue further in a later lecture.
####

dta <- read.csv("HCMST.csv", row.names = NULL)
dim(dta)
head(dta)
summary(dta)

##
## Age difference between partners.
##

## Treating our 3,009 individuals as our population of interest, create the pmf of the 
## age difference variable (call it X). Note that X has some missing values. Note also 
## the two couples for whom age difference is 69 and 70 years (!), respectively.
with(dta, table(is.na(AGE_DIFFERENCE)))
tt <- with(dta, table(AGE_DIFFERENCE))
x_possibilities <- as.numeric(names(tt))
f_X <- tt / sum(tt)
sum(f_X)
plot(f_X)

## E(X) and Var(X).
mu_X <- sum(x_possibilities * f_X)
sig_2_X <- sum((x_possibilities - mu_X) ^ 2 * f_X)

## Can match by just computing sample statistics. The minor discrepancy between variance 
## calculations is due to the sample variance dividing by n - 1 instead of n; again, this 
## is a technical point that we have not yet fully addressed in this class.
with(dta, mean(AGE_DIFFERENCE, na.rm = TRUE))
with(dta, var(AGE_DIFFERENCE, na.rm = TRUE))
with(dta, var(AGE_DIFFERENCE, na.rm = TRUE)) * (sum(tt) - 1) / sum(tt)

##
## GLB status and political party affiliation. (GLB - gay, lesbian, bisexual)
##

## Joint distribution, considering GLB status as X and political party affiliation as Y.
freq_table <- with(dta, table(GLBSTATUS, Q12))
print(f_XY <- prop.table(freq_table))
sum(f_XY)

## Marginal distribution of GLB status.
print(f_X <- apply(f_XY, MARGIN = 1, FUN = sum))
rowSums(f_XY)
sum(f_X)

## Marginal distribution of political party affiliation.
print(f_Y <- apply(f_XY, 2, sum))
colSums(f_XY)
sum(f_Y)

## A "mosaic" plot as a way to visualize our "contingency table".
##
## NOTE: May need to first install 'vcd' package.
library(vcd) 

mosaic(freq_table)

#* A slightly-prettier version can be had if we re-code the values of the two variables.
##
#* NOTE: May need to first install 'car' package.

library(car)

GLBSTATUS_old <- dta$GLBSTATUS
GLBSTATUS_new <- recode(GLBSTATUS_old, "'(0) not glb'='not glb'; '(1) glb'='glb'")

Q12_old <- dta$Q12
Q12_new <- recode(Q12_old, "'(1) republican'='republican'; '(2) democrat'='democrat'; '(3) independent'='independent'; '(4) another party, please specify'='other'; '(5) no preference'='no preference'")

mosaic(table(GLBSTATUS_new, Q12_new))

## Conditional distribution of political party affiliation, given GLB status.
print(f_Y_given_not_glb <- f_XY[1, ] / f_X[1])
print(f_Y_given_glb <- f_XY[2, ] / f_X[2])
sum(f_Y_given_not_glb)
sum(f_Y_given_glb)

## Law of total probability applied to the probability that a randomly-selected 
## individual is a democrat.
print(p_democrat <- f_Y_given_not_glb[2] * f_X[1] + f_Y_given_glb[2] * f_X[2])
f_Y[2]
sum(f_XY[, 2])

## Bayes' Theorem applied to the probability of GLB, given democrat.
print(p_GLB_given_democrat <- (f_Y_given_glb[2] * f_X[2]) / p_democrat)
freq_table[2, 2] / sum(freq_table[, 2])

##
## Statistical inference on the difference in HHINC, comparing GLB to non-GLB.
##

med_diff_obs <- with(dta, median(HHINC[GLBSTATUS == "(1) glb"]) - 
  median(HHINC[GLBSTATUS == "(0) not glb"]))

## 95% confidence interval via bootstrap.
B <- 500
med_diff_b <- numeric(B)
for(b in 1:B) {
  ## Create bootstrap sample by sampling with replacement from each of the two groups.
  y_not_glb_b <- with(dta, sample(HHINC[GLBSTATUS == "(0) not glb"], replace = TRUE))
  y_glb_b <- with(dta, sample(HHINC[GLBSTATUS == "(1) glb"], replace = TRUE))
  
  ## Compute and store median difference in household income.
  med_diff_b[b] <- median(y_glb_b) - median(y_not_glb_b)
}
quantile(med_diff_b, c(0.025, 0.975))

## Hypothesis test via bootstrap. Note that, since the 95% confidence interval does not 
## contain 0, we expect that the p-value for testing H_0: median difference = 0 vs. H_a: 
## median difference != 0 will be less than 0.05. Still, it is useful to have an actual 
## p-value for quantifying just how strong our evidence against H_0 is. Before running 
## the bootstrap, we center the observations in the two groups to have the same median, 
## forcing H_0 to be true.
med_diff_0_b <- numeric(B)
y_not_glb_0 <- with(dta, HHINC[GLBSTATUS == "(0) not glb"] - 
  median(HHINC[GLBSTATUS == "(0) not glb"]))
y_glb_0 <- with(dta, HHINC[GLBSTATUS == "(1) glb"] - 
  median(HHINC[GLBSTATUS == "(1) glb"]))
for(b in 1:B) {
  ## Construct bootstrap sample.
  y_not_glb_0_b <- sample(y_not_glb_0, replace = TRUE)
  y_glb_0_b <- sample(y_glb_0, replace = TRUE)
  
  ## Compute and store median difference.
  med_diff_0_b[b] <- median(y_glb_0_b) - median(y_not_glb_0_b)
}

## Because we are testing against a *two-sided* alternative hypothesis, the p-value is 
## defined as the probability of seing a median difference *as or more extreme* as the 
## median difference we originally observed. We can estimate the p-value as follows. Note
## that the p-value is *not* < 0.05, whereas we expected it to be. The relationship 
## between confidence intervals and hypothesis tests (e.g., p-value < 0.05 if the null 
## hypothesized parameter value is not contained in a 95% confidence interval) are 
## sometimes distorted in practice. In this case, there are only 19 unique values for 
## HHINC, and this substantially affects the bootstrap sampling distributions we obtain.
sum(abs(med_diff_0_b) >= abs(med_diff_obs)) / B
mean(abs(med_diff_0_b) >= abs(med_diff_obs))

####
#### Association between RESPONDENT_YRSED and PARTNER_YRSED.
####

## First get rid of partners with missing values.
ii_no_NA <- apply(dta, 1, function(x) { all(!is.na(x)) })
dta_no_NA <- dta[ii_no_NA, ]

x <- dta_no_NA$PARTNER_YRSED
y <- dta_no_NA$RESPONDENT_YRSED
n <- length(x)

## Scatterplot with smoother overlaid.
plot(x, y, xlab = "PARTNER_YRSED", ylab = "RESPONDENT_YRSED")
lines(lowess(x, y))

##
## Correlation.
##

r <- cor(x, y)
r

## Match manually.
x_bar <- mean(x)
y_bar <- mean(y)
s_x <- sd(x)
s_y <- sd(y)
ss_x <- s_x ^ 2 * (n - 1)
ss_y <- s_y ^ 2 * (n - 1)

r_manual <- sum((x - x_bar) * (y - y_bar)) / (sqrt(ss_x) * sqrt(ss_y))
r_manual

##
## Linear regression of RESPONDENT_YRSED (response) vs. PARTNER_YRSED (predictor).
##

## Fit model.
fit <- lm(y ~ x)
summary(fit)
abline(fit, col = "blue")

## Intercept is estimated RESPONDENT_YRSED when PARTNER_YRSED = 0.
cf <- summary(fit)$coef
points(0, cf[1, 1], pch = 20, col = "red", cex = 2)
text(0, 5, expression(hat(beta)[0]))

## Slope is estimated mean change in RESPONDENT_YRSED associated with a 1-unit increase 
## in PARTNER_YRSED. Five times the slope is the estimated mean change in RESPONDENT_YRSED
## associated with a *5*-unit increase in PARTNER_YRSED.
lines(c(0, 5), c(cf[1, 1], cf[1, 1]), lty = 2, col = "red")
lines(c(5, 5), c(cf[1, 1], cf[1, 1] + 5 * cf[2, 1]), lty = 2, col = "red")
text(6, 9, expression(paste("5 x ", hat(beta)[1], sep = "")))

## Relationship between slope and correlation estimates.
r * (s_y / s_x)
cf[2, 1]

##
## Check model assumptions.
##

ee <- summary(fit)$resid

## Is there reasonably a linear association between x and y? There is some curvature to 
## the residual plot. This is partially driven by the presence of an "outlier" on the low 
## end of PARTNER_YRSED. The residual plot can also be used to gauge the assumption of 
## constant residual variance.  
par(mfrow = c(1, 2))
plot(x, y, xlab = "PARTNER_YRSED", ylab = "RESPONDENT_YRSED")
lines(lowess(x, y))
abline(fit, col = "blue")
plot(x, ee, xlab = "PARTNER_YRSED", ylab = "residuals")
lines(lowess(x, ee))
abline(0, 0, lty = 2, col = "red")

## Are the residuals reasonably Normal in distribution?
qqnorm(ee)
qqline(ee)

## Testing H_0: beta_1 = 0 vs. H_a: beta_1 != 0 (two-sided).
t_stat <- cf[2, 1] / cf[2, 2]
p_val <- 2 * (1 - pt(abs(t_stat), n - 2))

## A 95% confidence interval for beta_1.
ci <- cf[2, 1] + c(-1, 1) * qt(0.975, n - 2) * cf[2, 2]

## Report a 95% confidence interval for the mean of RESPONDENT_YRSED among those with 
## PARTNER_YRSED = 15.
predict(fit, newdata = data.frame(x = 15), interval = "confidence")

## Report a 95% prediction interval for the value of RESPONDENT_YRSED for whom 
## PARTNER_YRSED = 15.
predict(fit, newdata = data.frame(x = 15), interval = "prediction")

