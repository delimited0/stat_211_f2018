####
#### Data on 261 undergraduate students in an introductory statistics class. Includes 
#### variables for classification (U1, U2, ..., U5), gender, average exam score, average 
#### homework score, number of absences, and final letter grade.
####

dta <- read.csv("stat_class.csv")
n <- nrow(dta)

##
## Conditional expectation.
##

## Among these 261 students, what is the expected grade on the final exam, given 10 or 
## more absences?
print(f_EXAM_given_ABSENT_geq_10 <- with(dta, prop.table(table(EXAM[ABSENT >= 10]))))
sum(f_EXAM_given_ABSENT_geq_10)

sum(as.numeric(names(f_EXAM_given_ABSENT_geq_10)) * f_EXAM_given_ABSENT_geq_10)
with(dta, mean(EXAM[ABSENT >= 10], na.rm = TRUE))

## Compare to expected grade on final exam, given fewer than 10 absences.
print(f_EXAM_given_ABSENT_lt_10 <- with(dta, prop.table(table(EXAM[ABSENT < 10]))))
sum(f_EXAM_given_ABSENT_lt_10)

sum(as.numeric(names(f_EXAM_given_ABSENT_lt_10)) * f_EXAM_given_ABSENT_lt_10)
with(dta, mean(EXAM[ABSENT < 10], na.rm = TRUE))

##
## Inference on mean difference in final exam grade, comparing males to females. Report 
## 95% confidence intervals and test H_0: mean difference = 0 against H_a: mean 
## difference != 0 at significance level 0.05.
##

## For simplicity, first remove the students with NA for final exam.
ii <- with(dta, !is.na(EXAM))
dta_no_NA <- dta[ii, ]

## Observed statistic value. Also estimate its standard deviation.
EXAM_F <- with(dta_no_NA, EXAM[SEX == "M"])
EXAM_M <- with(dta_no_NA, EXAM[SEX == "F"])
n_F <- length(EXAM_F)
n_M <- length(EXAM_M)
mean_diff_obs <- mean(EXAM_M) - mean(EXAM_F)
mean_diff_s <- sqrt(var(EXAM_M) / n_M + var(EXAM_F) / n_F)

## Transform data to force H_0 true, for use in bootstrap calculation of p-value.
EXAM_F_0 <- EXAM_F - mean(EXAM_F)
EXAM_M_0 <- EXAM_M - mean(EXAM_M)

## Bootstrap 95% CI and p-value.
B <- 500
mean_diff_b <- mean_diff_0_b <- numeric(B)
for(b in 1:B) {
  ## Sample from untransformed data for CI.
  EXAM_F_b <- sample(EXAM_F, replace = TRUE)
  EXAM_M_b <- sample(EXAM_M, replace = TRUE)
  mean_diff_b[b] <- mean(EXAM_M_b) - mean(EXAM_F_b)
  
  ## Sample from transformed data for p-value.
  EXAM_F_0_b <- sample(EXAM_F_0, replace = TRUE)
  EXAM_M_0_b <- sample(EXAM_M_0, replace = TRUE)
  mean_diff_0_b[b] <- mean(EXAM_M_0_b) - mean(EXAM_F_0_b)
}
ci_boot <- quantile(mean_diff_b, c(0.025, 0.975))
pval_boot <- sum(abs(mean_diff_0_b) >= abs(mean_diff_obs)) / B

## CLT 95% CI and p-value.
ci_clt <- qnorm(c(0.025, 0.975), mean_diff_obs, mean_diff_s)
pval_clt <- 2 * (1 - pnorm(abs(mean_diff_obs), 0, mean_diff_s))

##
## Bootstrap-based inference on the median ratio (median of female exam scores divided by 
## median of male exam scores). We will construct a 95% confidence interval.
## 

B <- 1000
med_rat_b <- numeric(B)
for(b in 1:B) {
  ## Sample with replacement.
  EXAM_F_b <- sample(EXAM_F, replace = TRUE)
  EXAM_M_b <- sample(EXAM_M, replace = TRUE)
  med_rat_b[b] <- median(EXAM_F_b) / median(EXAM_M_b)
}
ci_boot <- quantile(med_rat_b, c(0.025, 0.975))

##
## Large-sample inference on p = Pr(randomly-selected student gets A or B); a 95% 
## confidence interval and a test of H_0: p = 2/3 vs. H_a: p != 2/3.
##

x_AB <- with(dta, GRADE == "A" | GRADE == "B")
p_hat_AB <- sum(x_AB) / n
p_0 <- 2 / 3
sd_p_hat_AB <- sqrt((p_hat_AB * (1 - p_hat_AB)) / n)
sd_p_0 <- sqrt((p_0 * (1 - p_0)) / n)

## A 95% confidence interval.
ci_p_AB <- p_hat_AB + c(-1, 1) * qnorm(0.975, 0, 1) * sd_p_hat_AB

## A p-value for testing H_0: p = 2/3 vs. H_a: p != 2/3.
test_stat <- (p_hat_AB - p_0) / sd_p_0
pval_p_AB <- 2 * (1 - pnorm(abs(test_stat), 0, 1))

## 
## Large-sample inference on difference p_M - p_F.
##

x_AB_M <- with(dta, SEX == "M" & (GRADE == "A" | GRADE == "B"))
x_AB_F <- with(dta, SEX == "F" & (GRADE == "A" | GRADE == "B"))
n_M <- sum(dta$SEX == "M")
n_F <- sum(dta$SEX == "F")
p_hat_AB_M <- sum(x_AB_M) / n_M
p_hat_AB_F <- sum(x_AB_F) / n_F
sd_p_diff_AB <- sqrt((p_hat_AB_M * (1 - p_hat_AB_M)) / n_M + 
  (p_hat_AB_F * (1 - p_hat_AB_F)) / n_F)
  
## A 99% confidence interval.
ci_p_diff_AB <- p_hat_AB_M - p_hat_AB_F + c(-1, 1) * qnorm(0.995) * sd_p_diff_AB

## A p-value for testing H_0: p_M - p_F = 0 vs. H_a: p_M - p_F != 0.
test_stat <- (p_hat_AB_M - p_hat_AB_F) / sd_p_diff_AB
p_val_p_diff_AB <- 2 * (1 - pnorm(abs(test_stat)))

##
## Large-sample one-sided 99% interval (upper bound) for mean score among females and a 
## test of H_0: mu_F = 70 vs. H_a: mu_F > 70.
##

x_bar_F <- mean(EXAM_F)
s_F <- sd(EXAM_F)
ci_F <- qnorm(0.99, x_bar_F, s_F / sqrt(n_F))
pval_F <- 1 - pnorm(x_bar_F, 70, s_F / sqrt(n_F))

##
## Linear regression of exam score on number of absences.
##

## Start with a scatterplot and smoother. The relationship looks reasonably linear, 
## although there is some suggestion of curvature toward the right side of the plot.
with(dta_no_NA, plot(ABSENT, EXAM))
with(dta_no_NA, lines(smooth.spline(ABSENT, EXAM, df = 3), col = "red"))

## Linear regression model. There is a statistically significant linear association 
## between EXAM and ABSENT.
fit <- lm(EXAM ~ ABSENT, data = dta_no_NA)
abline(coef(fit), col = "blue")
summary(fit)

## Residuals plot to investigate linearity and look for potential outliers. We again see 
## evidence of subtle curvature.
res <- residuals(fit)
with(dta_no_NA, plot(ABSENT, res))
abline(0, 0, lty = 2)
with(dta_no_NA, lines(smooth.spline(ABSENT, res), col = "red"))

## QQ plot to investigate Normality of residuals. Normality appears suspect. However, 
## with a large sample size, it does not matter much.
qqnorm(res)
qqline(res)

## 95% confidence interval for mean response when 5 absences, compared to a 95% 
## prediction interval for an observed response when 5 absences. Prediction intervals are 
## always wider than their confidence interval counterparts.
predict(fit, newdata = data.frame(ABSENT = 5), interval = "confidence")
predict(fit, newdata = data.frame(ABSENT = 5), interval = "prediction")

##
## Now include gender in the model, allowing for different intercepts and slopes for 
## each gender.
##

## This fits the model with ABSENT, SEX, and their 'interaction'. We do not have 
## sufficient evidence to claim a non-zero interaction, suggesting that gender does not 
## play a significant role in the relationship between exam score and number of absences.
fit_int <- lm(EXAM ~ ABSENT + SEX + ABSENT * SEX, data = dta_no_NA)
summary(fit_int)

## Re-draw scatterplot, now adding our estimated gender-specific lines.
cf_int <- coef(fit_int)
with(dta_no_NA, plot(ABSENT, EXAM))
with(dta_no_NA, points(ABSENT[SEX == "F"], EXAM[SEX == "F"], pch = 20, 
  col = "lightpink"))
with(dta_no_NA, points(ABSENT[SEX == "M"], EXAM[SEX == "M"], pch = 20, 
  col = "lightblue"))
abline(cf_int[1], cf_int[2], col = "red", lwd = 2)
abline(cf_int[1] + cf_int[3], cf_int[2] + cf_int[4], col = "blue", lwd = 2)
legend(10, 105, legend = c("F", "M"), col = c("red", "blue"), lwd = rep(2, 2), bty = "n")

