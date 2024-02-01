library(ggplot2)

dat <- read.delim("https://raw.githubusercontent.com/dspluta/STAT120C/master/Data/fev.txt")

##########

# As an illustration of linear regression, 
# we will examine the relationship of height 
# and age in the FEV data.

## Univariate Summaries and Plots

summary(dat$age)
summary(dat$height)

hist(dat$age, breaks = 30)
hist(dat$height, breaks = 30)

## Scatterplot of age and height

ggplot(dat) + 
  geom_point(aes(x = age, y = height)) + 
  ggtitle("How does height relate to age in adolescents?")

## Find beta_0, beta_1 "manually"

mean_height <- mean(dat$height)

mean_age <- mean(dat$age)

beta_1_hat <- sum(dat$height * (dat$age - mean_age)) / sum((dat$age - mean_age)^2)

beta_0_hat <- mean_height - beta_1_hat * mean_age

#######################

## Part 2

Y <- dat$height
X <- dat$age
n <- nrow(dat)

### Compute fitted Y-values

Y_hat <- beta_0_hat + beta_1_hat * X

### Plot fitted line

plot(X, Y)
lines(X, Y_hat)

### Compute residuals and estimate s

resids <- Y - Y_hat
s <- sqrt(sum(resids^2) / (n - 2))

### Compute std error of beta_1_hat, beta_0_hat, t stats, and P-vals

s_beta_0_hat <- sqrt(s^2 * sum(X^2) / (n * sum((X - mean(X))^2)))
t_beta_0 <- beta_0_hat / s_beta_0_hat
P_beta_0 <- pt(t_beta_0, n - 2, lower.tail = FALSE)

s_beta_1_hat <- sqrt(s^2 / sum((X - mean(X))^2))
t_beta_1 <- beta_1_hat / s_beta_1_hat
P_beta_1 <- pt(t_beta_1, n - 2, lower.tail = FALSE)

### Compute R^2 and F-stat

SSTOT <- sum((Y - mean(Y))^2)
SSE <- (n - 2) * s^2
SSR <- SSTOT - SSE

R2 <- 1 - SSE / SSTOT

F_stat <- SSR / (SSE / (n - 2))

###########################

### Print results

cat("Est. beta_0", beta_0_hat, "\n")
cat("Est. beta_1", beta_1_hat, "\n\n")

cat("Std. Error beta_0", s_beta_0_hat, "\n")
cat("Std. Error beta_1", s_beta_1_hat, "\n\n")

cat("t-stat beta_0", t_beta_0, "\n")
cat("t-stat beta_1", t_beta_1, "\n\n")

cat("Multiple R^2", R2, "\n")
cat("F statistic", F_stat, "\n")

###########################

## Compare to results from built-in R function

fit <- lm(height ~ age, data = dat)
summary(fit)

############################

## DIAGNOSTICS

### We can examine the residuals to determine if some of the 
### linear regression model assumptions are satisfied.

#### 1. Errors are independent. (Discussed later)
#### 2. Errors are normally distributed. (QQ-plot)
#### 3. Errors have constant variance. (Residual plots)

### Are the errors normally distributed?

par(mfrow = c(1, 2))
qqnorm(resids)
qqline(resids)

hist(resids, breaks = 20)

### Do the errors have constant variance?
### Are X and Y linearly related?

plot(Y_hat, resids)
plot(Y, resids)

par(mfrow = c(3, 2))
plot(fit)
plot(Y_hat, sqrt(abs(resids / s)))

### Plot prediction interval

dat_plt <- data.frame(X = X, Y = Y, Y_hat = Y_hat, pred_lwr = NA, pred_upr = NA, conf_lwr = NA, conf_upr = NA)
dat_plt$pred_lwr <- Y_hat - qt(0.975, n - 2) * sqrt(s^2 * (1 + 1 / n + (X - mean(X))^2 / sum((X - mean(X))^2)))
dat_plt$pred_upr <- Y_hat + qt(0.975, n - 2) * sqrt(s^2 * (1 + 1 / n + (X - mean(X))^2 / sum((X - mean(X))^2)))
dat_plt$conf_lwr <- Y_hat - qt(0.975, n - 2) * sqrt(s^2 * (1 / n + (X - mean(X))^2 / sum((X - mean(X))^2)))
dat_plt$conf_upr <- Y_hat + qt(0.975, n - 2) * sqrt(s^2 * (1 / n + (X - mean(X))^2 / sum((X - mean(X))^2)))


ggplot(dat_plt) + 
  geom_line(aes(x = X, y = Y_hat)) + 
  geom_point(aes(x = X, y = Y)) +
  geom_line(aes(x = X, y = pred_lwr), color = "red") + 
  geom_line(aes(x = X, y = pred_upr), color = "red") + 
  geom_line(aes(x = X, y = conf_lwr), color = "blue") + 
  geom_line(aes(x = X, y = conf_upr), color = "blue")


ggplot(dat_plt) + 
  geom_smooth(aes(x = X, y = Y), method = "lm") + 
  geom_point(aes(x = X, y = Y))
