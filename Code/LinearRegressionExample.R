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

## Fit using built-in R function 

fit <- lm(height ~ age, data = dat)
summary(fit)

## Diagnostic Plots

plot(fit)

## Plot best fit line

ggplot(dat) + 
  geom_point(aes(x = age, y = height)) + 
  geom_smooth(aes(x = age, y = height), method = "lm") + 
  ggtitle("How does height relate to age in adolescents?")

#######################

## Questions

# 1. What is the interpretation of beta_1_hat

# 2. What is the interpretation of beta_0_hat?

# 3. Does it appear that any model assumptions are violated?

