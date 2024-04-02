library(ggplot2)
library(dplyr)
dat_raw <- read.csv("https://raw.githubusercontent.com/dspluta/STAT7630_S2024/main/Data/Real_estate_Data.csv")

dat_raw
dat <- dat_raw
colnames(dat) <- c("NO", "date", "age", "MRT_dist", "num_conv", "latitude", "longitude", "price")
dat <- dat %>% select(-NO)

head(dat)

# Question of Interest:
## How does date affect unit housing price, 
# adjusted for age of the house and location of the house?

# Proposed Model:
# PRICE ~ DATE + AGE + MRT_dist + latitude + longitude

# Exploratory Data Analysis

## Univariate Plots

hist(dat$date)
hist(dat$age)
hist(dat$MRT_dist, breaks = 50)
hist(dat$num_conv)
hist(dat$latitude)
hist(dat$longitude)
hist(dat$price)

## Bivariate Plots

plot(dat$date, dat$price)
plot(dat$age, dat$price)
plot(dat$MRT_dist, dat$price)
plot(dat$num_conv, dat$price)

## Trivariate plot

dat$old <- dat$age > 20

ggplot(dat) + 
  geom_point(aes(x = date, y = price, color = old)) + 
  geom_smooth(aes(x = date, y = price, color = old), method = "lm")

## Initial Modeling

fit1 <- lm(price ~ date, data = dat)
summary(fit1)

fit2 <- lm(price ~ MRT_dist, data = dat)
summary(fit2)

fit3 <- lm(price ~ age, data = dat)
summary(fit3)

fit4 <- lm(price ~ latitude, data = dat)
summary(fit4)

fit5 <- lm(price ~ longitude, data = dat)
summary(fit5)

# Correlation plots/matrix

head(dat)
cor(dat[, 1:6])

## Primary Modeling

fit6 <- lm(price ~ date + age, data = dat)
summary(fit6)

fit7 <- lm(price ~ date + MRT_dist, data = dat)
summary(fit7)

fit8 <- lm(price ~ date + age + MRT_dist, data = dat)
summary(fit8)

fit9 <- lm(price ~ date + age + MRT_dist + latitude + longitude, data = dat)
summary(fit9)

## Diagnostics

plot(fit9)

### Identify the quadratic component

plot(dat$age, fitted(fit9))
plot(dat$date, fitted(fit9))
plot(dat$MRT_dist, fitted(fit9))
plot(dat$latitude, fitted(fit9))
plot(dat$longitude, fitted(fit9))

### Refit with quadratic component

fit10 <- lm(price ~ date + age + MRT_dist + latitude + I(latitude^2) + longitude, data = dat)
summary(fit10)

plot(fit10)
summary(fit1)

#### Goodness of fit tests

anova(fit1, fit10)
anova(fit9, fit10)

anova(fit9, fit10)

## Exploratory modeling

fit <- lm(price ~ age + MRT_dist + latitude + I(latitude^2) + longitude, data = dat)
summary(fit)

anova(fit, fit10)

fit <- lm(price ~ date + age + MRT_dist + latitude + I(latitude^2) + longitude + num_conv, data = dat)
anova(fit10, fit)
