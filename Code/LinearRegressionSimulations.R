
## Scenario 1: Normal, independent, constant variance, 
set.seed(1234)
n <- 20
X <- rnorm(n, 0, 1)
beta_0 <- 0
beta_1 <- 0.5
sigma <- 1

eps <- rnorm(n, 0, sigma)
Y <- beta_0 + beta_1 * X + eps

plot(X, Y, main = "Observed Data")

fit <- lm(Y ~ X)
summary(fit)
plot(fitted(fit), resid(fit), main = "Residuals by Fitted Values")

qqnorm(resid(fit))
qqline(resid(fit))

## Scenario 2: Normal, independent, constant variance, larger n
set.seed(1234)
n <- 100
X <- rnorm(n, 0, 1)
beta_0 <- 0
beta_1 <- 0.5
sigma <- 1

eps <- rnorm(n, 0, sigma)
Y <- beta_0 + beta_1 * X + eps

plot(X, Y)

fit <- lm(Y ~ X)
summary(fit)
plot(fitted(fit), resid(fit))

qqnorm(resid(fit))
qqline(resid(fit))


## Scenario 3: Non-linear, Normal, independent, constant variance, larger n
set.seed(1234)
n <- 100
X <- rnorm(n, 0, 1)
beta_0 <- 0
beta_1 <- -1
sigma <- 1

eps <- rnorm(n, 0, sigma)
Y <- beta_0 + beta_1 * X^2 + eps

plot(X, Y)

fit <- lm(Y ~ X)
summary(fit)

plot(X, Y)
lines(X, fitted(fit))

plot(fitted(fit), resid(fit))
plot(X, resid(fit))


qqnorm(resid(fit))
qqline(resid(fit))


## Scenario 4: Normal, independent, NON-constant variance
set.seed(1234)
n <- 200
X <- rnorm(n, 6, 2)
beta_0 <- 3
beta_1 <- 2
sigma <- 0.7 * sqrt(X * beta_1)

eps <- rnorm(n, 0, sigma)
Y <- beta_0 + beta_1 * X + eps

plot(X, Y)

fit <- lm(Y ~ X)
summary(fit)
plot(fitted(fit), resid(fit))

qqnorm(resid(fit))
qqline(resid(fit))


## Scenario 5: Normal, independent, NON-constant variance
set.seed(1234)
n <- 500
X <- rnorm(n, 6, 2)
beta_0 <- 3
beta_1 <- 2
sigma <- 0.7 * sqrt(X * beta_1)

eps <- rnorm(n, 0, sigma)
Y <- beta_0 + beta_1 * X + eps

plot(X, Y)

fit <- lm(Y ~ X)
summary(fit)
plot(fitted(fit), resid(fit))

qqnorm(resid(fit))
qqline(resid(fit))


## Scenario 6: NON-Normal, independent, constant variance
set.seed(1234)
n <- 20
X <- rnorm(n, 0, 1)
beta_0 <- 0
beta_1 <- 1

eps <- runif(n, -2, 2)
Y <- beta_0 + beta_1 * X + eps

plot(X, Y)

fit <- lm(Y ~ X)
summary(fit)
plot(fitted(fit), resid(fit))

qqnorm(resid(fit))
qqline(resid(fit))

## Scenario 7: NON-Normal, independent, constant variance
set.seed(1234)
n <- 20
X <- rnorm(n, 0, 1)
beta_0 <- 0
beta_1 <- 1

eps <- runif(n, -2, 2)
Y <- beta_0 + beta_1 * X + eps

plot(X, Y)

fit <- lm(Y ~ X)
summary(fit)
plot(fitted(fit), resid(fit))

qqnorm(resid(fit))
qqline(resid(fit))


## Scenario 8: NON-Normal, independent, NON-constant variance
set.seed(1234)
n <- 100
X <- rnorm(n, 2, 1)
beta_0 <- 0
beta_1 <- 1

Y <- rpois(n, exp(beta_0 + beta_1 * X))

plot(X, Y)

fit <- lm(Y ~ X)
summary(fit)
plot(fitted(fit), resid(fit))

qqnorm(resid(fit))
qqline(resid(fit))



