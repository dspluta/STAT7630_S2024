library(ggplot2)
library(dplyr)

# Model Space

# y = beta * x + eps
# eps ~ N(0, sigma2)

generate_data <- function(n, beta, sigma) {
  x <- rnorm(n, 0, 1)
  y <- beta * x + rnorm(n, 0, sigma)
  dat <- data.frame(x = x, y = y)
  return(dat)
}

# reps: number of simulations to run at each setting

power_analysis <- function(reps, n, beta, sigma) {
  test_results <- rep(NA, reps)
  
  for (rep in 1:reps) {
    dat <- generate_data(n, beta, sigma)
    fit <- lm(y ~ x, data = dat)
    P_val <- summary(fit)$coefficients[2,4]
    test_results[rep] <- (P_val < 0.05)
  }
  
  pow <- mean(test_results)
  return(pow)
}

dat_sim <- data.frame(power = NA, n = c(10, 20, 50, 100), beta = -0.5, sigma = 1)

for (i in 1:nrow(dat_sim)) {
  dat_sim$power[i] <- power_analysis(20, dat_sim$n[i], dat_sim$beta[i], dat_sim$sigma[i])
}
dat_sim

ggplot(dat_sim) + 
  geom_line(aes(x = n, y = power)) + 
  geom_point(aes(x = n, y = power))

#####


