################################################################################
#
# UCI Statistics Bootcamp 2016
#   - Demonstrating the Birthday Problem via Simulation
#   - Developed by: Brian Vegetabile
#
################################################################################

# Setting a Seed for reproducibility
set.seed(2015)

# Setting up our sequence of birthdays
birthdays <- seq(1,365)

# Setting up the number of simulation runs for each n
nsims <- 1000

# Creating a variable to store our birthdays
p_birthday <- c()

# Creating a timer <- diagnostic technique
tstart <- Sys.time()

# Iterating through days 2 to 100... Let's see how our 
# probabilities look on the first 100 days
for (d in 2:100){
  draws <- c()
  for(i in 1:nsims){
    draws <- rbind(draws, sample(birthdays,d,replace = TRUE))
  }
  p_birthday <- c(p_birthday, 
                  sum(unlist(lapply(apply(draws, 1, unique),length)) != d) / nsims)
  if (d %% 10 == FALSE){
    message(paste('Finished Birthday: ', d))  
  }
}
tend <- Sys.time() - tstart
message(paste('Elapsed time: ', tend))
par(mfrow=c(1,1))
plot(seq(2,100), p_birthday, pch=19, col=rgb(0,0.5,1, 0.6), axes=FALSE,
     xlab='Number of People', 
     ylab='Probability of two people with the same birthday')
abline(h=0.5)
axis(1,at =seq(0,100,5))
axis(2,at =seq(0,1,0.1))

# Let's see how we do against the truth!
z <- seq(2,100)
true_prob <- 1 - exp(lfactorial(365) - (z*log(365) + lfactorial(365 - z)))

par(mfrow=c(1,1))
plot(seq(2,100), p_birthday, pch=19, col=rgb(0,0.5,1, 0.6), axes=FALSE,
     xlab='Number of People', 
     ylab='Probability of two people with the same birthday')
lines(z, true_prob, lty=2, lwd=3, col=rgb(1,0,0,0.9))
axis(1,at =seq(0,100,5))
axis(2,at =seq(0,1,0.1))
legend(80,0.15, c('Simulated', 'Truth'), lty=c(NaN,2), pch=c(19,NaN),
       col=c(rgb(0,0.5,1, 0.6), col=rgb(1,0,0,0.9)))