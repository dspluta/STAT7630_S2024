
set.seed(1234)
IDS <- 1:100
n.obs <- length(IDS)
male <- as.logical(rbinom(n.obs, 1, prob = .504))
n.male <- sum(male)
n.female <- n.obs - n.male
heights <- ifelse(male,
                  rnorm(n.male, mean = 69.3, sd = 3.33),
                  rnorm(n.female, mean = 63.8, sd = 3))
weights <- ifelse(male, 
                  rnorm(n.male, mean=195, sd = 15), 
                  rnorm(n.female, mean=165, sd=20))

start <- Sys.time()
for(i in 1:1000){
  if(!(i %% 10)){
    message(paste('Step: ', i, sep=""))
  }
}
stop.time <- Sys.time()
print(stop.time - start)


test.start <- Sys.time()
storage <- c()
for(i in 1:100000){
  storage <- c(storage, 10)
}
test.stop <- Sys.time()
print(test.stop - test.start)



test.start <- Sys.time()
storage <- rep(NA, 100000)
for(i in 1:100000){
  storage[i] <- 10
}
test.stop <- Sys.time()
print(test.stop - test.start)
