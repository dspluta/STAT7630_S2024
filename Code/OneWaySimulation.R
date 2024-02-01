#####
#####  Set the random number generator so that we all get the same results
#####

set.seed(111)


#####
#####  Set the simulation parameters
#####

nSims <- 5000				## The total number of simulations we intend to run
nGrp <- c(20,40,60)			## The number of observations per group
grpMeans <- c(1,1,1)		## The true mean of the distribution each group is sampled from
grpVar <- c(1,2,3)			## The true variance of each observation from each of the three groups		

#####
#####  Initialize the matrix that will hold the results of our tests 
#####  (test statistics will be in column 1; corresponding p-values will be in column 2) 
#####

rslt <- matrix( NA, nrow=nSims, ncol=2 )


#####
#####  Run the simulation nSims times 
#####
for( i in 1:nSims ){
	
	## Sample the data for each group, depending upon the specified mean and variance
	y <- rnorm(n=sum(nGrp), mean=rep( grpMeans, times=nGrp ), sd=rep( sqrt(grpVar), times=nGrp ) )  
	group <- rep(1:3, times=nGrp )
	simData <- as.data.frame( cbind( group, y ) )
		
	## Fit the one-way ANOVA model, then extract the F-statistic and the p-value
	fit <- aov( y ~ factor( group ), data=simData )
	Fstat <- summary( fit )[[1]][1,4]
	pVal <- summary( fit )[[1]][1,5]
	
	##  Store the results of simulation loop i in row i of the results matrix
	rslt[i,] <- c( Fstat, pVal )
	}



#####
#####  Calculate the observed type I error rate
#####
  
nReject <- sum( rslt[,2] < .05 )	
simType1Error <- nReject / nSims
simType1Error	


#####
#####  Plot the observed distribution of F-statistics vs. the theoretical distribution
#####

hist( rslt[,1], xlab="Simulated F-statistics", prob=T, ylim=c(0,1) )
lines( seq( 0, 10, by=.1 ), df( seq( 0, 10, by=.1 ), df1=3-1, df2=sum(nGrp ) - 3 ) )



