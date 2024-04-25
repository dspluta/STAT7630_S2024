#####	Collection of useful functions for STAT 7630
#####
#####	Author: D. Pluta
#####	Course: Stat 7630, Spring 2024
##
##

#####	Helper function to return vector for binary test
##
ifelse1 <- function (test, yes, no){
  if (test) yes
  else no
}

##
#####  Function to produce CIs for LM parameters (or exponentiated parameters)
##
lmCI <- function( model, expcoef=FALSE, robust=FALSE ){
	coef <- summary( model )$coef[,1]
	se <- ifelse1( robust, robust.se.lm(model)[,2], summary( model )$coef[,2] )
	tvalue <- coef / se
	pvalue <- 2*(1-pt(abs(tvalue), model$df.residual))
	if( expcoef ){
		ci95.lo <- exp( coef - qt(.975, model$df.residual) * se )
		ci95.hi <- exp( coef + qt(.975, model$df.residual) * se )
		est <- exp( coef )
	}
	else{
		ci95.lo <- coef - qt(.975, model$df.residual) * se
		ci95.hi <- coef + qt(.975, model$df.residual) * se
		est <- coef
	}
	rslt <- round( cbind( est, ci95.lo, ci95.hi, tvalue, pvalue ), 4 )
	colnames( rslt ) <- ifelse1( 	robust,
					c("Est", "robust ci95.lo", "robust ci95.hi", "robust t value", "robust Pr(>|t|)"),
					c("Est", "ci95.lo", "ci95.hi", "t value", "Pr(>|t|)") )
	colnames( rslt )[1] <- ifelse( expcoef, "exp( Est )", "Est" )
	rslt
	}

##
#####	Function to plot dfBetas resulting from a lm() object
##
plot.dfbeta <- function( dfbeta.fit, labels ){
	#oldmar <- par()$mar
	#par( mar=c(5, 4, 3+.75*dim(dfbeta.fit)[2], 2) + 0.1 )
	plot( c(1,dim(dfbeta.fit)[1]*1.1), range(dfbeta.fit)*1.1, xlab="Obersvation", ylab="dfBeta", type="n" )
	for( i in 2:dim(dfbeta.fit)[2] ){
			points( 1:dim(dfbeta.fit)[1], dfbeta.fit[,i], col=i )
			text( 1:dim(dfbeta.fit)[1]+1, dfbeta.fit[,i]+.1, labels=labels, col=i )
			mtext( colnames( dfbeta.fit )[i], col=i, line=-1+i )
	}
	abline( h=c(-1,1)*(2/sqrt(dim(dfbeta.fit)[1])), col="red", lwd=2 )
	#par( mar=oldmar )
	}


##
#####	Function to test linear contrasts
##
LinContr.mfit <- function( contr.names, contr.coef, model ){
	beta.hat <- as.vector( t( summary( model )$coefficients[,1] ) )
	se <- as.vector( t( summary( model )$coefficients[,2] ) )
	cov.beta <- vcov( model )

	contr.index <- is.element( dimnames( cov.beta )[[1]], contr.names )
	beta.hat <- beta.hat[ contr.index ]
	cov.beta <- cov.beta[ contr.index,contr.index ]
	est <- contr.coef %*% beta.hat
	se.est <- sqrt( contr.coef %*% cov.beta %*% contr.coef )
	zStat <- est / se.est
	pVal <- 2*pnorm( abs(zStat), lower.tail=FALSE )
	ci95.lo <- est - qnorm(.975)*se.est
	ci95.hi <- est + qnorm(.975)*se.est
	cat( "\nTest of H_0: " )
	for( i in 1:(length( contr.names )-1) ){
		cat( contr.coef[i], "*", contr.names[i], "+ " )
	}
	cat( contr.coef[i+1], "*", contr.names[i+1], "= 0 :\n\n" )
	rslt <- data.frame(se.est, zStat, pVal, ci95.lo, ci95.hi )
	round( rslt, 3 )
}

##
#####
#####  	The following three function compute prediction
#####	optimality criteria for linear regression models
#####
#####	*Notes: This includes polynomial fits, b-splines and natural splines
#####
##
##
#####		Computation of the CV or k-fold CV statistic for
#####   	a lm fit (squared error loss)
##
cv.lm <- function( lmFit, data, K="n", GCV=FALSE ){
  y <- model.frame(lmFit)[,1]
  yhat <- lmFit$fitted
  n <- length(yhat)
  lmFormula <- formula( lmFit )
  
  Xmat <- model.matrix(lmFit)
  p <- dim( Xmat )[2]
  H <- Xmat %*% solve( t(Xmat)%*%Xmat ) %*% t(Xmat)
  if( GCV==FALSE ) cv <- mean( ( (y-yhat) / (1-diag(H)) )^2 )
  else cv <- mean( ( (y-yhat) / (1-sum(diag(H))/n) )^2 )
  
  cv.k <- NULL
  if( K !="n" ) {
    ord <- sample(1:n, n)
    y <- y[ord]
    data <- data[ord,]
    rss.k <- rep(NA,n)
    for( i in 1:K ){
      keep <- 1:ceiling(n/K) + (i-1)*ceiling(n/K)
      if( max(keep) > n ) keep <- min(keep):n
      fit.k <- lm( lmFormula, data=data[!is.element( 1:n, keep ),] )
      yhat.k <- predict( fit.k, newdata=data[keep,] )
      rss.k[keep] <- (y[keep] - yhat.k)^2
    }
    cv.k <- mean( rss.k )
  }
  return( c(cv, cv.k) )
}

##
#####		Computation of the bootstrap estimate of MSE for a lm() fit
##
bsMSE.lm <- function( lmFit, data, B=1000 ){
  y <- model.frame(lmFit)[,1]
  yhat <- lmFit$fitted
  n <- length(yhat)
  lmFormula <- formula( lmFit )
  
  mse.bs <- rep(NA,B)
  rss.1out <- matrix( NA, nrow=B, ncol=n )
  for( i in 1:B ){
    keep <- sample( 1:n, size=n, replace=TRUE )
    fit.b <- lm( lmFormula, data=data[keep,] )
    
    ##	Simple bootstrap estimate of MSE
    mse.bs[i] <- mean( (y - predict(fit.b, newdata=data ))^2 )
    
    ##	Leave-one-out bootstrap
    rss.1out[i,] <- ifelse( is.element( 1:n, keep ), NA,
                            ( y - predict(fit.b, newdata=data ) )^2 )
    
  }
  bsMSE <- mean(mse.bs)
  bsLve1out <- mean( apply( rss.1out, 2, mean, na.rm=TRUE ) )
  bs.632 <- .368*bsMSE + .632*bsLve1out
  return( cbind( bsMSE, bsLve1out, bs.632 ) )
}

##
#####		Computation of common prediction error estimates for a lm() fit
##
lm.predcrit <- function( lmFit, data, GCV="FALSE", K="n",
                         B=1000, boot=FALSE, sigmaSq="calculate" ){
  if( sigmaSq=="calculate" ) sigmaSq <- summary(lmFit)$sigma^2
  rss <- sum( lmFit$residuals^2 )
  n <- length(lmFit$fitted)
  p <- summary( lmFit )$df[1]
  loglik <- (-1/2) * (n*log(2*pi*sigmaSq) + rss / sigmaSq)
  
  mse <- rss / n
  Cp <- (rss + 2*p*sigmaSq) / n
  aic <- -2*loglik + 2*p
  bic <- -2*loglik + log(n)*p
  cv <- cv.lm( lmFit, data, K=K, GCV=GCV )
  bsRslt <- NULL
  if( boot ) bsRslt <- bsMSE.lm( lmFit, data, B )
  
  
  rslt <- data.frame( t(c( p, mse, Cp, aic, bic, cv, bsRslt )) )
  rownames( rslt ) <- ""
  fullNames <- c("df", "mse", "Cp", "aic", "bic",
                 "cv", "cv.k", "bs.mse", "bs.1out", "bs.632")
  if( K != "n" & boot==TRUE ) names( rslt ) <- fullNames
  else if( K != "n" & boot==FALSE ) names( rslt ) <- fullNames[1:7]
  else if( K == "n" & boot==TRUE ) names( rslt ) <- fullNames[c(1:6,8:10)]
  else names( rslt ) <- fullNames[1:6]
  return( rslt )
}
