rm(list = ls())
set.seed(1234)
setwd("G:\\math\\514")
source("hw3_514.R")
require(MASS)
 
dat<-generate.1d.dataset(); w0 <- rnorm( ncol( dat$x ) )
#dat<-generate.2d.dataset(); w0 <- rnorm( ncol( dat$x ) )
b0 <- rnorm(1)


neg.logl  <- function( x ,  y , b , w ){
	-log( sigmoid(b+ x %*% w) )*y - (1-y)*log( sigmoid(b+ x %*% w) ) }

f.prop <- function(x, b, w, neg.logll=T){
	if(neg.logll){ f.prop = sigmoid(x%*%w +b  )} else 
		{f.prop =  x%*%w +b }
	f.prop  }

b.prop <- function(x,y,yhat,b,w,neg.logll=T){
	if( neg.logll == T) {e <- apply(x , 1 , function(R) 
		sigmoid( b + t(w) %*% R ) ) - y } else {
	 	e <- apply(x , 1 , function(R)  ( b + t(w) %*% R ) ) - y }
	db <- ( 1/length(y) ) * sum(e)
	dw <- (1/length(y) ) * colSums( e * x )
	return( list(db = db, dw = dw ) )}
	

f.prop( x = dat$x   , b=b0 ,w= w0  )

b.prop( x = dat$x , y = dat$y , yhat= 1 , b=b0 ,w= w0 ,neg.logll=F)
 
num.gradient <- function(cost,x,y,b,w,h=1e-8, neg.logll=T )  {
	db <- ( cost(x, y , b + h , w , neg.logll  =  neg.logll   ) - 
		cost(x, y , b , w   , neg.logll =   neg.logll   ) ) / h
	dw<-wH<-w0
	for( i in 1:length(w) ) {
		wH<-w;	wH[i] <- wH[i]+h
		dw[i] <- (cost(x, y , b , wH , neg.logll =  neg.logll      ) - 
		cost(x, y, b, w  , neg.logll =   neg.logll    ))/ h  }
	return( list(db = db, dw = dw ) )} 

num.gradient(cost,x = dat$x ,dat$y,b0 ,w0  ,h = .00001,neg.logll=F )


Predict <- function(x,b,w){return( ifelse( f.prop(x,b,w) > .5 ,  1 , 0 ) )}
 Predict(dat$x , b0, w0 )

 
### x <- dat$x ; y <- dat$y; Nsim = 2000; a = .25; w <- w0; b <- b0; a = .25
log.fit <- function( x ,y , neg.logll = T , eta=.25, Nsim = 2000, 
	tol, b0 = 0 , w0 = 0 ){
 
	if( length( w0 != dim(x)[2] ) ) { w0 <- rnorm( dim(x)[2] ) }
	err <- b <- Costs <- c(b0,rep(NA,Nsim ) )
	w <- matrix( NA , Nsim + 1, dim(x)[2] ) ; w[1,] <- w0
	for( i in 1:Nsim  ){
		#err[i] <- sigmoid( b[i] + x %*% w[i,] ) - y
		Costs[i] <- cost(x,y, b[i] ,w[i ,], neg.logll = neg.logll )
		BP <- b.prop( x,y, 1 ,b[i],w[ i, ], neg.logll = neg.logll )
		b[i+1] <- b[i] - eta *  BP$db
		w[i+1,] <- w[i ,] - eta *  BP$dw
	 #if( abs( b[i] - b[i+1] ) < .00001 & ( norm( w[i+1] , "2" ) - 
	 #	norm( w[i ] , "2" ) ) <  .00001 ){ break }
	}
		#print(i)
		return( list( b = b[i], w = w[i,], W = w[1:i,], B = b[1:i],
		cost = Costs[!is.na(Costs)], gradnorm = apply( 	cbind(b[1:i], 
		w[1:i,]) , 1 , function(N) norm(N, "2")  ) ) )}



  
###########
A <- matrix(rnorm(10),5,2)

m1=50;mu1=1;s1=.5
m2=50;mu2=2;s2=.7
c(xin,y) %<-% generate.1d.dataset(m1,mu1,s1,m2,mu2,s2)
plot(xin,col=y+3)
bayes.cut2=function(mu1,s1,mu2,s2){
  ff=function(x){dnorm(x,mu2,s2) - dnorm(x,mu1,s1)}
  uniroot(ff,c(mu1,mu2))$root
}

cut=bayes.cut2(mu1,s1,mu2,s2)
abline(h=cut)
h1=hist(xin[y==0], col=rgb(0,1,0,1/4), xlim=c(0,4),ylim=c(0,30))
h2=hist(xin[y==1], col=rgb(0,0,1,1/4), xlim=c(0,4),ylim=c(0,30), add=T)
abline(v=cut)
error=.5*pnorm(cut,mu2,s2)+.5*(1-pnorm(cut,mu1,s1))
error

################################# #####################################


fit <- log.fit( dat$x , dat$y ,neg.logll=T  ) ;  
c( fit$b, fit$w )
coef( glm( dat$y  ~ dat$x , family=binomial ) )



z <- cbind( fit$W, fit$B , fit$cost)

 
plot(   fit$cost )
plot( fit$B, fit$W )
plot(   fit$W )
plot( fit$gradnorm )


contour(x = seq(-10,  30, length.out = nrow(z)),
        y = seq(-10,  10, length.out = ncol(z)),  
	z , nlevels = 50)

lines( fit$W , col = "red", type = "o", pch = "16")

################################# 

fit <- log.fit( dat$x , dat$y ,neg.logll= F  ) ;  
c( fit$b, fit$w )
coef( glm( dat$y  ~ dat$x , family=gaussian ) )




contour(x = seq(-10,  30, length.out = nrow(z)),
        y = seq(-10,  10, length.out = ncol(z)),  
	z , nlevels = 50)

lines( fit$W , col = "red", type = "o", pch = "16")


require(ggplot2)

z <- as.data.frame(z);tail(z)
ggplot(z, aes(x = V1, y = V2, z = V3)) +
         stat_contour(geom = "polygon", aes(fill = ..level..) ) +
         geom_tile(aes(fill = qsec)) +
         stat_contour(bins = 15) +
         xlab(“Weight (1,000lbs)”) +
         ylab(“Horsepower”) +
         guides(fill = guide_colorbar(title = “¼ Mi. Time (s)”))


