rm(list = ls())

x <- rbinom(10, 1, .5)
n <- length(x)
set.seed(1234)
setwd("G:\\math\\514")
source("hw3_514.R")
require(MASS)
theta <- runif(1)

theta^x * (1-theta)^(1-x)


b0 <- rnorm(1)
w0 <- rnorm(2)

log( prod( theta^x * (1-theta)^(1-x) ) )

neg.logll <- function( x ,  y , b , w ){
	-log( sigmoid(b+ x %*% w) )*y - (1-y)*log( sigmoid(b+ x %*% w) ) }

f.prop <- function(x,b,w){ sigmoid(apply(x ,1 , function(R) b + t(w) %*% R ))}  

dat<-generate.1d.dataset(); w0 <- rnorm( ncol( dat$x ) )
dat<-generate.2d.dataset(); w0 <- rnorm( ncol( dat$x ) )
b0 <- rnorm(1)

b.prop <- function(x,y,yhat,b,w,neg.logll=T){
	
	e <- apply(x , 1 , function(R) sigmoid( b + t(w) %*% R ) ) - y
	db <- ( 1/length(x) ) * sum(e)
	dw <- (1/length(x) ) * colSums( e * x )
	return( list(db = db, dw = dw ) )}

b.prop( x = dat$x , y = dat$y , yhat= 1 , b=b0 ,w= w0)
 



num.gradient <- function(cost,x,y,b,w,h=1e-8,neg.logll=T)  {
	db <- ( cost(x, y , b+h , w ) - cost(x, y , b , w  ) ) / h
	dw <- ( cost(x, y , b , w +h ) - cost(x, y , b , w  ) ) / h
	return( list(db = db, dw = dw ) )} 

num.gradient(cost, dat$x, dat$y , b0, w0, h=1e-8 , neg.logll=T)

 cost(dat$x , dat$y , b0 , w0+h ) 

sapply( w , function(H) cost(dat$x , dat$y , b0 , H+h ) )


