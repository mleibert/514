setwd("G:\\math\\514")
source('hw4_514.R')
library(MASS)
library(zeallot)
n1=50; mu1=c(.5,.5);  cov1=diag(.2,2)
n2=40; mu2=c(1.5,1.5);cov2=diag(.1,2)
n3=30; mu3=c(1.5,0);  cov3=diag(.1,2)
mean.cov.list=list()
mean.cov.list[[1]]=list(n=50, mu=c(.5,.5),  cov=diag(.2,2))
mean.cov.list[[2]]=list(n=40, mu=c(1.5,1.5),cov=diag(.1,2))
mean.cov.list[[3]]=list(n=30, mu=c(1.5,0),  cov=diag(.1,2))

#c(X,y) %<-% generate.gaussian.data.class3(n1,mu1,cov1,n2,mu2,cov2,n3,mu3,cov3)
c(X,y) %<-% gen.gaussian.data.2d(mean.cov.list)
plot(X[1,],X[2,],pch=1,col=y+1,lwd=2,cex=1)

one.hot <- function( Y ){
	onehots <- diag( length( unique( as.vector( Y ) ) ) )
	rownames(onehots) <-  as.character( unique( as.vector( Y ) ) ) 
	return(onehots ) }

one.hot(y)

dim(X) 
k = length( unique( as.vector( y ) ) )
m = dim(X)[2]
n = dim(X)[1]
W  <- matrix( rnorm( k*m  ), k , n  )
W; dim(W)
b <- matrix( rnorm(k) , ncol = 1)
 
Z <- b %*% matrix( rep(1,m) , nrow = 1) + W%*%X
dim(Z)

H <-   ( exp(Z) %*% diag( 1/colSums( exp(Z) )   )   )
 
 
one.hot <- function(Y){ 
	Y <- as.factor(Y)
	OH <- t(model.matrix(aov(rnorm(length( (Y))) ~  (Y) - 1))[
		1:length(Y),1:nlevels(Y)]); rownames(OH)<-NULL
	return(OH) }
	
Tt <- one.hot(y)
dim(H);dim(Tt)

-Tt * log( H)

 
