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
require(Matrix) 
#c(X,y) %<-% generate.gaussian.data.class3(n1,mu1,
	#cov1,n2,mu2,cov2,n3,mu3,cov3)

c(x,y) %<-% gen.gaussian.data.2d(mean.cov.list)
#plot(X[1,],X[2,],pch=1,col=y+1,lwd=2,cex=1)
 c(x1,y1) %<-% gen.gaussian.data.2d(mean.cov.list)

#dim(x); k = length( unique( as.vector( y ) ) ); m = dim(x)[2]
#n = dim(x)[1]; w0  <- matrix( rnorm( k*n  ) * .1 , k , n  )
#w0  ; dim(w0  ); b0 <- matrix( rnorm(k) , ncol = 1)
 
  
one.hot <- function(Y){ 
	Y <- as.factor(Y)
	OH <- t(model.matrix(aov(rnorm(length( (Y))) ~  (Y) - 1))[
		1:length(Y),1:nlevels(Y)]); rownames(OH)<-NULL
	return(OH) }
	
fwd.prop <- function( X, b ,w ){
	Z <- ( b %*% matrix( rep(1, dim(X)[2] ) , nrow = 1) + w %*% X) 
	H <- exp( sweep(Z,2,apply(Z,2,max) )) %*% (1/colSums( exp( 
	sweep(Z,2,apply(Z,2,max)  ) ) ) * Diagonal(dim(Z)[2]) 	)   
	return( as.matrix(H))  }


cost <- function(X,Y,b,w){
	H <- fwd.prop(X,b,w)
  	(-1/dim(X)[2]) * sum(  colSums( one.hot(Y) * log( H)) )
}

 

###########################################################################

bk.prop <- function(X,Y ,fprop ){
	M <- dim(X)[2]; E <- fprop - one.hot(Y) 
 	db <- (1/M) *   E   %*% 	rep(1,M )
	dw <- (1/M) *   E   %*% 	t(X)
	return( list( db = db , dw = dw ))
}
 

num.gradient <- function(cost,X,Y,b,w,g=1e-8 )  {
	db <- b
	for( i in 1:length(b) ) {
		bP <- bM <-b; bP[i] <- bP[i]+g; bM[i] <- bM[i]-g
		db[i] <- (cost(X, Y , bP , w ) - cost(X, Y, bM, w  ) ) / (2*g)  }
	dw <- w*0
	for( i in 1:nrow(w) ){
	for( j in 1:ncol(w) ) {
		wP <- wM <- w ; wP[i,j] <- wP[i,j]+g; wM[i,j] <- wM[i,j]-g
		dw[i,j] <- (cost(X, Y , b , wP ) - cost(X, Y, b, wM ) ) / (2*g)
	}}
return( list(db = db , dw =  dw) )} 

 
 

###########################################################################

softmax.fit <- function(X,Y,lr=.01,max.its=100 ){

	K <- length( unique( as.vector( Y ) ) )
	N <- dim(X)[1]; M <- dim(X)[2]
	b<- rnorm( K ) * .1
	W <- matrix( rnorm( K * N ) *.1 , K , N )
	blist <- wlist <- Costs <- gradlist<- list()

	st <- system.time(
	for( i in 1:max.its ) {
		FP <- fwd.prop(X , b, W )
		Costs[[i]] <- (-1/M) * sum( colSums( one.hot(Y) * log(FP)) )
		BP <- bk.prop(X,Y,FP)
		b <- blist[[i]] <- b - lr * BP$db
		W <- wlist[[i]] <- W - lr * BP$dw
		gradlist[[i]] <-  norm( cbind(b,W) , "2")
	} )
	return( list ( b = b , W = W , st = st) )	
}

norm( matrix( rnorm(9) , 3 ,3 ), "2")

Predict <- function(X,Y,b,w){  
 	Classes  <-  sort(unique(Y))
 	Classes[apply( fwd.prop( X , b , w ), 2 , function( M ) 
		which( M == max(M) ) ) ] 	}

 
###########

fit <- softmax.fit( x , y , lr = .2 ,max.its=2000  )
fit

newdat <- gen.gaussian.data.2d(mean.cov.list)

sum(( Predict(newdat[[1]], newdat[[2]], fit$b, fit$W) == newdat[[2]] )*1 ) / 
	length(newdat[[2]])

plot(t(newdat[[1]]))

px <-  seq( -1 , 2.5 , .01 )
Predict( as.matrix( c( px[1], py[1] ) ,nrow = 1 ) , y ,   fit$b, fit$W )
Colors <- c("red", "blue", "green")

for( i in 1:length(px) ){
for( j in 1:length(px) ){
	
	points(  px[i], px[j] , col = Colors[
	Predict(  ( as.matrix( c( px[i], px[j] )  ) ), y , fit$b , fit$W)] )
}}

######################################################################
######################################################################
######################################################################

n1=50; mu1=c(.5,.5);  cov1=diag(.2,2)
n2=40; mu2=c(1.5,2.); cov2=diag(.1,2)
n3=30; mu3=c(1.5,-1); cov3=diag(.1,2)
n4=30; mu4=c(2,0);    cov4=diag(.1,2)
n5=30; mu5=c(2,1);    cov5=diag(.1,2)

mean.cov.list=list()
mean.cov.list[[1]]=list(n=n1, mu=mu1,  cov=cov1)
mean.cov.list[[2]]=list(n=n2, mu=mu2,  cov=cov2)
mean.cov.list[[3]]=list(n=n3, mu=mu3,  cov=cov3)
mean.cov.list[[4]]=list(n=n4, mu=mu4,  cov=cov4)
mean.cov.list[[5]]=list(n=n5, mu=mu5,  cov=cov5)

c(x,y) %<-% gen.gaussian.data.2d(mean.cov.list)


fit <- softmax.fit( x , y , lr = .2 ,max.its=2000 )
fit

Predict(x, fit$b, fit$W) 

newdat <- gen.gaussian.data.2d(mean.cov.list)

 
######################################################################
######################################################################
######################################################################

mtrain <- read.csv("mnist_train.csv" , header = F)
test <- read.csv("mnist_test.csv" , header = F)
dim(test)
dim(mtrain)
colnames(test )[1] <- colnames(mtrain )[1] <- "Y"

set.seed(1)
mtrains <- mtrain[ sample( 1:nrow(mtrain) , 1000 ) , ]
mtrains <- as.matrix(mtrains)
 

nx <- unname( test[,-1] )
dim(nx)
nx <- t(nx);dim(nx)
ny <-  unname( test[,1] )


x <- unname( mtrains[,-1] )
dim(x)
x <- t(x);dim(x)

x[1:10,1:10]
y <-  unname( mtrains[,1] )

fit <- softmax.fit( x , y, lr = .1 ,max.its= 100 )

sum(( Predict(nx,  ny, fit$b, fit$W) == ny )*1 ) / length(ny)

##########################

x <- unname( mtrain[,-1] )
dim(x)
x <- t(x)
y <-  as.matrix( unname(  mtrain[,1] ))

fit <- softmax.fit( x , y, lr = .5 ,max.its= 1000 ); beep("coin") 
fit

sum(( Predict(nx,  ny, fit$b, fit$W) == ny )*1 ) / length(ny)

