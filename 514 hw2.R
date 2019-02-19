rm(list = ls())
options(scipen= 999)

setwd("G:\\math\\514")

source( "hw2_514.R" )

 


#print.fun('print.fun')
#print.fun('perceptron.box.data')
 

set.seed(123)
gamma <- .05
dat  <- perceptron.box.data( 100, gamma ) 

m1 <- apply( dat[ which(dat[,3] == 1), -ncol(dat) ]  ,2 ,mean)
m2 <- apply( dat[ which(dat[,3] == -1), -ncol(dat) ]  ,2 ,mean)
m1;m2

m2 - m1

plot.perceptron.box.data( dat  ) 

 
 # as.matrix( dat[,-3] )

vanilla <- function( X , ti  , PLOT = T ){
	b <- 0
	w <- c(0,0)
	Z <- rep(NA, length(ti) )

	repeat{
		prev <- c(w,b);
		for( i in 1:nrow( X )) {
	 		z <- b + t(w) %*% X[ i , ];	Z[i] <- z
			if( ti[i]*z  <=  0 ){
			w <- w + ti[i] * X[ i , ] 
			b <- b +  ti[i]
		}}  
	if( ( prev[1] - w[1] < 1e-5) & (prev[2] - w[2] < 1e-5) & 
		(prev[3] - b < 1e-5) ) { break } }
	if( PLOT == T ){
		plot.perceptron.box.data( cbind(X,ti)  ) 
		abline(  -b/  w[2] ,  -w[1] /  w[2] ) }
	Z <- cbind( X, ti , ifelse( Z > 0 , 1, -1 ) )
	Z <- as.data.frame(Z); colnames(Z) <- c( paste0( "x",1:ncol(X)),
		"ti"  , "prediction" )
	Z$accurate <- Z[,ncol(Z)] == Z[,ncol(Z)-1] 
	Zlist <- list(Z, c(  sum(Z$accurate*1) / nrow(Z)  ) , c(b,w) )
	return(Zlist )
 } 

 

va <- vanilla(  as.matrix(  dat[,-3] ) , as.matrix( dat[,3] ) )

weights <- va[[3]]


predict.perceptron <- function( X , b , w , ti ){  
	z <- apply(X , 1 , function(q) b + t(w) %*% q )
	z <- ifelse( z > 0 , 1, -1 )
	plot.perceptron.box.data( cbind(X,ti)  )
	abline(  -b/  w[2] ,  -w[1] /  w[2] )
 	return(z) }
 
newdat  <- perceptron.box.data( 100, gamma ) 
newdat <- cbind( newdat, predict.perceptron( newdat[,-3] ,
	 weights[1] , weights[-1] , newdat[,3] ) )

newdat <- as.data.frame(newdat)
newdat$accurate <- newdat[,ncol(newdat)] ==  newdat[,ncol(newdat)-1]
F %in% newdat$accurate 


#################
blerg <- rep(NA,100)

for( i in 1:length( seq(.1,1,length.out = 10) ) ){

	gamma <- seq(.1,1,length.out = 10)[i]
	
	for( j in 1:10){
		PBD  <- perceptron.box.data( 100, gamma ) 
		blerg[(i-1)*10+j] <- 100 - length( which( vanilla( PBD[,-3] , 
			PBD[,3] , 	PLOT = F)[[1]]$accurate == T ))
		}  
}
err <- data.frame(  sort(rep(seq(.1,1,length.out = 10),10)) , blerg )
colnames(err) <- c("gamma" , "errors")


4*( 3.026267 ^2 ) / .1^-2
 
  norm(   perceptron.box.data( 100, gamma )[,-3]  , "2" )

max( apply( dat , 1 , function(v) norm(v ,"2") ) )

####################################################################
####################################################################
####################################################################

dat <- perceptron.box.data( 100, .1 )

 
	
X <- dat[,-3]
	v  <- rep(0, dim(X)[2] )
y <- dat[,3]
k <- 1
 cc <- 0
V <-   X[1,,drop=F] *0
 
for( J in 1:100){
for( i in 1:nrow(X) ) { 
	 Yhat <- sign(   V[ k ,  ] %*% X[i,]  )   

	if( Yhat == y[i] ) { cc[k] <-  cc[k]+1 } else {
		V  <- rbind( V , V[ k  ,  ] + y[i] * X[i,] )
 		cc <-   c( cc , 1 )
		k <- k + 1
	} 
}}

V ; cc

b <- cc[ k ]
  
w1 <-  (V[ k ,1]);
w2 <-  (V[ k ,2]);
 

w1 <-  mean(V[   ,1]);
w2 <-  mean(V[   ,2]);

plot.perceptron.box.data( dat  )

abline( .75  ,  - w1 / w2 , col = "green" )
#	abline(  -b/  w[2] ,  -w[1] /  w[2] )


 
newdat <- perceptron.box.data( 100, .1 )
.


# [[3]]
# [1]  3.000000  3.162171 -3.005580

Xn <- newdat[ ,-3]
 newdat[1, ]

apply( V , 1 , function(w)  w %*% Xn   )  
Yhats <- 1:nrow(newdat)

for( i in 1:nrow(newdat) ) {

	Yhats[i] <-  sign( sum(sign( apply( V , 1 , function(w)  w %*%  
		newdat[i,-3] )) * 	cc  ))
}

 
Yhats <- apply( Xn , 1 , function(W)  sign( sum( sign( apply( V , 1 , 
	function(w)  w %*%  W )  )* cc ) ) )

freund.voted.perceptron.train



cbind(newdat, sign( newdat[,3] - Yhats) )

avg.perceptron.train




####################################################################
####################################################################
####################################################################

dat <- perceptron.box.data( 100, .1 )

w <- u <- dat[ 1 ,-3 , drop = F] * 0
b <- B <- 0
cc <- 1
y <- dat[,3]

X <- dat[,-3]

for( K in 1:200){

for( i in 1:nrow(X) ) {

	if( (w %*% t(X[i,, drop = F]) + b)*y[i] <= 0 ) {

		w <- w + y[i] * X[i,, drop = F]
		b <- b + y[i]
		u <- u + y[i] * cc* X[i,, drop = F]
		B <- B + y[i] * cc 
	}	
	cc <- cc + 1 }}
 
b <- b - (1/cc) * B
W <- w - (1/cc) * u

plot.perceptron.box.data( dat  )
abline(  -b/  W[2] ,  -W[1] /  W[2] )



####################################################################
####################################################################
####################################################################
rm(list = ls())
options(scipen= 999)

setwd("G:\\math\\514")

source( "hw2_514.R" )

dat <- perceptron.box.data( 100, .1 )



tail(dat)

dat1 <- dat[ which( dat[,3] == 1 ) , ]
X1 <- cbind(   dat1[ , -3] )
n1 <- nrow( dat1  )
m1 <- (1 / n1 ) * apply( X1 , 2 , sum)
s1 <- var(X1)
 

dat2 <- dat[ which( dat[,3] == -1 ) , ]
X2 <- cbind(  dat2[ , -3] )
n2 <- nrow( dat2  )
m2 <- (1 / n2 ) * apply( X2 , 2 , sum)
s2 <- var(X2)

Sw <- s1 + s2
 
w <- solve(Sw)%*%( m2-m1 )
m <- 1/(n1+n2) * ( n1*m1 + n2*m2 )
 
 
plot.perceptron.box.data( dat  )
 
abline(   1.136903 ,  0.6074181 )

solve(w[2],1.2)

 
fisher.2d.line(w)

colnames(dat) <- LETTERS[1:3]
library(MASS)
  lda(C ~ ., data=as.data.frame(dat) )
 

 fisher.2d.line(w,m)


 
 






