n1=50; mu1=c(.5,.5);  cov1=diag(.2,2)
n2=40; mu2=c(1.5,1.5);cov2=diag(.1,2)
n3=30; mu3=c(1.5,0);  cov3=diag(.1,2)
mean.cov.list=list()
mean.cov.list[[1]]=list(n=50, mu=c(.5,.5),  cov=diag(.2,2))
mean.cov.list[[2]]=list(n=40, mu=c(1.5,1.5),cov=diag(.1,2))
mean.cov.list[[3]]=list(n=30, mu=c(1.5,0),  cov=diag(.1,2))

c(x,y) %<-% gen.gaussian.data.2d(mean.cov.list)


dim(x)
dim( rbind( t(w[[1]]), t(w[[2]]) ) ) 

dim( ( rbind( t(w[[1]]), t(w[[2]]) ) ) %*% x )

l

nodes <- sample( 2:5, l , replace = T)


w <- b <- z <- a <- list()	
n <- dim(x)[1]

ones <- matrix( rep(1, dim(x)[2]) , nrow = 1 )

w[[1]] <- matrix( rnorm( nodes[1] * dim(x)[1] ) , nodes[1] , dim(x)[1] ) 
b[[1]] <- matrix( rnorm( nodes[1]   ) , nodes[1] ,1 ) 


for( i in 2:l){
	
	w[[i]] <- matrix( rnorm( nodes[i] * nodes[i-1]), nodes[i], nodes[i-1] ) 
	b[[i]] <- matrix( rnorm( nodes[i] ) , nodes[i] , 1 )
}

z[[1]] <- b[[1]] %*% ones + w[[1]] %*% x
a[[1]] <- apply( z[[1]] , c(1,2), relu ) 

for( i in 2:l){
	z[[i]] <- b[[i]] %*% ones + w[[i]] %*% a[[i- 1]] 
	a[[i]] <- apply( z[[i]] , c(1,2), relu ) 
}



`tg
init.wgts( dim(x)[1], 2 , 1 )

 

 




c(xx,y,xgrid,ygrid) %<-% data.lawrence.giles(12345)
lm(t(y)~ t(xx) )

b0 <- rnorm(1)
w0 <- rnorm(1)

## Temp example squared error cost with 
## identity output activation using data from data.lawrence.giles

l <- 4
dim(xx)
#xx <- rbind( xx   ,rnorm(20)  )
wb <- init.wgt( l , c(3,5,4,2) , xx) 
lapply(wb$W , dim )

require(beepr)

Lr <-  .1
 
weightz <- init.wgt(1 , 2 ,  xx )
z <- a <- dz <- db <- dw <- list()
weightz$b1 <- weightz$B[[1]] 
weightz$b2 <- weightz$B[[2]] 

weightz$w1 <-  weightz$W[[1]] 
weightz$w2 <-  weightz$W[[2]]




for(i in 1:400000){
z[[1]] <- as.matrix( weightz$b1 ) %*%  rep( 1, dim(xx)[2] ) +
	 ( weightz$w1 ) %*% xx
a[[1]] <- apply(  z[[1]] , c(1,2) ,  tanh )

z[[2]] <- as.matrix( weightz$b2 ) %*%  rep( 1, dim(xx)[2] ) +
	 ( weightz$w2 ) %*% a[[1]]
a[[2]] <- Identity(a[[1]], weightz$b2 , weightz$w2 )

 
#fwd.prop( xx , 1 , list( weightz$w1, weightz$w2 ) , list( as.matrix(
#	weightz$b1 ) , as.matrix( weightz$b2 )  ) , activation = tanh, 
#	output = Identity)$A 


dz[[2]] <- a[[2]] - y
dw[[2]] <- (1/20) * dz[[2]] %*% t( a[[1]] )
db[[2]] <- (1/20) * dz[[2]] %*% rep(1, 20 )

dz[[1]] <-  (apply( z[[1]] , c(1,2) , dtanh)) * ( t(weightz$w2 ) %*% dz[[2]])

dw[[1]] <- (1/20) * dz[[1]] %*% t( xx )
db[[1]] <- (1/20) * dz[[1]] %*% rep(1, 20 )

weightz$b1 <- weightz$b1 - Lr * db[[1]]
weightz$b2 <- weightz$b2 - Lr * db[[2]]
weightz$w1 <- weightz$w1 - Lr * dw[[1]]
weightz$w2 <- weightz$w2 - Lr * dw[[2]]

}; beep("mario")

dev.new()
plot(as.vector(y), ylim = c(-2,2) )
lines( as.vector(a[[2]]))
 
 aaaa <- a[[2]]

#  b  %*% rep( 1, dim(X)[2] )  + w %*% X 

weightz$b2 %*% rep( 1, dim( a[[1]] )[2] )
dim( weightz$w2 )
dim( a[[2]] )
weightz$w2 %*% a[[2]]
cost.negll( a[[1]] , y  , weightz$b2, weightz$w2 )







# Implement a numerical gradients function and use it to test each
# of the 3 cost functions. You can used tanh for the hidden 
# activation function for the gradient checking for all 3 cost functions.  

# Test gradients for squared error cost with identity output 
# activation using data from data.lawrence.giles function provided

# Test gradients for negative log-likelihood error with sigmoid output 
# activation using data from spiral data

# Test gradients for cross-entropy error with numerically stable 
# softmax output activation using data for 3 class mixture provided below
 

aa <- fwd.prop( xx , 4, wb$W, wb$B, activation = tanh, output = Sigmoid )

 
bk.prop( xx, y, 4, wb$W, wb$B , Activation = tanh, derivative = dtanh,
		  Output = Sigmoid )$dw

num.gradient( xx,y, 4 , wb$B,wb$W,h=1e-8 , tanh, Sigmoid   ) 
 

xa <-  aa$A
xa[[length(xa)+1]]  <-  xx

dbb <- list()
xa
hhh <- .000001
for( i in  5:4 ) {
	dbb[[i]] <- ( cost.negll( xa[[i-1]], y, wb$B[[i]] + hhh , wb$W[[i]] ) - 
	cost.negll(xa[[i-1]], y  , wb$B[[i]] - hhh , wb$W[[i]] )  ) / (2*hhh) } 

(cost.negll( xa[[i-1]], y  , wb$B[[i]] + hhh , wb$W[[i]] ) -
cost.negll( xa[[i-1]], y  , wb$B[[i]] - hhh , wb$W[[i]] ) ) / (2*hhh)
 

num.gradient( aa$A[[4]] , y ,  ( wb$B[[5]] ) , wb$W[[5]] )

 
ell <- 5; L <- 2
while( ell > L ) { L <- L + 1 }


aa <- fwd.prop( xx , 4, wb$W, wb$B, activation = tanh, output = Sigmoid )

bk.props( xx, y, 4, wb$W, wb$B , Activation = tanh, derivative = dtanh,
		  Output = Sigmoid )$dw

bk.prop(  xx, y, 4, wb$W, wb$B , aa$Z , aa$A ,  Activation = tanh, 
	derivative = dtanh,  Output = Sigmoid )$dw 


  

(1/20)*  (aa$A[[5]] - y)  %*% t(aa$A[[4]])

 
( t( wb$W[[5]] ) %*% (aa$A[[5]]-y)* apply( aa$Z[[5-1]] , c(1,2), dtanh) ) %*%
aa$A[[4]]

aaaa <- aa$A
	aaaa[[length(aaaa)+1]] <- xx
	aaaa<- aaaa[ c(length(aaaa), 1:(length(aaaa)-1) ) ]

	ddz <- ddw <- ddb <- list()


 	Lell <- 2+1
	ddz[[Lell ]] <- aaaa[[Lell +1]] - y
	
	while( Lell >= 1 ){
		
		ddw[[Lell ]] <- (1/20) * ddz[[Lell ]] %*% t( aaaa[[Lell ]] )  
		ddb[[Lell ]] <- (1/20)*ddz[[Lell ]] %*% rep(1, 20 )
		
		if( Lell > 1) {
			ddz[[Lell -1]] <- t( wb$W[[Lell ]] ) %*%  ddz[[Lell ]] *
			apply( aa$Z[[Lell -1]] , c(1,2), dtanh)  }
		Lell <- Lell - 1 }

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################


c(xx,y,xgrid,ygrid) %<-% data.lawrence.giles(12345)

HL <- 1
wb <- init.wgt( HL , 100 , xx) 


w0 <- wb$W
b0 <- wb$B
system.time( 
for( i in 1:10 ) {
	FP <- fwd.prop( xx , HL , w0, b0, tanh, Identity)
	BP <- bk.prop(xx,y,HL ,w0, b0, FP$Z, FP$A , tanh, dtanh)
for( j in 1:(HL + 1)  ){
	b0[[j]] <- b0[[j]] - .1 * BP$db[[j]]
	w0[[j]] <- w0[[j]] - .1 * BP$dw[[j]]
	}
 });beep("coin")


FP$A[[5]]

plot( as.vector( y  ) , pch = 3) 
lines( as.vector( FP$A[[5]]  ) )




  c(X,Y,xgrid,ygrid) %<-% data.lawrence.giles(12345)
 
  np=length(X)
 
x.set=c(X)
y.set=c(Y)
degree=15  
lm.fit = lm(y ~ poly(x,degree,raw=FALSE), data=data.frame(y=y.set,x=x.set))
y = predict.lm(lm.fit,data.frame(x=xgrid))
plot(xgrid,y,type="l",col="black",lwd=2)
legend("topright", legend = c(num_hidden,paste("degree=",degree)), col = colors,lwd=2 )





