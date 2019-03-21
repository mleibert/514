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
xx <- rbind( xx   ,rnorm(20)  )
wb <- init.wgt( l , c(3,5,4,2) , xx) 
lapply(wb$W , dim )



# Implement a numerical gradients function and use it to test each
# of the 3 cost functions. You can used tanh for the hidden 
# activation function for the gradient checking for all 3 cost functions.  

# Test gradients for squared error cost with identity output 
# activation using data from data.lawrence.giles function provided

# Test gradients for negative log-likelihood error with sigmoid output 
# activation using data from spiral data

# Test gradients for cross-entropy error with numerically stable 
# softmax output activation using data for 3 class mixture provided below
 


 
bk.prop( xx, y, 4, wb$W, wb$B , Activation = tanh, derivative = dtanh,
		  Output = Identity )$db
 
 
weightz <- init.wgts(1 , 2 ,  1 )
z <- a <- dz <- db <- dw <- list()

z[[1]] <- as.matrix( weightz$b1 ) %*%  rep( 1, dim(xx)[2] ) +
	 ( weightz$w1 ) %*% xx
a[[1]] <- apply(  z[[1]] , c(1,2) ,  tanh )

z[[2]] <- as.matrix( weightz$b2 ) %*%  rep( 1, dim(xx)[2] ) +
	 ( weightz$w2 ) %*% a[[1]]
a[[2]] <- sigmoid(z[[2]])

#testW <- init.wgt( 1, 2 , xx )
fwd.prop( xx , 1 , list( weightz$w1, weightz$w2 ) , list( as.matrix(
	weightz$b1 ) , as.matrix( weightz$b2 )  ) , activation = tanh, 
	output = Sigmoid)$A 

dz[[2]] <- a[[2]] - y
dw[[2]] <- (1/20) * dz[[2]] %*% t( a[[1]] )
db[[2]] <- (1/20) * dz[[2]] %*% rep(1, 20 )


Identity
cost.negll( a[[1]]  , y ,  weightz$b2 , weightz$w2 ) 

b  %*% rep( 1, dim(X)[2] )  + w %*% X

weightz$b2
weightz$w2

  weightz$w2 %*%  (a[[1]] )
