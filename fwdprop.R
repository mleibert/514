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
lm(t(y)~ t(x) )

b0 <- rnorm(1)
w0 <- rnorm(1)

## Temp example squared error cost with 
## identity output activation using data from data.lawrence.giles

l <- 4
dim(xx)
xx <- rbind( xx   ,rnorm(20)  )
wb <- init.wgt( l , c(3,5,4,2) , xx) 
lapply(wb$W , dim )

a <- fwd.prop( xx , l , wb$W , wb$B , output = Identity ) 
m <- length( as.vector( y ))
dz <- dw <- db <- list()

dz[[l+1]] <- a[[l+1]]-y
dw[[l+1]] <- (1/m)*dz[[l+1]] %*% t( a[[l]] )
db[[l+1]] <- (1/m)*dz[[l+1]] %*% rep(1, m )

for( i in l:1){
	dz[[i]] <- t( wb$W[[i+1]] ) %*%  dz[[i+1]] *
		apply( a$Z[[i ]] , c(1,2), drelu )
	if( i == 1){ AA <- xx } else{ AA <- a$A[[i-1]] }
	dw[[i]] <- (1/m)*dz[[i]] %*% t( AA  )
	db[[i]] <- (1/m)*dz[[i]] %*% rep(1, m )
}

 
bk.prop( xx, y, 4, wb$W, wb$B )


