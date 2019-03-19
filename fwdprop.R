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


for( i in 1:l){
	if( i == 1 ){ A <- x } else  { A <- a[[i-1]] }
	z[[i]] <- b[[i]] %*% ones + w[[i]] %*% A
	a[[i]] <- apply( z[[i]] , c(1,2), relu ) 
}

	