
train.perceptron <- function( X ,y,iter=100){

	w <-  rep( 0 , dim(X)[2] )  
 	b <- 0
	
	for( J in 1:iter) {
	
		for( i in 1:nrow(X) ){
			a <-  t(w) %*% X[ i ,  ] + b 

			if( y[i]*a <= 0 ){
				w <- w + y[i]*X[ i ,  ] 
				b <- b +  y[i]
		}}}		
	W <- list( w , b ); return(W) }





 predict.perceptron <- function(w,b,x){
	z <- sign( apply(x , 1 , function(Q) b + t(w) %*% Q ) )
 	return(z) }
