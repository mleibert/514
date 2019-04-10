
train.perceptron <- function( X ,y,iter=100 , Plot = F){

	w <-  rep( 0 , dim(X)[2] )  
 	b <- 0
	
	for( J in 1:iter) {
	
		for( i in 1:nrow(X) ){
			a <-  t(w) %*% X[ i ,  ] + b 

			if( y[i]*a <= 0 ){
				w <- w + y[i]*X[ i ,  ] 
				b <- b +  y[i]
		}}}		
	
	if( Plot == T ){
	plot.perceptron.box.data( cbind(X,y)  ) 
	abline(  -b/  w[2] ,  -w[1] /  w[2] ) }
	return( list( w = w , b = b ) ) }



plot.perceptron.box.data

 predict.perceptron <- function(w,b,x){
	z <- sign( apply(x , 1 , function(Q) b + t(w) %*% Q ) )
 	return( list( w = z ) ) }


############

freund.voted.perceptron.train <- function(x,y,iter=200){
	b <- 0
	k <- 0
	v <- x[1,,drop=F ]*0
	cc <- 0

	for( J in 1:iter) {
		for( i in 1:nrow(x) ){
			yhat <- sign( b[k+1] + t( v[ k+1 , ] ) %*% x[i,] )	
			if( yhat == y[i] ) { cc[k+1] <- cc[k+1] + 1 } else {
				v <- rbind( v , v[ k+1 ,   ] + y[i]*x[i,]  )
				b <- c( b , b[k+1] +  y[i] )
				cc <- c( cc , 1 )
				k <- k + 1 }
	}}
	return( list( b = b[-1], w = v[-1,], cc = cc[-1], mistakes = k ) ) } 


predict.voted <- function( wgts,cs,bs,x ){ sign( apply( x , 1 , 
	function(V) ( sum( cs * sign( apply( wgts , 1 , function(v) t(v)
	 %*%  V ) + bs ) ) ) ) ) 
	  }
 
############

avg.perceptron.train <- function(x,y,iter=100){
	w <- u <- x[ 1 ,  , drop = F] * 0
	b <- B <- 0
	cc <- 1
	for( K in 1:iter){

	for( i in 1:nrow(x) ) {
		if( (w %*% t(x[i,, drop = F]) + b)*y[i] <= 0 ) {
			w <- w + y[i] * x[i,, drop = F]
			b <- b + y[i]
			u <- u + y[i] * cc* x[i,, drop = F]
			B <- B + y[i] * cc 
		}	
	cc <- cc + 1 }}
 
	return( list( b = b - (1/cc) * B, w = as.vector( w - (1/cc) * u ) ) )
	}


###############

fisher <- function(x,y){

    Dat <- cbind( x, y )

    Dat1 <- Dat[ which( Dat[,3] == -1 ) , ]
    X1 <- cbind(   Dat1[ , -3] )
    n1 <- nrow( Dat1  )
    m1 <- (1 / n1 ) * apply( X1 , 2 , sum)
	
    #s1 <- var(X1) * ((n1 - 1)/n1 )
	s1 <- apply( X1 , 1 , function( W ) W -  m1  )
 	s1 <- matrix( rowSums( as.matrix( apply( s1 , 2 ,  function( W ) 
		W %*% t(W) )  ) ) , 2 ,2)

    Dat2 <- Dat[ which( Dat[,3] == 1 ) , ]
    X2 <- cbind(  Dat2[ , -3] )
    n2 <- nrow( Dat2  )
    m2 <- (1 / n2 ) * apply( X2 , 2 , sum)
    #s2 <- var(X2) * ((n2 - 1)/n2 )
	s2 <- apply( X2 , 1 , function( W ) W -  m2  )
 	s2 <-  matrix( rowSums( as.matrix( apply( s2 , 2 ,  function( W ) 
		W %*% t(W) )  ) ) , 2 ,2)

    Sw <- s1 + s2
 
    w <-   solve(Sw)%*%( m2-m1 )  
    m <- 1/(n1+n2) * ( n1*m1 + n2*m2 )
    return(list(w=as.vector( w), m=m)) 
	}



margin  <- function( x , y , w , b ){
	mar <- ( -y * apply( x , 1 , function(v) b + ( t( w ) %*% v ) ) /
	norm(w,"2") )
	sum( mar[which( mar < 0 )] ) }


margins=function(b,w,x){
  ww=sum(w*w)
	 
 return( -x[,ncol(x)]*(b+x[,-ncol(x)]%*%w) /sqrt(ww))

}
Margin=function(b,w,x){
  ww=sum(w*w)
  distances=margins(b,w,x)
  if(all(distances>=0)){
    print("all positive")
    min(distances)
  }else{
    sum(distances[distances<0])
  }
}
 



