#Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#helper function to print functions
print.fun=function(x){header=deparse(args(x))[1]; b=body(x);  print(gsub('function',x,header));  print(b);}

# one-hidden layer logistic net
# n inputs, nh hidden nodes (one output)
# only changes to regression code
# - change cost function
# - add activation to output node (in fwd.prop)

#source("nn_fns.R")
#source("../lib/data_sources.R")
library(zeallot)   # defines unpacking assignment %<-%


# see: https://www.aaai.org/Papers/AAAI/1997/AAAI97-084.pdf
# lawrence, giles: Lessons in Neural Network Training
data.lawrence.giles=function(seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  # break
  X1 = seq(0,pi,length.out = 7)[c(-1,-7)]
  X2 = seq(pi,2*pi,length.out=15)
  Y1 = -cos(X1)+runif(length(X1),-.25,.25)
  Y2 = cos(3*(X2-pi))+runif(length(X2),-.25,.25)
  X = c(X1,X2)
  dim(X)=c(1,length(X))
  Y = c(Y1,Y2)
  dim(Y)=c(1,length(Y))
  X.grid=seq(0,2*pi,length.out = 99)
  Y.grid=c(-cos(seq(0,pi,length.out=50))[-1],cos(3*(seq(pi,2*pi,length.out = 50)-pi)))
  X=X/(2*pi)
  X.grid=X.grid/(2*pi)
  list(X=X,Y=Y,X.grid=X.grid,Y.grid=Y.grid)
}
plot.lawrence.giles=function(lg.data){
  plot(lg.data$X.grid,lg.data$Y.grid)
  points(lg.data$X,lg.data$Y,col=3,lwd=3)
}



#install.packages('downloader')
download_mnist=function(){
  library(downloader)
  
  if(!file.exists("train-images-idx3-ubyte")) {
    if(!file.exists("train-labels-idx1-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",destfile="train-images-idx3-ubyte.gz")
    system("gunzip train-images-idx3-ubyte.gz") 
  }
  if(!file.exists("train-labels-idx1-ubyte")) {
    if(!file.exists("train-images-idx3-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",destfile="train-labels-idx1-ubyte.gz")
    system("gunzip train-labels-idx1-ubyte.gz")
  }
  if(!file.exists("t10k-images-idx3-ubyte")) {
    if(!file.exists("t10k-images-idx3-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",destfile="t10k-images-idx3-ubyte.gz")
    system("gunzip t10k-images-idx3-ubyte.gz")
  }
  if(!file.exists("t10k-labels-idx1-ubyte")) {
    if(!file.exists("t10k-labels-idx1-ubyte.gz")) download(url="http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",destfile="t10k-labels-idx1-ubyte.gz")
    system("gunzip t10k-labels-idx1-ubyte.gz")
  }
  print("MNIST data load complete")
}


#https://stackoverflow.com/questions/48928652/load-the-mnist-digit-recognition-dataset-with-r-and-see-any-results
load_image_file <- function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  ret$n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  ret
}

load_label_file <- function(filename) {
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  y = readBin(f,'integer',n=n,size=1,signed=F)
  close(f)
  y
}


# binary classication data 
library(mlbench)


softmax=function(a){
  ea=exp(a)
  t(t(ea)/colSums(ea))
}
stable.softmax=function(a){
  a=a-max(a)
  ea=exp(a)
  t(t(ea)/colSums(ea))
}




chunker.cl=function(M,chunk.size){
  chunk=0
  defect= M %% chunk.size
  num.chunks=floor((M+chunk.size-1)/chunk.size)
  # this is very wasteful if chunk.size evenly divides M
  chunks=rep(chunk.size,num.chunks)
  if(sum(chunks)>M) chunks[num.chunks]=M-sum(chunks[-num.chunks])
  sum.chunks=0 # placeholder
  sequence=0   # placeholder
  
  function(){
    if(chunk==0){
      chunks<<-chunks[sample(1:num.chunks,num.chunks,replace=FALSE)]  # reorder chunks
      sum.chunks<<-cumsum(chunks)
      sequence<<-sample(1:M,M,replace=FALSE)
    }
    chunk<<-chunk+1
    if(chunk > num.chunks) {
      chunk<<-0; return(NULL)
    }else{
      start=ifelse(chunk==1,1,sum.chunks[chunk-1]+1)
      sequence[start:sum.chunks[chunk]]
    }
  }
}

if(FALSE){ # example, how to use chunker.cl
  m=32   # number of samples
  chunker=chunker.cl(m,12)  # 12=mini-batch size
  test.chunker=c()
  while(TRUE){
    samples=chunker()
    if(is.null(samples)) break;
    test.chunker=c(test.chunker,samples)
    print(samples)
  }
  all(sort(test.chunker)==(1:m))
}

# alternative to chunker - nicer sematics

seq.generator.cl=function(M,seq.size){
  num.chunks=floor((M+seq.size-1)/seq.size)
  sequence=0     # placeholder
  chunk.start=0  # placeholder
  defect.size= M %% seq.size
  defect.start=0 # placeholder
  seq.not.done=function(){
    response = (chunk.start <= M)
    if(response==FALSE){
      chunk.start <<- 0
    }
    response
  }
  next.seq=function(){
    if(chunk.start==0){ # start new sequence
      chunk.start <<- 1
      sequence <<- sample(1:M,M,replace=FALSE)
      if(defect.size != 0) {
        defect.start <<- (sample(num.chunks,1)-1)*seq.size+1
      }
    }
    if(defect.size==0){
      seq.chunk=sequence[chunk.start:(chunk.start+seq.size-1)]
      chunk.start <<- chunk.start+seq.size
    }else{
      size=ifelse(chunk.start==defect.start,defect.size,seq.size)
      seq.chunk=sequence[chunk.start:(chunk.start+size-1)]
      chunk.start <<- chunk.start+size
    }
    seq.chunk
  }
  list(seq.not.done=seq.not.done,next.seq=next.seq)
}
if(FALSE){
  require(zeallot)
  M=12
  batch.size=7
  c(seq.not.done,next.seq) %<-% seq.generator.cl(M,batch.size)
  for(epochs in 1:3){
    sequence=c()
    while(seq.not.done()){
      sequence=c(sequence,next.seq())
      print(sequence)
    }
    print(all(1:M == sort(sequence)))
  }
}


init.wgts=function(n.in,n.hid,n.out){
  b1 = runif(n.hid,-.1,.1)
  w1 = matrix(rnorm(n.in*n.hid,0,.1),nrow=n.hid,ncol=n.in)
  b2 = runif(n.out,-.1,.1)
  w2 = matrix(rnorm(n.out*n.hid,0,.1),nrow=n.out,ncol=n.hid)
  list(b1=b1,w1=w1,b2=b2,w2=w2)
}

gaussian.mixture.1d=function(n1,m1,s1,n2,m2,s2){
  data=matrix(0,nrow=n1+n2,ncol=2)
  data[(1:n1),1]=rnorm(n1,m1,s1)
  data[(1:n1),2]=1
  data[(n1+1):nrow(data),1]=rnorm(n2,m2,s2)
  data[(n1+1):nrow(data),2]=2
  data[sample(1:nrow(data),nrow(data)),]
}

  generate.1d.dataset=function(n1=30,m1=-1,s1=1,n2=70,m2=2,s2=1){
  #n1=30; m1=-1; s1=1; n2=70; m2=2; s2=1
  data=gaussian.mixture.1d(n1,m1,s1,n2,m2,s2)
  x=matrix(data[,1],nrow=nrow(data))
  y=as.integer(data[,2]==max(data[,2]))
  list(x=x,y=y)
}


gaussian.mixture.2d=function(n1,m1,c1,n2,m2,c2){
  x1=mvrnorm(n1,m1,c1)
  x1=cbind(x1,1)
  x2=mvrnorm(n2,m2,c2)
  x2=cbind(x2,2)
  x =rbind(x1,x2)
  x[sample(1:nrow(x),nrow(x)),]
}

generate.2d.dataset=function(n1=30,m1=c(1,1),c1=diag(c(1,1)),
	n2=70,m2=c(3,3),c2=c(1,1)){
  n1=30; m1=c(1,1); c1=diag(c(1,1))
  n2=70; m2=c(3,3); c2=diag(c(3,3))
  data=gaussian.mixture.2d(n1,m1,c1,n2,m2,c2)
  x=data[,-ncol(data)]
  y=as.integer(data[,ncol(data)]==max(data[,ncol(data)]))
  list(x=x,y=y)
}
library(MASS)
require(mvtnorm )
gen.gaussian.data.2d=function(mean.cov.list){
  data=list()
  for(params in mean.cov.list){
    n=params$n
    mu=params$mu
    cov=params$cov
    print(mu)
    data[[length(data)+1]]=mvrnorm(n,mu,cov)
  }
  X=t(do.call(rbind,data))
  y=matrix(rep(1:length(mean.cov.list),sapply(mean.cov.list,'[[',"n")),nrow=1)
  
  return(list(X,y))
}


############## **Step 2 implement 3 cost functions**

cost.squared.error <- function(X,Y,b,w){
	yhat <-  Identity(X ,b,w )
	M = length(yhat)
	e = matrix(Y - yhat,ncol=1)
	(1/(2*M) )*sum(e^2) }

cost.negll <- function(X,Y,B,W){
	yhat <- Sigmoid( X ,B,W )
	M = length(as.vector(y))
	(-1/M)*sum( ( Y * log( yhat ) ) +  ( (1-Y) * log( 1- yhat ) ) )}



cost.cross.entropy <- function(X,Y,b,w){
	H <- fwd.prop(X,b,w)
  	(-1/dim(X)[2]) * sum(  colSums( one.hot(Y) * log( H)) )}

 

############## **Step 3 Implement 3 output activation functions**

Identity <- function(X ,b,w){ b  %*% rep( 1, dim(X)[2] )  + w %*% X  }

cost.squared.errors <- function(X,Y,b,w){ 
	1/( 2 * length( as.vector( Y ) ) ) * norm( Identity(X,b,w)-Y ,"2")^2 }
	

sigmoid <- function(x) 1 / (1 + exp(-x))

Sigmoid <- function(X ,b,w){ ( 1 / ( 1 + exp( - Identity(X ,b,w) ) ) ) }

require(Matrix)


one.hot <- function(Z){	return(unname( as.matrix( 
	as.data.frame( t( model.matrix(~ as.factor(Z) + 0) ) ) ) )) }


stable.softmax <- function( X, b ,w ){
	V <- ( b %*% matrix( rep(1, dim(X)[2] ) , nrow = 1) + w %*% X)
	V <-  exp( sweep(V,2,apply(V,2,max) ))
	H <- V %*% (1/colSums( V ) * Diagonal(dim(V)[2]) 	)   
	return( as.matrix(H))  }



cost.negll <- function(X,Y,b,w){
	(-1 / length(as.vector(Y)))*sum( Y * log( Sigmoid(X ,b,w) ) + 
	(1 - Y ) *  log( 1 - Sigmoid(X ,b,w) )  ) }

stable.softmax <- function( X, b ,w ){
	Z <- ( b %*% matrix( rep(1, dim(X)[2] ) , nrow = 1) + w %*% X)
	Z <-  exp( sweep(Z,2,apply(Z,2,max) ))
	H <- Z %*% (1/colSums( Z ) * Diagonal(dim(Z)[2]) 	)   
	return( as.matrix(H))  }

cost.cross.entropy <- function(X,Y,b,w){
	H <- stable.softmax(X,b,w)
  	(-1/dim(X)[2]) * sum(  colSums( one.hot(Y) * log( H)) )}


Cost <-  function( Y, Yhat, Outputs, batches = 1  ){
		M <- length( as.vector( Y ) )
		if( Outputs == "Identity" ) {  return(   
			(  1/ (2* M )) * norm( Yhat - Y ,"2" )^2  )
		} else if ( Outputs == "Sigmoid" ) {  
			Yhat <- ifelse( Yhat == 1 , Yhat - 1e-9 , Yhat)
			return( 
			(-1 / M )*sum( ( Y * log( Yhat ) )  + 
			( (1 - Y ) *  log( 1 - Yhat )  )  )    
		) } else { 
		if( batches > 1 ) {
			return( (-1/ncol(Y) ) * sum( colSums( (Y) * log( Yhat ) )
			 ) )	} else { 
		return( (-1/M) * sum(colSums(one.hot(Y) * log(Yhat))))}
	}}




######### **Step 4 Implement 3 hidden layer activation functions**

relu <- function(X){   max( c(0,X) )   }
drelu <- function(X){ ifelse( X < 0 , 0 , 1 ) }

dtanh <- function(X){1-tanh(X)^2 }

dsigmoid <- function(X){ sigmoid(X) * (1-sigmoid(X) ) }
 
 

init.wgt <- function( layers , nodes , X, Y, outp = "Sigmoid", d = .01 ) {
	W <- B <- list()
	LN <- ifelse( outp != "stable.softmax", 
		1 ,	length(as.vector(  unique( y ) ) ) )
	node <- c( dim(X)[1] , nodes, LN )
	for( i in 1:(layers+1) ){
		W[[i]] <- d*matrix(rnorm(node[i]*node[i+1],.1),
				node[i+1],node[i ])
		B[[i]] <-d*matrix(rnorm(  node[i+1],.1 ) , node[i+1] , 1 )}
	return(list( W = W , B = B) ) }


 


fwd.prop <- function( X , L, W , B , activation , output   ){
 
	A <- Z <- list()
	for( i in 1:L){
		if( i == 1 ){ AA <- X } else  { AA <- A[[i-1]] }
		Z[[i]] <- B[[i]] %*%  rep( 1, dim(AA)[2] ) +  ( W[[i]] ) %*% AA
		A[[i]] <- apply( Z[[i]] , c(1,2), activation ) 	}

	Z[[L+1]] <-B[[L+1]] %*%  rep( 1, dim(AA)[2] ) + W[[L+1]] %*% A[[L]]
	A[[L+1]] <-  output( A[[L]],B[[L+1]] , W[[L+1]]  )
	 return( list( A = A , Z = Z )  )
} 

#fwd.props <- function( X , L, W, B , activation = relu, output = Sigmoid ){
#
#	A <- Z <- list()
#	A[[1]] <- X
#	for( i in 1:L){
#		Z[[i]] <- B[[i]] %*%  rep( 1, dim(A)[2] ) +  ( W[[i]] ) %*% A
#		A[[i]] <- apply( Z[[i]] , c(1,2), activation ) 	}
#
#	Z[[L+1]] <-B[[L+1]] %*%  rep( 1, dim(A)[2] ) + W[[L+1]] %*% A[[L]]
#	A[[L+1]] <-  output( A[[L]],B[[L+1]] , W[[L+1]]  )
#	 return( list( A = A , Z = Z )  )
#} 



##	 X <- xx ; Y <- y ; L <-4 ; W <- wb$W ; B <- wb$B


bk.props <-  function( X, Y, L, W, B, Activation = relu, derivative = drelu,
		  Output = Identity ){

	fp <- fwd.prop( X , L, W , B, activation = Activation, output = Output ) 
	m <- length( as.vector( y ))
	dz <- dw <- db <- list()

	dz[[L+1]] <- (fp$A[[L+1]]) - Y
	dw[[L+1]] <- (1/m)*dz[[L+1]] %*% t( fp$A[[L]] )
	db[[L+1]] <- (1/m)*dz[[L+1]] %*% rep(1, m )

	for( i in L:1){
		dz[[i]] <- t( W[[i+1]] ) %*%  dz[[i+1]] *
			apply( fp$Z[[i ]] , c(1,2), derivative )
		if( i == 1){ AA <- X } else{ AA <- fp$A[[i-1]] }
		dw[[i]] <- (1/m)*dz[[i]] %*% t( AA  )
		db[[i]] <- (1/m)*dz[[i]] %*% rep(1, m )
	}
	return( list(dz = dz , db= db , dw=dw ) ) }




num.gradient <- function( X, Y, B,W, cost ,h=1e-8  )  {
	dB <- dW <- list()
	for( i in 1:length(B) ) {
	bP <- bM <-as.vector( B ) ; bP[i] <- bP[i] + h ; bM[i] <- bM[i] - h
	dB[[i]] <- (cost(X, Y , bP , W ) - cost(X, Y, bM, W  ) ) / (2*h)  }
	dW <- W*0
	for( i in 1:nrow(W) ){
	for( j in 1:ncol(W) ) {
		wP <- wM <- W ; wP[i,j] <- wP[i,j]+h; wM[i,j] <- wM[i,j]-h
		dW[i,j] <- (cost(X, Y , B , wP ) - cost(X, Y, B, wM ) ) / (2*h)
	}}
	return( list(dB = dB , dW =  dW) )} 

require(beepr)


bk.prop <-  function( X, Y, L, W, B,  Z , A,  Activation = relu, 
		derivative = drelu  ){

	m <- length( as.vector( Y ))
	dZ <- dW <- dB <- list()
	A[[length(A)+1]] <- X
	A <- A[ c(length(A), 1:(length(A)-1) ) ]

 	ell <- L+1
	dZ[[ell]] <- A[[ell+1]] - Y
	
	while( ell >= 1 ){
		
		dW[[ell]] <- (1/m) * dZ[[ell]] %*% t( A[[ell]] )  
		dB[[ell]] <- (1/m)*dZ[[ell]] %*% rep(1, m )
		
		if( ell > 1) {
			dZ[[ell-1]] <- t( W[[ell]] ) %*%  dZ[[ell]] *
			apply( Z[[ell-1]] , c(1,2), derivative ) }
			ell <- ell - 1 } 

	return( list(dZ = dZ , dB= dB , dW=dW ) ) }
 

####

nnet1.fit <- function( X, Y, HL, nodes, Nsim ,  MaxLR = 1,
		 Activation = relu,   Output = Identity ){
	
	LR <- MaxLR
	
	Acts <- as.character(substitute(Activation ) )
	Outpt <- as.character(substitute( Output ) )
 
	WB <- init.wgt( HL, nodes , X, Y , Outpt  ) 
	W <- WB$W; B <- WB$B

	if( Acts == "relu" ){ Derivative <- drelu 
		} else if ( Acts == "tanh" ) {   Derivative <- dtanh 
		} else { Derivative <- dsigmoid } 

	C1 <- 0; Costs <- rep(NA, Nsim)
	M <- length(as.vector(Y))
 
	ST <-system.time( 
	for( i in 1:Nsim) {
		FP  <- fwd.prop( X , HL , W, B, Activation, Output)
		C2 <- Costs[i] <- Cost( Y , FP$A[[ HL+1 ]] ,  Outpt )
 		
 		BP <-  bk.prop(X, Y, HL , W, B, FP$Z, FP$A , Acts ) 
 
		B.OLD <- B; W.OLD <- W
	for( j in 1:(HL + 1)  ){
		B[[j]] <- B[[j]] - LR * BP$dB[[j]]
		W[[j]] <- W[[j]] - LR * BP$dW[[j]]
		if( is.nan( FP$A[[ HL + 1 ]][1] ) == T ) {break}  }

		if( C1 < C2 ){ i = i-1; LR <- LR *.5; B <- B.OLD; W <- W.OLD 
			} else {	LR <- LR * 1.1  }
		C1 <- C2
 
		LR <- ifelse( LR > MaxLR , MaxLR , LR ) 
 
	}) 
	return( list(   st = ST, yhat = FP$A[[ HL+1 ]], costs = C2 , W=W, B=B)  ) }




 


bk.prop <-  function( X, Y, L, W, B,  Z , A,  Act    ){

	if( Act == "relu" ){ derivative <- drelu 
	} else if ( Act== "tanh" ) {   derivative <- dtanh 
	} else { derivative <- dsigmoid }	

	m <- length( as.vector( Y ))
	dZ <- dW <- dB <- list()
	A[[length(A)+1]] <- X
	A <- A[ c(length(A), 1:(length(A)-1) ) ]

 	ell <- L+1
	if( nrow(  A[[ length(A) ]]  > 2  )){ Y <- one.hot(Y) }
	dZ[[ell]] <- A[[ell+1]] - Y
	
	while( ell >= 1 ){
		
		dW[[ell]] <- (1/m) * dZ[[ell]] %*% t( A[[ell]] )  
		dB[[ell]] <- (1/m)*dZ[[ell]] %*% rep(1, m )
		
		if( ell > 1) {
			dZ[[ell-1]] <- t( W[[ell]] ) %*%  dZ[[ell]] *
			apply( Z[[ell-1]] , c(1,2), derivative ) }
			ell <- ell - 1 } 

	return( list(dZ = dZ , dB= dB , dW=dW ) ) }
 


 

# 
# bk.prop <-  function( X, Y, L, W, B,  Z , A,  Acti , Batch = 1  ){
#   
#   Act  <- as.character(substitute( Acti ) )
#   
#   if( Act == "relu" ){ derivative <- drelu 
#   } else if ( Act== "tanh" ) {   derivative <- dtanh 
#   } else { derivative <- dsigmoid }	
#   
#   m <- ifelse( Batch > 1 & (is.matrix(Y)==T), 
#                ncol(Y), length( as.vector( Y )))
#   dZ <- dW <- dB <- list()
#   A[[length(A)+1]] <- X
#   A <- A[ c(length(A), 1:(length(A)-1) ) ]
#   
#   ell <- L+1
#   if(   dim(   A[[ L + 1 ]] )[2]   > 2  ){
#     OneH <- ifelse( is.matrix(Y) == T, Y  , one.hot(Y) )
#     dZ[[ell]] <- A[[ell+1]]- OneH } else {
#       dZ[[ell]] <- A[[ell+1]] - Y }
#   
#   while( ell >= 1 ){
#     
#     dW[[ell]] <- (1/m) * dZ[[ell]] %*% t( A[[ell]] )  
#     dB[[ell]] <- (1/m)*dZ[[ell]] %*% rep(1, m )
#     
#     if( ell > 1) {
#       dZ[[ell-1]] <- t( W[[ell]] ) %*%  dZ[[ell]] *
#         apply( Z[[ell-1]] , c(1,2), derivative ) }
#     ell <- ell - 1 } 
#   
#   return( list(dZ = dZ , dB= dB , dW=dW ) ) }
# 

 
 
nnet1.fit.batch <- function( X, Y, HL, Batches, nodes, Nsim ,  MaxLR = 1,
		 Activation  ,   Output  ){

	LR <- MaxLR
	WB <- init.wgt( HL, nodes , X) 
	W <- WB$W; B <- WB$B

	Acts <- as.character(substitute(Activation ) )
	Outpt <- as.character(substitute( Output ) )
 

	if( Acts == "relu" ){ Derivative <- drelu 
		} else if ( Acts == "tanh" ) {   Derivative <- dtanh 
		} else { Derivative <- dsigmoid } 

	C1 <- 0; Costs <- list()
	M <- length(as.vector(Y))

	#Batch	
	Xt <- Yt <- list()
	BS <- matrix(  0 , round(M / Batches) , Batches)
	BP <- list()
	ij <- 1
	Costs <- rep(NA, Nsim * Batches)
	 db <- dw <- list()
	C2 <- 100; sm <- .001; Perf <- rep(NA, Nsim) 
	if(Outpt == "stable.softmax"){ OH <- one.hot(Y)  }

	ST <-system.time( 
	for( i in 1:Nsim ) {

	BS[1:M] <- sample(M) 
 	for( k in 1:Batches) {
		Xt[[k]] <- X[, BS[   BS[,k] > 0 , k ] ]
		if (  Outpt == "stable.softmax") { 
			Yt[[k]] <- OH[  , BS[   BS[,k] > 0 , k ] ] } else { 
			Yt[[k]] <-  Y[ BS[   BS[,k] > 0 , k ]  ] } }
#})
#return( list( xt = Xt , yt = Yt )) }

	for( v in 1:length(Xt) ){
		
		FP  <- fwd.prop( Xt[[v]] , HL , W, B, Activation, Output)
		C2 <- Costs[ij] <- Cost(Yt[[v]], FP$A[[ HL+1 ]] , Outpt, Batches )
 		ij <- ij + 1
 		BP  <-  bk.prop(Xt[[v]], Yt[[v]], HL , W, B, FP$Z, FP$A , 
			Activation, Batches) 	
 
	for( j in 1:(HL + 1)  ){
		B[[j]] <- B[[j]] - (LR) * BP$dB[[j]]
		W[[j]] <- W[[j]] - (LR) * BP$dW[[j]] }
	}
	Perf[i] <- nnet.Predict(Outpt , X, Y, HL, W, B, Activation) 
	if( Perf[i] == 1) {break}
	})  	
	return( list( performance = Perf[!is.na(Perf)], 
		costs = Costs[!is.na(Costs)], ST = ST , W=W, B=B, iter = i)  )
}





nnet.Predict <- function( OP, X, Y , HL, W, B, Activation  ) {

	 if( OP  == "Sigmoid" ){
		yhat <- fwd.prop( X, HL,  W, B, Activation, Sigmoid)$A[[HL+1]] 
		yhat <- ifelse( yhat == 1 , yhat - 1e-15 , yhat )
		yhat <- ifelse( yhat == 0 , yhat + 1e-15 , yhat )
		yhat <- ifelse( yhat > .5 , 1 , 0 )
		sum( ( Y == yhat )*1 ) / length( as.vector( Y ) )
	 } else if ( OP == "stable.softmax" ){ 0
		#Classes  <-  sort(unique(as.vector(Y)))
 		#yhat <- Classes[apply( fwd.prop( X , b , w, Activation, 
		#	stable.softmax ),	2 , function( M ) which( M == max(M)))]  
		#sum( ( Y == yhat )*1 ) / length( as.vector( Y ) )
 	} else { print("?") }
}

 

# 
# bk.prop <-  function( X, Y, L, W, B,  Z , A,  Activation    ){
#   
#   Act  <- as.character(substitute( Act ) )
#   
#   if( Act == "relu" ){ derivative <- drelu 
#   } else if ( Act== "tanh" ) {   derivative <- dtanh 
#   } else { derivative <- dsigmoid }	
#   
#   m <- length( as.vector( Y ))
#   dZ <- dW <- dB <- list()
#   A[[length(A)+1]] <- X
#   A <- A[ c(length(A), 1:(length(A)-1) ) ]
#   
#   ell <- L+1
#   if(  length(as.vector(  unique( y ) ) ) > 2  ){
#     dZ[[ell]] <- A[[ell+1]]- one.hot(Y) } else {
#       dZ[[ell]] <- A[[ell+1]] - Y }
#   
#   while( ell >= 1 ){
#     
#     dW[[ell]] <- (1/m) * dZ[[ell]] %*% t( A[[ell]] )  
#     dB[[ell]] <- (1/m)*dZ[[ell]] %*% rep(1, m )
#     
#     if( ell > 1) {
#       dZ[[ell-1]] <- t( W[[ell]] ) %*%  dZ[[ell]] *
#         apply( Z[[ell-1]] , c(1,2), derivative ) }
#     ell <- ell - 1 } 
#   
#   return( list(dZ = dZ , dB= dB , dW=dW ) ) }
# 
# 


# 
# bk.prop <-  function( X, Y, L, W, B,  Z , A,  Acti , Batch = 1  ){
#   
#   Act  <- as.character(substitute( Acti ) )
#   
#   if( Act == "relu" ){ derivative <- drelu 
#   } else if ( Act== "tanh" ) {   derivative <- dtanh 
#   } else { derivative <- dsigmoid }	
#   
#   m <- ifelse( Batch > 1 & (is.matrix(Y)==T), 
#                ncol(Y), length( as.vector( Y )))
#   dZ <- dW <- dB <- list()
#   A[[length(A)+1]] <- X
#   A <- A[ c(length(A), 1:(length(A)-1) ) ]
#   
#   ell <- L+1
#   if(   dim(   A[[ L + 1 ]] )[2]   > 2  ){
#     OneH <- ifelse( is.matrix(Y) == T, Y  , one.hot(Y) )
#     dZ[[ell]] <- A[[ell+1]]- OneH } else {
#       dZ[[ell]] <- A[[ell+1]] - Y }
#   
#   while( ell >= 1 ){
#     
#     dW[[ell]] <- (1/m) * dZ[[ell]] %*% t( A[[ell]] )  
#     dB[[ell]] <- (1/m)*dZ[[ell]] %*% rep(1, m )
#     
#     if( ell > 1) {
#       dZ[[ell-1]] <- t( W[[ell]] ) %*%  dZ[[ell]] *
#         apply( Z[[ell-1]] , c(1,2), derivative ) }
#     ell <- ell - 1 } 
#   
#   return( list(dZ = dZ , dB= dB , dW=dW ) ) }
# 
