
setwd("G:\\math\\514")
source("hw5_514.r")

spirals <- spiralpred <- mlbench.spirals(75,1.5,.07)

y <- as.numeric(spirals$classes) - 1
x <- t(spirals$x )

fit <- 
nnet1.fit.batch( x  , y , 1 , 3 ,  35, 50000, 3, 
	Activation = sigmoid, Output = Sigmoid, 32); beep("coin")

nnet1.fit( x, y, 1, 15, 50 ,  MaxLR = 1,
 Activation = relu,   Output = Identity )

fit$perf
#beep("coin") 

 fit$perf[!is.na(fit$performance)]

nnet.Predict( x , 1 , y, 1, fit$B, fit$W, relu, Sigmoid, Batch = 3)


plot(spirals , pch = 15)

plot(1:10, type="l")
#########


# X <- x; Y <- y; HL <- 1; nodes <- 55; batches <- 4; LR = .5

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
	BS <- matrix(  0 , round(M / batches) , batches )
	BP <- list()
	ij <- 1
	Costs <- rep(NA, 30000 * batches)
	 db <- dw <- list()
	C2 <- 100; sm <- .001

	Its <- 75000

	ST <-system.time( 
	for( i in 1:Its ) {
		if( i %in% seq(2,Its ,20) ){ print(C2) }

	BS[1:M] <- sample(M) 
 	for( k in 1:batches) {
		Xt[[k]] <- X[, BS[   BS[,k] > 0 , k ] ]
		Yt[[k]] <- Y[ BS[   BS[,k] > 0 , k ]  ] }

	for( v in 1:length(Xt) ){

		FP  <- fwd.prop( Xt[[v]] , HL , W, B, tanh, Sigmoid)
		C2 <- Costs[ij] <- Cost(Yt[[v]] ,   FP$A[[ HL+1 ]] , "Sigmoid")
 		ij <- ij + 1
 		BP  <-  bk.prop(Xt[[v]], Yt[[v]], HL , W, B, FP$Z, FP$A , 
			tanh) 	
 
	for( j in 1:(HL + 1)  ){
		B[[j]] <- B[[j]] - (LR) * BP$dB[[j]]
		W[[j]] <- W[[j]] - (LR) * BP$dW[[j]] }
	} 
	if( nnet.Predict(  "Sigmoid" , X , Y, 1, W , B, tanh ) == 1){break}

}  )  
beep("coin")

 
 

 yhat <-fwd.prop( X, HL,  W, B, relu , Sigmoid)$A[[HL+1]]
 
 yhat <- ifelse( yhat == 1 , yhat - 1e-15 , yhat )
 yhat <- ifelse( yhat == 0 , yhat + 1e-15 , yhat )
yhat <- ifelse( yhat > .5 , 1 , 0 )
sum(( Y == yhat )*1 )
 
		yhat <- fwd.prop( X , HL , W, B, relu, Sigmoid)$A[[2]]
		yhat <- ifelse( yhat > .5 , 1 , 0    )
		
		if( sum(( as.vector(yhat) == y ) * 1) /  
			length(as.vector(Y)) == 1 ) {break}




yhat <- fwd.prop( X , HL , W, B, relu, Sigmoid)$A[[2]]
yhat <- ifelse( yhat > .5 , 1 , 0    )
sum(( as.vector(yhat) == y ) * 1) /  length(as.vector(Y))

 fwd.prop( X , HL , W, B, sigmoid, Sigmoid)$A[[2]][
	which( ( as.vector(yhat) == y ) == F)]


log(  FP$A[[ HL+1 ]]  )

 Cost(Yt[[v]] ,ifelse( FP$A[[ HL+1 ]] == 1 , 
	 FP$A[[ HL+1 ]] - sm , FP$A[[ HL+1 ]]),
 "Sigmoid")

fwd.prop(  	x , 1,  W, B, 		relu, Sigmoid)$A[[2]]


spiral.predict( 
	plot(spirals, pch = 18)
	SM <-  ( c( seq(-1.5,1.5,.1/2)  )  )

	for( i in 1:length(SM) ){
	for( j in 1:length(SM) ){ 
		Yhat <- fwd.prop( (as.matrix( c( SM[i], SM[j] ) )) , 
			1,  W, B, 	relu, Sigmoid)$A[[2]]  
		Yhat <- ifelse( Yhat > .5 , 2 , 1 )
		points( SM[i ] , SM[j ] , pch = 3 , col = Yhat) 
	}}


	

