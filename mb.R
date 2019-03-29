
setwd("G:\\math\\514")
source("hw5_514.r")

spirals <- spiralpred <- mlbench.spirals(75,1.5,.07)

y <- as.numeric(spirals$classes) - 1
x <- t(spirals$x )





X <- x; Y <- y; HL <- 1; nodes <- 5; batches <- 2; LR = .25

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

	ij <- 1
	Costs <- rep(NA, 30000 * length(Xt))
	
	ST <-system.time( 
	for( i in 1:100000) {
		
	BS[1:M] <- sample(M) 
 	for( k in 1:batches) {
		Xt[[k]] <- X[, BS[   BS[,k] > 0 , k ] ]
		Yt[[k]] <- Y[ BS[   BS[,k] > 0 , k ]  ] }

	for( k in 1:length(Xt) ){

		FP  <- fwd.prop( Xt[[k]] , HL , W, B, sigmoid, Sigmoid)
		C2 <- Costs[ij] <- Cost( Yt[[k]] , FP$A[[ HL+1 ]], "Sigmoid")
 		ij <- ij + 1
 		BP <-  bk.prop(Xt[[k]], Yt[[k]], HL , W, B, FP$Z, FP$A , 
			sigmoid, Sigmoid ) 
	for( j in 1:(HL + 1)  ){
		B[[j]] <- B[[j]] - (LR  )* BP$dB[[j]]
		W[[j]] <- W[[j]] - (LR ) * BP$dW[[j]] }
	}} ) 

 
beep("mario")
 
 
yhat <- fwd.prop( X , HL , W, B, sigmoid, Sigmoid)$A[[2]]
yhat <- ifelse( yhat > .5 , 1 , 0    )
sum(( as.vector(yhat) == y ) * 1) /  length(as.vector(Y))

 fwd.prop( X , HL , W, B, sigmoid, Sigmoid)$A[[2]][
	which( ( as.vector(yhat) == y ) == F)]


