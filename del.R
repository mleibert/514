 
 
nnet2.fit <- function( X, Y, HL, Batches, nodes, Nsim ,  MaxLR = 1,
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

	ST <-system.time( 
	for( i in 1:Nsim ) {

	BS[1:M] <- sample(M) 
 	for( k in 1:Batches) {
		Xt[[k]] <- X[, BS[   BS[,k] > 0 , k ] ]
		Yt[[k]] <- Y[ BS[   BS[,k] > 0 , k ]  ] }

	for( v in 1:length(Xt) ){

		FP  <- fwd.prop( Xt[[v]] , HL , W, B, Activation, Output)
		C2 <- Costs[ij] <- Cost(Yt[[v]] ,   FP$A[[ HL+1 ]] , Outpt )
 		ij <- ij + 1
 		BP  <-  bk.prop(Xt[[v]], Yt[[v]], HL , W, B, FP$Z, FP$A , 
			Activation) 	
 
	for( j in 1:(HL + 1)  ){
		B[[j]] <- B[[j]] - (LR) * BP$dB[[j]]
		W[[j]] <- W[[j]] - (LR) * BP$dW[[j]] }
	}
	Perf[i] <- Predict(Outpt , X, Y, HL, W, B, Activation) 
	if( Perf[i] == 1) {break}
	})  	
	return( list( performance = Perf[!is.na(Perf)], 
		costs = Costs[!is.na(Costs)], ST = ST , W=W, B=B, iter = i)  )
}
