setwd("G:\\math\\514")
source("hw5_514.r")

mtrain <- read.csv("G:\\math\\mnist_train.csv" , header = F)
x <- unname( mtrain[,-1] )
x <- t(x)
x <- x / 255
y <-  as.matrix( unname(  mtrain[,1] ))

fit <- nnet1.fit.batch( x  ,  (y) , 1 , 2000 ,  30 , 10, .15, 
	Activation = relu, Output = stable.softmax); beep("coin")

yhat <- fwd.prop( x  , 1 , W, B, relu, stable.softmax)
yhat <- yhat$A[[2]]
Classes  <-  sort(unique(as.vector(y)))
Classes[apply(yhat , 2 , function( M ) which( M == max(M) ) ) ] 	 


# X <- x; Y <- y; HL <- 1; nodes <- 30; Acts <- "relu"; i = v = 1
# Batches <- 2000; MaxLR = .15; Nsim = 10; Outpt <- "stable.softmax"


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