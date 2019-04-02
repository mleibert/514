

setwd("G:\\math\\514")
source("hw5_514.r")

spirals <- spiralpred <- mlbench.spirals(75,1.5,.07)

y <- as.numeric(spirals$classes) - 1
x <- t(spirals$x )


sfit <- nnet1.fit( x, y, 1, 5, 15000  , .5,  tanh ,  Sigmoid  );beep("coin") 

names(sfit)
sfit$st
plot(sfit$perf, type = "l")
plot(sfit$costs, type = "l")
sum( 1*(ifelse(fwd.prop( x , 1, sfit$W, sfit$B, sigmoid  , 
	Sigmoid  )$A[[2]] > .5,1,0) == y ) ) / length(y)

cbs = c("#f1a340","#998ec3")[spirals$classes]
plot(spirals$x[,1],  spirals$x[,2], col = cbs , pch = 19 )
cb <- c("#f1a340","#998ec3")


myseq <- seq(-1.5,1.5,.1/2)
for( i in 1:length(myseq) ){
for( j in 1:length(myseq) ){
	d <-  ( as.matrix( c(myseq[i],myseq[j])) )
	points(  d[1, ],d[2, ], pch = 3, col = cb[1+ ifelse(fwd.prop(  (d) , 
		1, sfit$W, sfit$B, sigmoid  , Sigmoid  )$A[[2]] > .5 , 1 ,0  )]
 )}}




###

setwd("G:\\math\\514")
source("hw5_514.r")


c(x,y,xgrid,ygrid) %<-% data.lawrence.giles(12345)
fit.LG1 <- nnet1.fit( x, y, 1 , 100 , 1000, 1.5,  tanh,  Identity );  
beep("coin") 

