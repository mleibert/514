setwd("G:\\math\\514")
source("hw5_514.r")
c(x,y,xgrid,ygrid) %<-% data.lawrence.giles(12345)
 np=length(x)
 
x.set=c(x)
y.set=c(y)
degree=15  
lm.fit = lm(y ~ poly(x,degree,raw=FALSE), data=data.frame(y=y.set,x=x.set))
yh = predict.lm(lm.fit,data.frame(x=xgrid))
plot(x.set, y.set, type='p', col='blue', 
	pch=19, ylim=c(-3,3), main='Linear Fit by N.Hid')
points(xgrid,yh,type="l",col="black",lwd=2)


fit.LG1 <- nnet1.fit( x, y, 1 , 5 , 100000 , 1.5,  tanh,  Identity );  
beep("coin") 

 fit.LG1$yhat
points( x,as.vector(fit.LG1$yhat) , type = "l", lwd=2 , col = '#7fc97f') 
points( as.vector(fit.LG2$yhat) , type = "l", lwd=2 , col = '#beaed4') 
points( as.vector(fit.LG3$yhat) , type = "l", lwd=2 , col = '#fdc086') 


 



setwd("G:\\math\\514")
source("hw5_514.r")

spirals <- spiralpred <- mlbench.spirals(75,1.5,.07)

y <- as.numeric(spirals$classes) - 1
x <- t(spirals$x )


sfit <- nnet1.fit( x, y, 1, 5, 10000 ,  3,  sigmoid  ,  Sigmoid  )
names(sfit)

plot(sfit$perf, type = "l")
plot(sfit$costs, type = "l")
sum( 1*(ifelse(fwd.prop( x , 1, sfit$W, sfit$B, sigmoid  , 
	Sigmoid  )$A[[2]] > .5,1,0) == y ) ) / length(y)

 ifelse(fwd.prop( x , 1, sfit$W, sfit$B, sigmoid  , 
	Sigmoid  )$A[[2]] > .5 , 1 ,0  )

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

