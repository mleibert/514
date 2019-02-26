#Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#helper function to print functions
print.fun=function(x){header=deparse(args(x))[1]; b=body(x);  print(gsub('function',x,header));  print(b);}


#source("./nn_fns.R")
require(zeallot)
require(plyr)
require(mvtnorm)

sigmoid = function(x) 1 / (1 + exp(-x))

# provide means and sd for each component
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

generate.2d.dataset=function(n1=30,m1=c(1,1),c1=diag(c(1,1)),n2=70,m2=c(3,3),c2=c(1,1)){
  n1=30; m1=c(1,1); c1=diag(c(1,1))
  n2=70; m2=c(3,3); c2=diag(c(3,3))
  data=gaussian.mixture.2d(n1,m1,c1,n2,m2,c2)
  x=data[,-ncol(data)]
  y=as.integer(data[,ncol(data)]==max(data[,ncol(data)]))
  list(x=x,y=y)
}



# this function is not used by logistic code - it is used to look as cost as a function of b,w
cost=function(x,y,b,w,neg.logll=TRUE){
  yhat=f.prop(x,b,w,neg.logll=neg.logll)
  m = length(yhat)
  if(neg.logll){
    #cost=(-1/m)*sum(y*log(yhat)+(1-y)*log(1-yhat))
    cost=(-1/m)*(sum(log(yhat[y==1]))+sum(log(1-yhat[y==0])))
  }else{
    e = matrix(y - yhat,ncol=1)
    cost=(1/(2*m) )*sum(e^2)
  }
  cost
}

classify=function(xin,b,w){
  
}

f.prop=function(x,b,w){
  
}

b.prop=function(x,y,yhat,neg.logll){
  
  list(cost=cost,db=db,dw=t(dw))
}


num.gradient=function(cost,x,y,b,w,eps=1e-8,neg.logll=TRUE){
  
  return(list(db=db,dw=dw))
}

# should return cost, b,w, db,dw and history of these values for each iteration
log.fit = function(x,y,neg.logll,eta,max.its,tol,b.init=0,w.init=0){
  
}
