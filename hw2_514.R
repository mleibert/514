#Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#helper function to print functions
print.fun=function(x){header=deparse(args(x))[1]; b=body(x);  print(gsub('function',x,header));  print(b);}


# randomly place points in the box xlim=c(0,1),ylim=c(0,3)
# The two classes have a minimum distance of 2*gamma
# If gamma < 0, then the classes are not separable
perceptron.box.data=function(n,gamma=.25,seed=NULL){
  require(zeallot)
  if(!is.null(seed)) set.seed(seed)
  data=matrix(0,nrow=n,ncol=3)
  # the discriminant mid-point line runs from (0,1) to (1,2) (slope 1)
  discriminant=function(x,y){(1+x-y)/sqrt(2)}
  m=0
  while(m < n){
    x=runif(1,0,1)
    y=runif(1,0,3)
    d=discriminant(x,y)
    d1=d >= gamma
    d2=d <= -gamma
    if(d1 & !d2){
      m=m+1
      data[m,] %<-% c(x,y,+1)
    }else if(d2 & !d1){
      m=m+1
      data[m,] %<-% c(x,y,-1)
    }else if(d1 & d2){
      m=m+1
      data[m,] %<-% c(x,y,sample(c(-1,1),1))
    }
  }
  data
}


plot.perceptron.box.data=function(data,title='perceptron'){
  col=data[,3]+3
  col=c("red","blue")[col/2]
par(mar=c(6.1, 4.1, 4.1, 2.1), xpd=F)

  plot(data[,1],data[,2],type="p",col=col,pch=16,main=title,ylim=c(min(data[,2]),max(data[,2])),xlim=c(min(data[,1]),max(data[,1])))
}

#this function should return a list with w (weights, b= intercept, mistakes=# of mistakes counter)
perceptron.train=function(x,y,epoch=100){
  
  return(list(w=w,b=b,mistakes=mistakes))
}

#this function applies the perceptron training w,b to x data to classify points
# it should return predictions of the perceptron

predict.perceptron=function(w,b,x){
}


# This function should take data, num_trials, and epochs and return a vector of lists called results, 
# storing the output of the perceptron train data
run.experiments=function(data,num.trials=1,epochs=100){
  results=vector(mode="list")
  
  return(results)  
}


#This function should return the w,b, and mistakes from the avg perceptron algorithm in a list just like perceptron train
avg.perceptron.train=function(x,y,iter=100){
  
  
}




#this function takes x,y data and returns and the w projection for the fisher discriminant and global mean m
fisher=function(x,y){
  
  return(list(w=w,m=m))
}


#This function computes the decision boundary line slope a and intercept b for the fisher discriminant
fisher.2d.line=function(w,m){
  a=0
  b=0
  if(length(w)!=2) stop("Fisher fit is not 2d")
  wm = sum(w*m);
  
  a=wm/w[2];
  b=-w[1]/w[2];  # not sure if the a, b do anything
  return(list(a=a,b=b))
}


# This function uses the lists of wgts, cs, bs to apply voted perceptron to classify a single observation x 
# and returns the prediction
compute_vote=function(wgts,cs,bs,x)
{
  
}


#The voted perceptron train function should apply the voted perceptron on x,y
#This function should return a list of wgts weights made along the way,
#list of called cs the number of times the weight survives before being changes,
#a list called bs containing intercept weight updates and mistakes the # of mistakes
freund.voted.perceptron.train=function(x,y,iter=200){
  
}

#this function will apply the wgts, cs and bs to the entire data set
predict.voted=function(wgts,cs,bs,x){
  
  
}
