#Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#helper function to print functions
print.fun=function(x){header=deparse(args(x))[1]; b=body(x);  print(gsub('function',x,header));  print(b);}

show_digit2 = function(arr784, col=gray(12:1/12),...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, xaxt='n', yaxt='n',...)
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





library(zeallot)   # defines unpacking assignment %<-%

# binary classication data 
library(MASS)
set.seed(123)
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

sigmoid = function(x) 1 / (1 + exp(-x))

one.hot=function(y,classes=NULL){
  
}


fwd.prop=function(X,b,w){
}

bk.prop=function(X,Y,fprop){
  
}


cost=function(X,Y,b,w){
  
}



predict=function(X,b,w){
  
}

# X is the input data matrix with samples arranged in columns
# Y is the label matrix for X, one-hot encoded, samples arranged in columns
# w maps input vectors to output vectors. ncol(w)=nrow(X), nrow(w)=nrow(Y)
# b has an element for each output node. length(b)=nrow(Y)
softmax.fit=function(X,Y,lr=.01,max.its=100,b,w){
  
}



num.gradient=function(cost,x,y,b,w,eps=1e-8){
  
  return(list(db=db,dw=dw))
}


accuracy=function(X,Y,b,w){
  pred=predict(X,b,w)
  sum(pred == Y)/length(Y)
}


