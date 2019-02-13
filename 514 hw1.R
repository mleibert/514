rm(list = ls())
setwd("G:\\math\\661")

coin.toss=function(p,num.tosses,num.trials){
draws=runif(num.tosses*num.trials)
results=matrix(1*(draws < p),nrow=num.trials)
return(results)
}


create.coin.toss.data=function(n,m1,p1,m2,p2){
c1=coin.toss(p1,n,m1)
c2=coin.toss(p2,n,m2)
data=rbind(c1,c2)
y = c(rep(0,m1),rep(1,m2))
# what do the following lines of code do?
i = sample(1:nrow(data))
return(list(x=data[i,],y=y[i]))
}


n=6 
m1=4; p1=.35;
m2=6; p2=.5;

# n=64
# m1=40; p1=.42;
# m2=60; p2=.43;

bayes.discriminant = function(x,n,m1,p1,m2,p2){
	s1=sqrt(p1*(1-p1)/n); s2=sqrt(p2*(1-p2)/n)
	dnorm(x,p2,s2)*m2 - dnorm(x,p1,s1)*m1
}

 
bayes.cut.norm=function(n,m1,p1,m2,p2){
	ff=function(x){bayes.discriminant(x,n,m1,p1,m2,p2)}
	uniroot(ff,c(p1,p2))$root
}




bayes.cut.norm(n,m1,p1,m2,p2)

curve( dnorm(x,p2,sqrt(p2*(1-p2)/n))*m2 - dnorm(x,p1,sqrt(p1*(1-p1)/n))*m1 ) 
abline( v = c(p1,p2) , col = "green")
abline( h = 0 , col = "pink" )


############
rm(list = ls())

coin.toss = function(p,num.tosses,num.trials){
  draws=runif(num.tosses*num.trials)
  results=matrix(1*(draws < p),nrow=num.trials)
  return(results)
}

n = 8	
m1 = 6
p1 = .35
m2 = 4
p2 = .43
 

bayes.discriminant=function(x,n,m1,p1,m2,p2){
	s1=sqrt(p1*(1-p1)/n); s2=sqrt(p2*(1-p2)/n)
	dnorm(x,p2,s2)*m2 - dnorm(x,p1,s1)*m1
}

S1=sqrt(p1*(1-p1)/n); S2=sqrt(p2*(1-p2)/n)  
fff <- function(y){ dnorm(y,p2,S2)*4 -  dnorm(y,p1,S1)*m1  }

plot(fff , xlim= c( -.5, 1.2))


# finding the right starting points for optimize is not easy
# brute way to find decent starting points
Low <- (-10:10)[which( fff( -10:10 )  < 0 )][1] #Where to find lower bound?
Up <- (Low:10)[which( fff(  Low:10 )  > 0 )][1]  #Where to find Upper bound?

Low <- optimize( fff, lower = Low , upper = Up  )$minimum
Up <-  optimize( fff , lower = Low , upper = Up , maximum = T )$max

abline( v =  Low , col = "blue" )
abline( v =  Up, 	 col = "blue" )
abline( h = uniroot(fff,c(Low ,Up))$root, col = "pink" )
 uniroot(fff,c(Low ,Up))$root


bayes.cut.norm=function(n,m1,p1,m2,p2){
	ff=function(x){bayes.discriminant(x,n,m1,p1,m2,p2)}
	lower <- optimize( ff , lower = -10, upper = 10  )$minimum 
	upper <- optimize( ff , lower = lower , upper = 10 , maximum = T )$max
	uniroot(ff,c(lower ,upper ))$root
	plot(ff,  xlim = c(-.5,2)); abline(h=0);
	abline(v=uniroot(ff,c(lower ,upper ))$root, col = "green")
	abline( v =c(lower ,upper ), col = "blue" )
}

bayes.cut.norm(n,m1,p1,m2,p2)