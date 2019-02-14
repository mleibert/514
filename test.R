#	rm(list = ls())


bayes.discriminant = function(x,n,m1,p1,m2,p2){
	s1=sqrt(p1*(1-p1)/n); s2=sqrt(p2*(1-p2)/n)
	dnorm(x,p2,s2)*m2 - dnorm(x,p1,s1)*m1
}

 
bayes.cut.norm=function(n,m1,p1,m2,p2){
	ff=function(x){bayes.discriminant(x,n,m1,p1,m2,p2)}
	uniroot(ff,c(p1,p2))$root
}


bayes.cut.norm(6 , 4 , .35 , 6, .5)

n=6 
m1=4; p1=.35;
m2=6; p2=.5;
 

S2 <- sqrt(p2*(1-p2)/n)
S1 <- sqrt(p1*(1-p1)/n)

fff <- function(x ){ dnorm(x,p2,S2)*m2 - dnorm(x,p1,S1)*m1 }
plot(fff)
uniroot(fff, c(p1,p2) )


abline( v = c(p1,p2) , col = "green")
abline( h = 0 , col = "red" )


# finding the right starting points for optimize is not easy
# brute way to find decent starting points
Low <- (-10:10)[which( fff( -10:10 )  < 0 )][1] #Where to find lower bound?
Up <- (Low:10)[which( fff(  Low:10 )  > 0 )][1]  #Where to find Upper bound?

Low <- optimize( fff, lower = Low , upper = Up  )$minimum
Up <-  optimize( fff , lower = Low , upper = Up , maximum = T )$max

abline( v =  Low , col = "blue" )
abline( v =  Up, 	 col = "blue" )
abline( v = uniroot(fff,c(Low ,Up))$root, col = "purple" )
uniroot(fff,c(Low ,Up))$root









 
uniroot(fff,c(Low ,Up))$root