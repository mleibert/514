f <- function(x) cos(x)^4 - 4*cos(x)^3 + 8*cos(x)^2 - 5*cos(x) + 1/2

plot(f, xlim = c(0,5), ylim = c(-5,20) )
abline(h=0)

uniroot(f, c(0,2))

 curve(   dnorm(x,p2,s2)*m2 - dnorm(x,p1,s1)*m1, xlim= c(-.5,1.2) )
 abline( v =c(p1 ,p2 ), col = "blue" )


ff <- function(x){ dnorm(x,p2,s2)*4 -  dnorm(x,p1,s1)*m1  }
 
plot(ff , xlim= c( -.5, 1.2))

abline( h = 0 , col = "green" )

lower <- optimize( ff , lower =  0, upper = 1 , maximum = F )$minimum 
upper <- optimize( ff , lower =  .5, upper = 1 , maximum = T )$max

 abline( v = uniroot(ff,c(lower ,upper ))$root )

abline( v = optimize( ff , lower =  .5, upper = 1 , maximum = T )$max )
abline( v = optimize( ff , lower =  0, upper = 1 , maximum = F )$minimum )
 

uniroot(  dnorm(x,p1,s1)*m1 -  dnorm(x,p2,s2)*m2 , c(0,1) )