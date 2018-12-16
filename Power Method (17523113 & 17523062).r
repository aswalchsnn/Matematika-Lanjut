
#Aswal Chusnan W (17523113)
#Adhin Alifarchan (17523062)
library(matlib)

#1
A <- matrix(c(4,2,-5,-3), 2,2,FALSE)
A

b <- c(1,0)
b

C <- A%*%b
C

powerMethod(A,v = b,maxiter = 5,FALSE)

#2
B <- matrix(c(0,11,-5
             ,-2,17,-7
             ,-4,26,-10),3,3,TRUE)
B
e <- c(1,1,1)
e

D <- B%*%e
D

powerMethod(B,v = e,maxiter = 5,FALSE)
