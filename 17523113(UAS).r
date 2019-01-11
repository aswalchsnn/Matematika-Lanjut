#1
data1 <- read.csv(file.choose(),header=TRUE)
data1

model <-lm(y ~ x, data=data1)
summary(model)

#2
y <- function(x){
  hasil <- (-93.6 +( 2.7*x))
}
print (y(55))

#3
library(polynom)
x <- c(0,1,2,3,4)
y <- c(1,2.25,3.75,4.25,5.65)

data2 <- data.frame(cbind(x,y))
poly.calc(x,y)

#5
f <- function(x,y){
  hasil<- x^2
  return(hasil)
}
plot(x,y)
curve(f, add = TRUE)

#6
fx <- function(x) {
  return(x^3 + 4*x^2 - 10)
}


#Bisection
bi <- function(a, b) {
  
  re <- 3
  
  pn <- (a + b) / 2
  
  
  while(re >= 0.0001) {
    
    print(paste(a, b, pn, fx(pn), fx(a), re, sep=" "))
    
    p <- pn
    
    if (sign(fx(p)) == sign(fx(a))) {
      a <- p
    } else {
      b <- p
    }
    
    pn <- (a + b) / 2
    
    re <- abs(pn-p) / abs(pn)
  }
  print(paste(a, b, pn, fx(pn), fx(a), re, sep=" "))
}


#11
library(pracma)

f1 <- function(x){
  hasil <- x^2 - 6
  return(hasil)
}

trapzfun(f1,0,1)


#12
F2 <- function(x){
  hasil <- (x^3 + 4*(x^2) -10)
  return(hasil)
}

trapzfun(f2,1,2)

#13,14
h<- 0.1
x<- seq(0,1,by=h)
f<-function(x){
  return(x^2)
}

f0 <- f(x[1])
fi <- sapply(x[2:10],f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  L <- h *(f0+ (2* sum(fi)) + fn)/2
  return(L)
}

trap(f0,fi,fn,h)

#15
h<- 0.2
x<- seq(0,1,by=h)
f<-function(x){
  return(x^2)
}

f0 <- f(x[1])
fi <- sapply(x[2:5],f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  L <- h/2 *(f0+ (2* sum(fi)-1) + fn)
  return(L)
}

trap(f0,fi,fn,h)

