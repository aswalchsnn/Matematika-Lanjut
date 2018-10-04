
#1) constant functions
#f(x) = c
#f(x) = 10

f <- function(x){
    fx <- 10
    return(fx)
}
input <- -8:8
plot(input,
sapply(input,f),
type="l" , xlab="x",
ylab = "f(x)")

#2) linear functions
#f(x) = ax+b
#f(x) = 5x+12

f <- function(x){
    fx <- 5*x + 12
    return (fx)
} 
input <- -4:8
plot(input,
sapply(input,f),
type = "l",
xlab = "x" , ylab = "f(x)")

#3) quadratic functions
#f(x) = ax^2 + bx + c
#f(x) = 4x^2 + 6x + 8

f <- function(x){
    fx <- 4*x^2 + 6*x + 8
    return(fx)
}
input <- -50:65
plot(input,
sapply(input,f),
type="l",
xlab="x",ylab="f(x)")

#4) polynomial functions
#f(x) = 3x^3 - 4x^2 + 5x-1

f <- function(x){
    fx <- 3*x^3 - 4*x^2 + 5*x-1
    return(fx)
}
input <- seq(-18,15,0.1)
plot(input,
sapply(input,f),
type = "l",
xlab = "x", ylab = "f(x)")

#5) rational function
#f(x) = p(x)/q(x)
#f(x) = 8/x

f <- function(x){
    fx <- 8/x
    return(fx)
}
input <- seq(2, 16, 0.2)
plot(input,
sapply(input, f),
type="l",
xlab = "x", ylab = "f(x)")
