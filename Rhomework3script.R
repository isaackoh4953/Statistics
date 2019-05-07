### This function is to find the least square solution ###
install.packages('matlib')
library(matlib)
inv(t(X)%*%X)
leastsquare <- function(X,y){
  B<- inv(t(X)%*%X) %*% t(X) %*% y
  return(B)
}
leastsquare(X,y)  
### This function is to create the Covariance Matrix ###
Covariance <- function(X,y){
  n <- nrow(X)
  p <- ncol(X)
  sigma <- sum((y - X%*%leastsquare(X,y))^2)/(n-p)
  Cov <- sigma * solve(t(X) %*% X)
  return(Cov)
}  
length(X[1,])
Covariance(X,y) 

sqrt(diag(Covariance(X,y))) #If you want to find the standard error  
tstatistics(X,y)
  
A <- matrix(1:4,2)
b<- c(5,6)

tstatistics <- function(X,y){
  n <- length(X[1,])
  a <- c()
  B <-leastsquare(X,y)
  Covar <- Covariance(X,y)
  i <- 1
  while(i < n+1){
    a[i] <- B[i]/ sqrt(Covar[i,i])                   
    i <- i+1
  }
return(a)  
}

ncol(X)

degreeoffreedom <- function(x,y){
  n <- nrow(X)
  p <- ncol(X)
  return(n-p)
}
degreeoffreedom(X,y)


pvalue <- function(X,y){
  a <- tstatistics(X,y)
  b <- c()
  d <- degreeoffreedom(X,y)
  i <- 1
  while(i < length(a)+1){
    b[i] <-  2 * pt(-abs(a[i]), d-1)
    i <- i + 1  
  }
  return(b)
}

pvalue(X,y)

n = 200
p = 5
X = matrix(rnorm(n*p), nrow = n, ncol = p) # design matrix
beta = matrix(runif(p),p,1)
e = matrix(rnorm(n),n,1)
y = X%*%beta + e
df = as.data.frame(cbind(y,X))
names(df) = c("y",paste("X", 1:5, sep=""))
fit = lm(y~., data = df) # you should use your own code to replace lm()
summary(fit)
beta #you may compare your estimate with the true beta

t.test(beta,y = NULL, alternative = c("two.sided"), conf.level = 0.95,)