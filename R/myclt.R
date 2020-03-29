#' Central Limit Theorem Function
#'
#' @param n Represent the number of y's used in each sample
#' @param iter Represents the number of columns i.e. the number of samples
#' @param a parameter of uniform distribution
#' @param b second parameter of uniform distribution
#'
#' @return summation of the y's over iter samples with n y's per sample. The higher the value of n, the closer the resulting distribution will be to a normal distribution
#' @export
#'
#' @examples
#' w=myclt(n=50,iter=10000,a=5,b=10)
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}

