# @ Description
# Simulates draws from the exponential distribution when its parameter,
# is also a uniformly drawn random variable

# @ Parameters
#     n: scalar. Sample Size
#     lambda: scalar (1xn). Exponential Parameter

SimExp<-function(n, lambda){
  u<-runif(n)
  x = -(1/lambda)*log(u)
  return(x)
}
# End SimExp
