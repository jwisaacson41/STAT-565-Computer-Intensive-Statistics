CalcZStat<-function(x, lambda){
  n = length(n)
  z = (mean(x)-(1/lambda))/(sd(x)/sqrt(n))
}
# End CalcZStat.R