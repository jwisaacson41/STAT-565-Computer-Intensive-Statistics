### Random Uniform Generator
unif.datagenerator<-function(n, columns, min, max){
  mtx<-matrix(NA, nrow=n, ncol=columns) # Bulid Matrix
  for(i in 1:columns){
    x<-runif(n, min, max) # Generate random uniform Numbers
    mtx[,i]<-x # Assing into Matrix
  }
  return(mtx)
}
# End unif.datagenerator
