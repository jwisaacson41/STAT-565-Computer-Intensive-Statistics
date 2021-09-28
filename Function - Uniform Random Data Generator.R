### Random Uniform Generator
#' @Parameters
#'    - n (integer):  the number of observations to create
#'    - columns (integer): the number of variables/columns to create
#'    - min (integer): minimum boundary for uniform distribution
#'    - max (integer): maximum boundary for uniform distribution
#'    - x0 (logical): indicator to include an inital row for x_0 
#' 
#' @Output
#'    - matrix (n x columns or n x columns+1)


unif.datagenerator<-function(n, columns, min, max, x0){
  mtx<-matrix(NA, nrow=n, ncol=columns) # Build Matrix
  for(i in 1:columns){
    x<-runif(n, min, max) # Generate random uniform Numbers
    mtx[,i]<-x # Assign into Matrix
  }
  if(x0==TRUE){ # Logical condition to include/exclude the x0 column
    mtx.full<-cbind(matrix(1, nrow=n,1),mtx)
    } else{
    mtx.full<-mtx
    }
  return(mtx.full)
}
# End unif.datagenerator


