# @ Description
# A simulation to illustrate that p-value in a hypothesis test is uniformly
# distributed under H0

# @ Author: Jon Isaacson, jisaacson@uidaho.edu

# @ Parameters
# @ Return
# @ Last Change 9/6/2018
# Date and small description of change

#------------------------------------------------------------------------------

# Paths
BaseDir<-""
ScriptDir<-pasto(BaseDir, "/Code")
OutputDir<pasteo(BaseDir, "/Data")

# Libraries & Functions
# None Needed
source(paste0(ScriptDir, "/Sim")) # Inverse CDF method to to simulate
# exponential

source(paste0(ScriptDir, "/CalcZStat.R"))

# Seeds
set.seed(42)

# Input Parameters
N<-10^4
n<-10^2
lambdamax=10

z<-NULL
p<-NULL

pb = txtProgressBar(min = 0, max = N, initial = 0) 

# Output Parameters

for(i in 1:N){
  lambda.star<-runif(10,0,lambdamax)
  x.star<-rexp(n,lambda.star)
  z.star<-(x.star-(1/lambda.star))/(sd(x.star)/sqrt(n))
  p.star<-pnorm(z.star, 0, 1)
  z<-c(z, z.star)
  p<-c(p, p.star)
  setTxtProgressBar(pb,i)
  
}
summary(p.star)
hist(p.star, breaks=30)


# Input Parameters
N<-10^4
n<-10^2
lambdamax<-10

# Output Parameters
lambda<-c()
p<-c()
x<-matrix(NA,N,n)
z<-c()


parameters<-list()
data<-list()

#------------------------------------------------------------------------------
# Code Body


# lambda<-runif(N,0, lambdamax) (optional) # Optional method to write it
for(i in 1:N){
  lambda[i]<-runif(1, 0, lambdamax)
  x[i,]<-SimExp(n, lambda[i])
  z[i]<-CalcZStat(x[i,], lambda[i])
  p[i]<-pnorm(z[i], lower.tail = F)
}



parameters[[1]]<- N
parameters[[2]]<- n
parameters[[3]]<- lambdamax
names(parameters)<-c("N: Total Simulations",
                     "n :Simulated Exponential",
                     "lambdamax: Max Lambda")

data[[1]]<-lambda
data[[2]]<-x
data[[3]]<-z
data[[4]]<-p
names(data)<-c("Lambda: Lambda Values", "x: Simulated Exponential Value", 
               "z: Z-Statistic", "p: P-Value")


save(parameters, file = paste0(OutputDir, "/parameters.csv"))
save(data, file = paste0(Outputdir, "/Parameters File.csv"))

hist(p, 20)
# End Main.R



