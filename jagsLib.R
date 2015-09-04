library(rjags)
library(coda)
library(superdiag)
library(R2WinBUGS)

str_range = function(name, s, e)
{
  str=paste(name,"[",as.character(s),"]",sep="")
  for (i in (s+1):e)
  {
    str=c(str,paste(name,"[",as.character(i),"]",sep=""))
  }
  return(str)
}

doRJags=function(model,dat,name,iter,sinkFlag=FALSE,sinkFile)
{
  
  if (sinkFlag)
  {
    setwd("~/R/valencepaper/results")
    sink(sinkFile, append=FALSE, split=FALSE)
    setwd("~/R/valencepaper/models")
  }
  print(Sys.time())
  
  mspatial=NULL
  mspatial.out=NULL
  
  mspatial <- jags.model(file=model, data=dat,  n.chains=3,  n.adapt=0) 

  print(Sys.time())
  print(update(mspatial, n.iter=iter))
  print(Sys.time())
  mspatial.out<- coda.samples(model=mspatial, variable.names=name,n.iter=iter)
  print(Sys.time())
  print(summary(mspatial.out))
  print(Sys.time())
  print(superdiag(as.mcmc.list(mspatial.out),burnin=iter/2))
  if (sinkFlag) sink()
  return(0)
}