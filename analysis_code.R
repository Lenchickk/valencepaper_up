library(rjags)
#library(arm)
library(coda)
library(superdiag)
library(R2WinBUGS)

#setwd("C:/Users/Lenchick/Google Drive/WASHU FALL2014/Third year paper/steps/jags/clean_output")
setwd("~/R/valencepaper")

iter=1000
source("spain2008.jags")

str_range = function(name, s, e)
{
  str=paste(name,"[",as.character(s),"]",sep="")
  for (i in (s+1):e)
  {
    str=c(str,paste(name,"[",as.character(i),"]",sep=""))
  }
  return(str)
}


d1 = d

d1$cPSOE=NULL
d1$cPP=NULL
d1$cIU=NULL

doRJags=function(model,dat,name,iter,sinkFlag=FALSE,sinkFile)
{
  if (sinkFlag) sink("output_model2008_simple1.txt", append=FALSE, split=FALSE)
  print(Sys.time())
  
  mspatial=NULL
  mspatial.out=NULL
  
  mspatial <- jags.model(file=model, data=dat,  n.chains=3,  n.adapt=0) 
  print(Sys.time())
  update(mspatial, n.iter=iter)
  print(Sys.time())
  mspatial.out<- coda.samples(model=mspatial, variable.names=name,n.iter=iter)
  print(Sys.time())
  print(summary(mspatial.out))
  print(Sys.time())
  print(superdiag(as.mcmc.list(mspatial.out),burnin=iter/2))
  if (sinkFlag) sink()
}


#setwd("C:/Users/Lenchick/Google Drive/WASHU FALL2014/Third year paper/steps/jags/clean_output")
sink("output_model2008_simple1.txt", append=FALSE, split=FALSE)
print(Sys.time())
mspatial=NULL
mspatial.out=NULL


mspatial <- jags.model(file="simpleVCL", data=d1,  n.chains=3,  n.adapt=0) 
update(mspatial, n.iter=iter)
mspatial.out<- coda.samples(model=mspatial, variable.names=name,n.iter=iter)
print(summary(mspatial.out))
print(superdiag(as.mcmc.list(mspatial.out),burnin=iter/2))
sink()


mspatial.out<- coda.samples(model=mspatial, variable.names=name,n.iter=iter)
print(summary(mspatial.out))

mspatial1.out<- coda.samples(model=mspatial, variable.names=name1,n.iter=iter)
print(summary(mspatial1.out))

print( asap2.dic <- dic.samples(mspatial, n.iter=iter, type="pD") )
#print(summary(dic.samples(mspatial, n.iter=iter, type="pD")))
superdiag(as.mcmc.list(mspatial.out),burnin=iter/2)
#superdiag(as.mcmc.list(mspatial1.out),burnin=iter/2)
sink()

#####################
setwd("C:/Users/Lenchick/Google Drive/WASHU FALL2014/Third year paper/steps")
print(Sys.time())
iter=12500
source("spain2011.jags")
d1=dd
rm(dd)
#pure spatial model, common coefficients (beta) for 2 ideological dimensions 
#write.model(basicVCL1,"basicVCL1")
#pure spatial model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions 
#write.model(basicVCL2,"basicVCL2")
#pure spatial model, conditional model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions,
#beta2 only for Catalona 9, Basque Country 16, Galicia 12, Valencia 10
#write.model(basicVCL3,"basicVCL3")
#pure spatial model, conditional model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions,
#beta2 only for Catalona 9, Basque Country 16, Galicia 12, Valencia 10, beta2s and beta1 are disting for the regions
#write.model(basicVCL4,"basicVCL4")
#pure spatial model, conditional model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions,
#beta2 only for Catalona 9, Basque Country 16, Galicia 12, Valencia 10, beta2s and beta1 are disting for the regions +  age, gender
#write.model(basicVCL5,"basicVCL6")

#for basicVCL1-4

mspatial=NULL
mspatial.out=NULL

#setwd("C:/Users/Lenchick/Google Drive/WASHU FALL2014/Third year paper/steps/jags/clean_output")
sink("output_model2011.txt", append=FALSE, split=FALSE)


name=c("beta1","beta2","lambda[1]","lambda[3]","lambda[4]","lambda[5]","lambda[6]","lambda[7]","lambda[8]","lambda[9]","mu[1,1]","mu[1,2]","mu[1,3]","mu[1,4]"
       ,"mu[9,1]","mu[9,2]","mu[9,3]","mu[9,4]","mu[9,5]","mu[9,9]","mu[12,1]","mu[12,2]","mu[12,3]","mu[12,4]","mu[12,8]",
       "mu[13,1]","mu[13,2]","mu[13,3]","mu[13,4]","mu[16,1]","mu[16,2]","mu[16,3]","mu[16,4]","mu[16,6]","mu[16,7]")

#name1=c("beta1","beta2","lambda[2]","lambda[3]","lambda[4]","lambda[5]","lambda[6]","lambda[7]","lambda[8]",
# "lambda[6]","lambda[7]","lambda[8]","lambda[9]","lambda[10]","lambda[11]",
#    "lambda[12]","lambda[13]","lambda[14]","lambda[15]","mu")

mspatial=NULL
mspatial.out=NULL

print(paste("iter=",iter))

mspatial <- jags.model(file="basicVCL2011", data=d1,  n.chains=3,  n.adapt=0) 
update(mspatial, n.iter=iter)
mspatial.out<- coda.samples(model=mspatial, variable.names=name,n.iter=iter)
print(summary(mspatial.out))
#print( asap2.dic <- dic.samples(mspatial, n.iter=iter, type="pD") )
#print(summary(asap2.dic))
superdiag(as.mcmc.list(mspatial.out),burnin=iter/2)
sink()
