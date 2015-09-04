library(psych)
library(mice)

create_party_means = function(d, np)
{
  PP = array(1:np*2,dim=c(2,np))
  
  for (i in 1:np)
  {
    PP[1,i] = mean(d[d$v==i,]$x)
    PP[2,i] = mean(d[d$v==i,]$y)
  }
  
  return(PP)
}

get_rid_of_novote = function(d)
{
  
  return(d[d$vote<15 & d$vote>0,])
}


process = function(data)
{
  data=get_rid_of_novote(data)
  
  data=update_NA(data)
}

update_NA = function(data)
{
  #data=data[data$CCAA!=17,]

  
  data$multicultural[data$multicultural==99 ] <- NA
  data$multicultural[data$multicultural==98 ] <- NA
  
  data$economy[data$economy==99 ] <- NA
  data$economy[data$economy==98 ] <- NA
  
  data$homosexuals[data$homosexuals==99 ] <- NA
  data$homosexuals[data$homosexuals==98 ] <- NA
  
  data$antiterrorist[data$antiterrorist==99 ] <- NA
  data$antiterrorist[data$antiterrorist==98 ] <- NA
  
  data$decentralization[data$decentralization==99 ] <- NA
  data$decentralization[data$decentralization==98 ] <- NA
  
  data$national_id[data$national_id==99 ] <- NA
  data$national_id[data$national_id==98 ] <- NA
  
  data$proud_Spain[data$proud_Spain==99 ] <- NA
  data$proud_Spain[data$proud_Spain==98 ] <- NA
  
  data$candidate_PSOE[data$candidate_PSOE==99 ] <- NA
  data$candidate_PSOE[data$candidate_PSOE==98 ] <- NA
  
  data$candidate_PP[data$candidate_PP==99 ] <- NA
  data$candidate_PP[data$candidate_PP==98 ] <- NA
  
  data$candidate_IU[data$candidate_IU==99 ] <- NA
  data$candidate_IU[data$candidate_IU==98 ] <- NA
  data$candidate_IU[data$candidate_IU==97 ] <- NA
  
  data$candidate_IU[data$proud_Spain==99 ] <- NA
  data$candidate_IU[data$proud_Spain==98 ] <- NA

  return(data)
}

create_phi2008_r_1 = function()
{
  phi=NULL
  for (i in 1:16)
  {
    phi=rbind(phi,c(1,1,1,1,rep(0,8)))
  }
  phi[2,11] =phi[1,11]=1
  phi[5,9] = 1
  phi[9,5] = phi[9,6] = 1
  phi[12,8] = 1
  phi[15,12]=1
  phi[16,7] = phi[16,10] = 1
  return (as.matrix(phi))
}

create_phi2011_r_1 = function()
{
  phi=NULL
  for (i in 1:16)
  {
    phi=rbind(phi,c(1,1,1,1,rep(0,6),1,rep(0,3)))
  }
  phi[2,13] =1
  phi[3,12] = 1
  phi[5,10] = 1
  phi[9,6] = phi[9,7] = 1
  phi[12,9] = 1
  phi[16,5]=phi[15,5]=1
  phi[16,8] = 1
  phi[15,14]=1
  return (as.matrix(phi))
}





code_region = function(type,data)
{
  if (type==1) 
  {
    return(data[data$CCAA!=17,])
  }
  
  if (type==2)
  {
    data$CCAA[data$CCAA==14]=7
    data$CCAA[data$CCAA==17]=8
    return(data)
  }
  
  g1 = data$CCAA==3 | (data$CCAA>4 & data$CCAA<9) | data$CCAA==10 | data$CCAA==11 | data$CCAA==14 | data$CCAA==17
  data[g1,]$CCAA=1
}

post_update=function(d)
{
  d$region[1044]=2
  d$region[1132]=9
  d$region[3908]=9
  d$region[4228]=15
  return(d)
}

create_fulldata_from_file = function(file,type_region,np,year=2008,echo=TRUE)
{
  d = read.csv(file)
  d = get_rid_of_novote(d)
  d=update_NA(d)
  d=code_region(type_region,d)
  
  n = nrow(d)

  params=as.data.frame(cbind(d$multicultural, d$economy, d$homosexuals, d$antiterrorist, d$decentralization, d$national_id, d$proud_Spain, d$candidate_PSOE, d$candidate_PP, d$candidate_IU))
  
  if (echo) 
  {
    print("Pre-imputation")
    print(describe(params))
  }
  
  params = complete(mice(params, seed=1234))
  
  if (echo) 
  {
    print("After imputation")
    print(describe(params))
  }
  
  issues= params[,1:7]
  personal = params[,8:10]
  
  factors_issues=factanal(issues,2,scale=TRUE,rotation="varimax",scores="reg")
  print("Factor analysis")
  print(factors_issues)
  
  d = as.data.frame(cbind(d$CCAA,d$vote,d$weight,as.vector(factors_issues$scores[,1]),as.vector(factors_issues$scores[,2]),personal))
  names(d) = cbind("r","v","w","x","y","cPSOE","cPP","cIU")
  print("means by party")
  PP = create_party_means(d,np)
  print(PP)
  
  dd = NULL
  dd$sec=d$x
  dd$lr=d$y
  dd$vote = d$v
  dd$region = d$r
  dd$cPSOE=d$cPSOE
  dd$cPP=d$cPP
  dd$cIU =d$cIU
  dd$N=n
  dd$P=np
  dd$R=16
  dd$psec=NULL
  dd$plr=NULL
  
  for(i in 1:np) 
  { 
    
    dd$psec=c(dd$psec,PP[1,i])
    dd$plr=c(dd$plr,PP[2,i])
  }
  
  if (year==2008) phi = create_phi2008_r_1()
  else phi =create_phi2011_r_1()
  
  print("Phi matrix")
  
  print(phi)
  
  dd$phi=phi
  
  print("")
  
  print("Distribution by region")
  
  print(table(dd$r))
  
  print("")
  
  print("Dstribution by party")
  
  print(table(dd$v))
  
  dd$r = NULL
  
  if (year==2008) dd=post_update(dd)
  
  return(dd)
}


dump_jagsfile = function(inn,out,np,sink=FALSE,year=2008)
{
  
  if (sink) sink(paste(out,"log.txt"), append=FALSE, split=FALSE)
  d = create_fulldata_from_file(inn,1,np,year)
  dump("d",out)
  if (sink) sink()
}



dump_jagsfile("~/R/valencepaper/data/PrePost2008_Spain_sel.csv","spain2008.jags",12,TRUE)
#dump_jagsfile("~/R/valencepaper/data/PrePost2011_Spain_sel.csv","spain2011.jags",14,TRUE,2011)








