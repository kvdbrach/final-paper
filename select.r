###Remove non-European countries + Ukraine (MIPEX missing)
ess<-ess[ess$cntry!='Turkey',]
ess<-ess[ess$cntry!='Israel',]
ess<-ess[ess$cntry!='Russia',]
ess<-ess[ess$cntry!='Ukraine',]

###Age: +18
ess<-ess[ess$age>17,]
eds<-eds[eds$age>17,]

###GSS: waves >=2000
gss<-gss[gss$year>1999,]

###ESS: delete wave 1
ess<-ess[ess$essround>1,]


###Load in listwise_deletion function
source('listwise_deletion.R',echo=TRUE)
variables<-c('cons')
