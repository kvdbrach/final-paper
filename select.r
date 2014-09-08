###Create editable dataframes, keep main dataframes intact
ess_s<-ess
eds_s<-eds
gss_s<-gss

###Remove non-European countries + Ukraine (MIPEX missing)
ess_s<-ess_s[ess_s$cntry!='Turkey',]
ess_s<-ess_s[ess_s$cntry!='Israel',]
ess_s<-ess_s[ess_s$cntry!='Russia',]
ess_s<-ess_s[ess_s$cntry!='Ukraine',]

###Age: +18
#ess_s<-ess_s[ess_s$age>17,]
#eds_s<-eds_s[eds_s$age>17,]

###GSS: waves >=2000
#gss_s<-gss_s[gss_s$year>1999,]

###ESS: delete wave 1
#ess_s<-ess_s[ess_s$essround>1,]

###Select only migrants
ess_s<-ess_s[ess_s$migr>0,]
eds_s<-eds_s[eds_s$migr>0,]
gss_s<-gss_s[gss_s$migr>0,]

###Load in listwise_deletion function
source('listwise_deletion.R',echo=TRUE)
variables<-c('id','dsid','weight','cons','migr','countries','origin','age','sex','year','education','employed')
variables.ext<-c(variables,'affiliated','praying')

d<-rbind(ess_s[,variables.ext],eds_s[,variables.ext],gss_s[,variables.ext])
d.affiliated<-listwise_deletion(d,c(variables,'affiliated'))
d.praying<-listwise_deletion(d,c(variables,'praying'))
