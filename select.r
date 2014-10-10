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
ess_s<-ess_s[ess_s$age>17,]
eds_s<-eds_s[eds_s$age>17,]

###GSS: waves >=2000
gss_s<-gss_s[gss_s$year>1999,]

###ESS: delete wave 1
ess_s<-ess_s[ess_s$essround>1,]

###Set MISSING-category for origin as NA
ess_s$origin[ess_s$origin=='MISSING']<-NA
eds_s$origin[eds_s$origin=='MISSING']<-NA
gss_s$origin[gss_s$origin=='MISSING']<-NA

###Load in listwise_deletion function
source('listwise_deletion.r',echo=TRUE)
variables<-c('id','dsid','weight','cons','migr','countries','origin','age','sex','year','education','employed','denomination','qlang')
variables.ext<-c(variables,'affiliated','praying')

d<-rbind(ess_s[!is.na(ess_s$origin),variables.ext],eds_s[!is.na(eds_s$origin),variables.ext],gss_s[!is.na(gss_s$origin),variables.ext])

