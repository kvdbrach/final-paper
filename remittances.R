remittances<-read.csv('remittances.csv',header=TRUE,sep=';')
library('countrycode')
remittances<-remittances[,-ncol(remittances)]
remittances$s.iso<-countrycode(remittances$Sending,'country.name','iso2c',warn=TRUE)

library('reshape')
remittances.m<-melt(remittances[,-1],id=c('s.iso'))
remittances.m$receiving<-gsub('\\.',' ',remittances.m$variable)
remittances.m$r.iso<-countrycode(remittances.m$receiving,'country.name','iso2c',warn=TRUE)
remittances.m$community<-paste(remittances.m$s.iso,remittances.m$r.iso,sep='')
remittances.m<-remittances.m[,c('community','value')]
colnames(remittances.m)[2]<-'remittances'
