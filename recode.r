###Change DataFilenames if needed
ess<-ess
gss<-gss
eds<-eds

###Add constants to each dataset
ess$cons<-1
gss$cons<-1
eds$cons<-1

###Praying 
###Convert to dichotomous variables
###Categories:0: Less than once a week 2: At least once a week

eds$praying<-ifelse(as.numeric(eds$BK_Q135)>5,NA,as.numeric(eds$BK_Q135))
eds$praying<-ifelse(eds$praying>1,0,1)

ess$praying<-ifelse(as.numeric(ess$pray)>3,0,1)

gss$praying<-ifelse(as.numeric(gss$pray)>4,0,1)

###Affiliation
eds$RELSUM<-ifelse(as.numeric(eds$RELSUM)>12,NA,eds$RELSUM)
eds$affiliated<-ifelse(eds$RELSUM==1,0,1)

gss$affiliated<-ifelse(as.numeric(gss$relig)==4,0,1)

ess$affiliated<-ifelse(as.numeric(ess$rlgblg)==2,0,1)


###Migrants
ess$migr<-ifelse(ess$brncntr=='Yes',ifelse((ess$facntr=='Yes' & ess$mocntr=='Yes'),0,2),ifelse((ess$facntr=='Yes' | ess$mocntr=='Yes'),0,1))

###Destination
levels(ess$cntry)<-c(levels(ess$cntry),'United States','Canada')
eds$countries<-factor('Canada',levels=levels(ess$cntry))
gss$countries<-factor('United States',levels=levels(ess$cntry))
ess$countries<-ess$cntry

