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
gss$migr<-ifelse(gss$born=='YES',ifelse((gss$parborn=='BOTH IN U.S'),0,2),ifelse(!gss$parborn=='NEITHER IN U.S',0,1))
eds$numgen<-as.numeric(eds$GENSTAT)
eds$migr<-ifelse(eds$numgen==1,1,ifelse(eds$numgen==2,2,ifelse(eds$numgen==3,0,NA)))


###Destination
levels(ess$cntry)<-c(levels(ess$cntry),'United States','Canada')
eds$countries<-factor('Canada',levels=levels(ess$cntry))
gss$countries<-factor('United States',levels=levels(ess$cntry))
ess$countries<-ess$cntry


###Origin
eds$origin<-eds$EATC1
write(levels(eds$EATC1),file='eds_origins_old.txt')
levels(eds$origin)<-scan('eds_origins.txt', what='', sep='\n')

gss$origin<-gss$ethnic
write(levels(gss$ethnic),file='gss_origins_old.txt')
levels(gss$origin)<-scan('gss_origins.txt', what='', sep='\n')

ess$birthrp<-ifelse(ess$migr==1,ifelse(ess$essround==1,as.character(ess$cntbrth),ifelse(ess$essround<4,as.character(ess$cntbrtha),as.character(ess$cntbrthb))),as.character(ess$cntry))
ess$birthfa<-ifelse(ess$essround<4,as.character(ess$fbrncnt),as.character(ess$fbrncnta))
ess$birthmo<-ifelse(ess$essround<4,as.character(ess$mbrncnt),as.character(ess$mbrncnta))
ess$origin<-ifelse(ess$migr<2,as.character(ess$birthrp),ifelse(ess$facntr=='No',as.character(ess$birthfa),as.character(ess$birthmo)))
write(levels(ess$origin),file='ess_origins_old.txt')

