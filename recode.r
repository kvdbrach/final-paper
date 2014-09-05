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
ess$origin<-as.factor(ifelse(ess$migr<2,as.character(ess$birthrp),ifelse(ess$facntr=='No',as.character(ess$birthfa),as.character(ess$birthmo))))
write(levels(ess$origin),file='ess_origins_old.txt')
levels(ess$origin)<-scan('ess_origins.txt',what='',sep='\n')


###Demographics
###Age: Recode EDS to numerical variable
eds$agenumeric<-as.numeric(eds$AGES)
eds$age<-as.numeric(ifelse(eds$agenumeric==1,16,ifelse(eds$agenumeric==2,21,ifelse(eds$agenumeric==3,27,ifelse(eds$agenumeric==4,32,ifelse(eds$agenumeric==5,40,ifelse(eds$agenumeric==6,50,ifelse(eds$agenumeric==7,60,80))))))))
ess$age<-ess$agea
gss$age<-gss$age


###Sex: dichotomous, male=reference category
eds$sex<-as.numeric(eds$SEX)-1
gss$sexbackup<-gss$sex
gss$sex<-as.numeric(gss$sex)-1
ess$sex<-as.numeric(ess$gndr)-1


###Year:
eds$year<-2002
gss$year<-gss$year
ess$year<-(ess$essround*2)+2000


###Education:
eds$education<-as.factor(ifelse(as.numeric(eds$HLOS)>7,NA,eds$HLOS))
levels(eds$education)<-levels(eds$HLOS)
#PHD: 26 years
#Bachelor: 16 years
#CEGEP: 16 years
#some university: 15 years
#some college: 15 years
#high school: 14 years
#less than high school: 8 years
eds$schooling<-as.numeric(eds$HLOS)
eds$education<-ifelse(eds$schooling==1,26,ifelse(eds$schooling<4,16,ifelse(eds$schooling<6,15,ifelse(eds$schooling==6,14,8))))
gss$education<-gss$educ
ess$education<-ess$eduyrs


###Employment
eds$employed<-ifelse(as.numeric(eds$MAINACTS)==1,1,ifelse(as.numeric(eds$MAINACTS)>4,NA,0))
gss$employed<-ifelse(as.numeric(gss$wrkstat)<3,1,0)
ess$employed<-ifelse(as.numeric(ess$mnactic)==1,1,0)


###Calculate WVS and EVS variables
###Select US and Canada from WVS and discard other countries
wvs<-wvs[wvs$V2=='United States' | wvs$V2=='Canada',]

###Only select natives to calculate variables
wvs<-wvs[wvs$V215=='Not an immigrant' & wvs$V216=='Not an immigrant',]
wvs<-wvs[wvs$V237>17,]
evs<-evs[evs$v309=='yes' & evs$v311=='yes' & evs$v306=='yes',]

###Add countries-variable to enable merge
wvs$countries<-wvs$V2
evs$countries<-evs$country
levels(evs$countries)[levels(evs$countries)=='Great Britain']<-'United Kingdom'
levels(evs$countries)[levels(evs$countries)=='Slovak Republic']<-'Slovakia'


###Dislike immigrants as neighbors
wvs$mneigh<-abs(as.numeric(wvs$V37)-2)
evs$mneigh<-as.numeric(evs$v54)-1

disneigh<-rbind(aggregate(mneigh~countries,wvs,mean),aggregate(mneigh~countries,evs,mean))

###Prefer nationals when jobs are scarce
wvs$njobs<-ifelse(as.numeric(wvs$V45)==1,1,0)
evs$njobs<-ifelse(as.numeric(evs$v102)==1,1,0)

disjobs<-rbind(aggregate(njobs~countries,wvs,mean),aggregate(njobs~countries,evs,mean))

###Religiosity
praying<-rbind(aggregate(praying~countries,gss[gss$migr==0,],mean),aggregate(praying~countries,eds[eds$migr==0,],mean),aggregate(praying~countries,ess[ess$migr==0,],mean))
affiliated<-rbind(aggregate(affiliated~countries,gss[gss$migr==0,],mean),aggregate(affiliated~countries,eds[eds$migr==0,],mean),aggregate(affiliated~countries,ess[ess$migr==0,],mean))
religion<-merge(praying,affiliated)


#Read in MIPEX-data
#Obtained from mipex.eu
mipex<-read.table('mipex.csv',header=TRUE,sep=";")


###Merge all to destination-side dataset
destination<-merge(merge(merge(disneigh,disjobs),mipex),religion,all.x=TRUE,all.y=TRUE)
