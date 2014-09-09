hdi<-read.csv('hdi.csv',sep=';')
colnames(hdi)[2:14]<-gsub('X','hdi_',colnames(hdi)[2:14])

###Using countrycode library
library('countrycode')

###Add origin-variable to merge and fill with country-codes
hdi$origin<-countrycode(hdi$Country,'country.name','iso2c',warn=TRUE)
hdi[hdi$Country=='South Sudan','origin']<-'SS'

###Merge with individual dataset to list missings
merged<-merge(d,hdi,all.x=TRUE)
missing_origins<-aggregate(hdi_2013~origin,merged,function(x) sum(is.na(x)),na.action=na.pass)
missing_origins<-missing_origins[missing_origins$hdi_2013>0,]
#missing_origins$name<-countrycode(missing_origins$origin,'iso2c','country.name',warn=TRUE)
#write.csv(missing_origins,'missing_origins.csv')

#write(levels(d$origin),'global_origins_old.txt')


hdi<-hdi[,c('origin','hdi_2010')]
hdi$hdi_2010<-ifelse(hdi$hdi_2010>=1,NA,hdi$hdi_2010)


colnames(missing_origins)[2]<-'hdi_2010'
hdi<-merge(hdi,missing_origins,all.x=TRUE,all.y=TRUE)
hdi$hdi_2010<-ifelse(hdi$hdi_2010>=1,NA,hdi$hdi_2010)
hdi2<-hdi

hdi2$continents<-countrycode(hdi$origin,'iso2c','continent',warn=TRUE)
hdi2$regions<-countrycode(hdi$origin,'iso2c','region',warn=TRUE)

hdi[hdi$origin=='Africa','hdi_2010']<-mean(hdi2[hdi2$continents=='African','hdi_2010'],na.rm=TRUE)
