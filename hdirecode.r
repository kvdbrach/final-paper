hdi<-read.csv('hdi.csv',sep=';')
colnames(hdi)[2:14]<-gsub('X','hdi_',colnames(hdi)[2:14])

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

###Recode countries and regions
iso.codes<-c(countrycode_data[countrycode_data$continent=='Africa','iso2c'])
hdi[hdi$origin=='African','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
hdi[hdi$origin=='Czechoslowakia','hdi_2010']<-mean(c(hdi[hdi$origin=='CZ','hdi_2010'],hdi[hdi$origin=='SK','hdi_2010']))
iso.codes<-c(countrycode_data[countrycode_data$region=='South America','iso2c'],countrycode_data[countrycode_data$region=='Central America','iso2c'])
hdi[hdi$origin=='Latin, Central and/or South American','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c('BH','IQ','IL','JO','KW','LB','OM','PS','QA','SA','SY','AE','YE')
hdi[hdi$origin=='Other Arab','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c('RS','ME','HR','SI','BA','MK')
hdi[hdi$origin=='Yugoslavia','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c('JP','KP','KR','MO','MN','TW','BN','KH','ID','LA','MY','MM','SG','TH','VN','BD','BT','MV','NP','PK','LK')
hdi[hdi$origin=='OTHER ASIAN','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Caribbean','iso2c'])
hdi[hdi$origin=='Other Caribbean','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Northern Europe','iso2c'])
iso.codes<-iso.codes[-match(c('GB'),iso.codes)]
hdi[hdi$origin=='Other Northern European','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Southern Europe','iso2c'])
iso.codes<-iso.codes[-match(c('IT','PT'),iso.codes)]
hdi[hdi$origin=='Other Southern European','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Eastern Europe','iso2c'])
iso.codes<-iso.codes[-match(c('PL'),iso.codes)]
hdi[hdi$origin=='Other Eastern European','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Western Europe','iso2c'])
iso.codes<-iso.codes[-match(c('DE','NL'),iso.codes)]
hdi[hdi$origin=='Other Western European','hdi_2010']<-mean(hdi[match(iso.codes[!is.na(iso.codes)],hdi$origin),'hdi_2010'],na.rm=TRUE)
hdi[hdi$origin=='USSR','hdi_2010']<-hdi[hdi$origin=='RU','hdi_2010']