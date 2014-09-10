###Read in dataset
m.data<-read.csv('migrationdata.csv',header=TRUE)
m.data<-aggregate(countryflow_2005~country_orig,m.data,sum)
library('countrycode')
m.data$origin<-as.factor(countrycode(m.data$country_orig,'country.name','iso2c',warn=TRUE))

###Read in total population
tp<-read.csv('total_population.csv',header=TRUE,skip=2)
tp$origin<-as.factor(countrycode(tp$Country.Name,'country.name','iso2c',warn=TRUE))
tp<-tp[!is.na(tp$X2010) & !is.na(tp$origin),c('origin','X2010')]
tp<-tp[!duplicated(tp$origin),]

###Merge
m.data<-merge(m.data,tp)

###Calculate as migrants per 1000 inhabitants
m.data$emig_prop<-with(m.data,(countryflow_2005*1000)/X2010)
m.data<-m.data[,c('origin','emig_prop')]

merged<-merge(d,m.data,all.x=TRUE)
missing_origins<-aggregate(emig_prop~origin,merged,function(x) sum(is.na(x)),na.action=na.pass)
missing_origins<-missing_origins[missing_origins$emig_prop>0,]
colnames(missing_origins)[2]<-'emig_prop'
missing_origins$emig_prop<-NA
m.data<-merge(m.data,missing_origins,all.x=TRUE,all.y=TRUE)

###Recode countries and regions
iso.codes<-c(countrycode_data[countrycode_data$continent=='Africa','iso2c'])
m.data[m.data$origin=='African','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
m.data[m.data$origin=='Czechoslowakia','emig_prop']<-mean(c(m.data[m.data$origin=='CZ','emig_prop'],m.data[m.data$origin=='SK','emig_prop']))
iso.codes<-c(countrycode_data[countrycode_data$region=='South America','iso2c'],countrycode_data[countrycode_data$region=='Central America','iso2c'])
m.data[m.data$origin=='Latin, Central and/or South American','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c('BH','IQ','IL','JO','KW','LB','OM','PS','QA','SA','SY','AE','YE')
m.data[m.data$origin=='Other Arab','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c('RS','ME','HR','SI','BA','MK')
m.data[m.data$origin=='Yugoslavia','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c('JP','KP','KR','MO','MN','TW','BN','KH','ID','LA','MY','MM','SG','TH','VN','BD','BT','MV','NP','PK','LK')
m.data[m.data$origin=='OTHER ASIAN','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Caribbean','iso2c'])
m.data[m.data$origin=='Other Caribbean','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Northern Europe','iso2c'])
iso.codes<-iso.codes[-match(c('GB'),iso.codes)]
m.data[m.data$origin=='Other Northern European','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Southern Europe','iso2c'])
iso.codes<-iso.codes[-match(c('IT','PT'),iso.codes)]
m.data[m.data$origin=='Other Southern European','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Eastern Europe','iso2c'])
iso.codes<-iso.codes[-match(c('PL'),iso.codes)]
m.data[m.data$origin=='Other Eastern European','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
iso.codes<-c(countrycode_data[countrycode_data$region=='Western Europe','iso2c'])
iso.codes<-iso.codes[-match(c('DE','NL'),iso.codes)]
m.data[m.data$origin=='Other Western European','emig_prop']<-mean(m.data[match(iso.codes[!is.na(iso.codes)],m.data$origin),'emig_prop'],na.rm=TRUE)
m.data[m.data$origin=='USSR','emig_prop']<-m.data[m.data$origin=='RU','emig_prop']




