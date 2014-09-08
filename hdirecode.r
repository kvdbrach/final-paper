hdi<-read.csv('hdi.csv',sep=';')
colnames(hdi)[2:14]<-gsub('X','hdi_',colnames(hdi)[2:14])

###Using countrycode library
library('countrycode')

###Add origin-variable to merge and fill with country-codes
hdi$origin<-countrycode(hdi$Country,'country.name','iso2c',warn=TRUE)
hdi[hdi$Country=='South Sudan','origin']<-'SS'

