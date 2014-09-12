#Change to MLwiN-path below
mlwinpath<-"C:/Program Files (x86)/MLwiN v2.30/"

###Load library's and code
library(R2MLwiN)
source('MLwiNfunctions.r',echo=TRUE)

#Data
inData.affiliated<-d.affiliated
inData.praying<-d.praying

nlevels<-3
levID<-c('countries','origin','cons')
distri='Binomial'

formula<-'logit(affiliated,cons)~(0|cons)+(3|cons)+(2|cons)+(1|cons)'

#Options
estoptions<-list(EstM=1,xclass=list('classes'=c(2,3),'N1'=c(1,1)),mcmcOptions=list(hcen=nlevels,orth=1))

model1<-runMLwiN(formula,levID, D=distri,indata=inData.affiliated,estoptions,MLwiNPath=mlwinpath)

formula<-'logit(affiliated,cons)~(0|cons+migr)+(3|cons)+(2|cons)+(1|cons)'

model2<-runMLwiN(formula,levID, D=distri,indata=inData.affiliated,estoptions,MLwiNPath=mlwinpath)

formula<-'logit(affiliated,cons)~(0|cons+migr)+(3|cons+migr)+(2|cons)+(1|cons)'

model3<-runMLwiN(formula,levID, D=distri,indata=inData.affiliated,estoptions,MLwiNPath=mlwinpath,smat=c(3,1))