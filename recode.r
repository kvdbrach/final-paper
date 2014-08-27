###Change DataFilenames if needed
ess<-ess
gss<-gss
eds<-eds


###Praying 
###Omzetten naar 5-puntenschaal van EDS
###Categorieën:0: Never 1: Less than once a week 2: At least once a week

###pray-verdeling checken bij GSS

eds$praying<-ifelse(as.numeric(eds$BK_Q135)>5,NA,as.numeric(eds$BK_Q135))
eds$praying<-ifelse(eds$praying>1,0,1)

ess$praying<-ifelse(as.numeric(ess$pray)>3,0,1)

gss$praying<-ifelse(as.numeric(gss$pray)>4,0,1)

###Subjective-religiosity

