mipex<-read.table('mipex.csv',header=TRUE,sep=";")


###Merge all to destination-side dataset
destination<-merge(merge(merge(disneigh,disjobs),mipex),religion,all.x=TRUE,all.y=TRUE)
###Rename contextual variables
colnames(destination)[2:6]<-paste(colnames(destination)[2:6],'_dest',sep='')

