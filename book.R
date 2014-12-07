d<-read.table('orange_small_train.data.gz',header=T,sep='\t',na.strings=c('NA',''))
churn<-read.table('sm_churn_labels.csv',header=T,sep='\t')
d$churn<-churn$V1
appetency<-read.table('sm_appet_labels.csv',header=T,sep='\t')
d$appetency<-appetency$V1
upselling<-read.table('sm_upsell_labels.csv',header=T,sep='\t')
d$upselling<-upselling$V1
set.seed(729375)
d$rgroup<-runif(dim(d)[[1]])
dTrainAll<-subset(d,rgroup<=0.9)
dTest<-subset(d,rgroup>0.9)
outcomes=c('churn','appetency','upselling')
vars<-setdiff(colnames(dTrainAll),c(outcomes,'rgroup'))
catVars<-vars[sapply(dTrainAll[,vars],class) %in% c('factor','character')]
numericVars<-vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]
rm(list=c('d','churn','appetency','upselling'))
outcome<-'churn'
pos<-'1'
useForCal<-rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
dCal<-subset(dTrainAll,useForCal)
dTrain<-subset(dTrainAll,!useForCal)

#naive bayes (multivariate)
library('e1071')
lVars<-c(catVars,numericVars)
ff<-paste('as.factor(',outcome,'>0)~',paste(lVars,collapse='+'),sep='')
nbmodel<-naiveBayes(as.formula(ff),data=dTrain)
dTrain$nbpred<-predict(nbmodel,newdata=dTrain,type='raw')[,'TRUE']
dCal$nbpred<-predict(nbmodel,newdata=dCal,type='raw')[,'TRUE']
dTest$nbpred<-predict(nbmodel,newdata=dTest,type='raw')[,'TRUE']

calcAUC(dTrain$nbpred,dTrain[,outcome])
calcAUC(dCal$nbpred,dCal[,outcome])
calcAUC(dTest$nbpred,dTest[,outcome])
