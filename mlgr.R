library(utiml)
library(data.table)
###############################################################
###################MULTI LABEL CLASSIFICATION####################
###############################################################

#traindata

mydata = read.table('traindata.txt',sep = '\t',quote ='',header = F,stringsAsFactors = F)
mydata=data.frame(mydata,stringsAsFactors = F)
mydata[,1:366]=scale(mydata[,1:366],scale = T, center = T)
mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
summary(mymldr)
labelsets = as.data.frame(mymldr$labelsets,stringsAsFactors = F)

####follow smote
setwd('') 

#####smote data
load('smote.RData')
mydata=data.frame(newdata,stringsAsFactors = F)

####negdata

load('negdata.RData')
negdata=cbind(negdata,1)
mydata=cbind(mydata,0)
colnames(negdata)=colnames(mydata)
posindex=cut(sample(1:nrow(mydata)),10,labels=F)
negindex=cut(sample(1:nrow(negdata)),10,labels=F)
res=list()

for(kk in 1:10){
  print(kk)
  test.indexpos=which(posindex==kk)
  test.indexneg=which(negindex==kk)
  train.indexpos=which(posindex!=kk)
  train.indexneg=which(negindex!=kk)
  
  train=rbind(mydata[train.indexpos,],negdata[train.indexneg,])
  test=rbind(mydata[test.indexpos,],negdata[test.indexneg,])
  ds=list()
  ds$train=mldr_from_dataframe(train,labelIndices = c((ncol(mydata)-7):ncol(mydata)))
  ds$test=mldr_from_dataframe(test,labelIndices = c((ncol(mydata)-7):ncol(mydata)))
  y_test = ds$test$dataset[,(ncol(mydata)-7):ncol(mydata)]
  
  classifier.xgb = rakel(ds$train,base.algorithm = 'XGB',k=5,m=10)
  
  prediction.xgb = predict(classifier.xgb,ds$test )
  #write.table(prediction.xgb,file = '10cv/predicion.xgb.txt',sep = '\t',quote = F, col.names = F,row.names = F)
  result.xgb <- multilabel_evaluate(ds$test, prediction.xgb, labels=TRUE)
  result.xgb$multilabel
  #result.xgb$labels
  res[[kk]]=list()
  res[[kk]][[1]]=data.frame(result.xgb$multilabel,stringsAsFactors = F)
  #res[[kk]][[2]]=data.frame(result.xgb$labels,stringsAsFactors = F)
  
}  


#############################################
###############   10 CV  ###############
#############################################

cvmodel=function(classifier,mymldr){
  res = list()
  for(i in 1:10){
    ds=create_holdout_partition(mymldr, c(train=0.9, test=0.1), 'iterative')
    classifier = rakel(ds$train,base.algorithm = 'XGB')#SVM,RF
    prediction = predict(classifier,ds$test )
    #head(prediction.xgb)
    write.table(prediction,file = paste0(classifier,'_predicion.xgb_',i,'.txt'),
                sep = '\t',quote = F, col.names = F,row.names = F)
    result<- multilabel_evaluate(ds$test, prediction, labels=TRUE)
    res[[i]]=list()
    res[[i]][[1]]=data.frame(result$multilabel,stringsAsFactors = F)
    res[[i]][[2]]=data.frame(result$labels,stringsAsFactors = F)
  }
  save(res,file =paste0(classifier,'_reslist.RData'))
}


#train data 10-fold cv
mydata=read.table('traindata.txt',sep='\t',quote='',stringsAsFactors = F,header = F)
colnames(mydata)[(ncol(mydata)-6):ncol(mydata)]=c(1,2,3,4,5,6,7)
mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
cvmodel(classifier = 'train',mymldr=mymldr)
#smote data 10-fold cv
load('smotedata.RData')
colnames(mydata)[(ncol(mydata)-6):ncol(mydata)]=c(1,2,3,4,5,6,7)
mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
cvmodel(classifier = 'smote',mymldr=mymldr)
#gcn data 10-fold cv
load('gcndata.RData')
colnames(newdata)[(ncol(newdata)-6):ncol(newdata)]=c(1,2,3,4,5,6,7)#gcn
mymldr1 = mldr_from_dataframe(newdata, labelIndices = c((ncol(newdata)-6):ncol(newdata)))
summary(mymldr1)
labelsets1 = as.data.frame(mymldr1$labelsets,stringsAsFactors = F)    
cvmodel(classifier='gcn', mymldr = mymldr1)
#n2v data 10-fold cv
load('n2vdata.RData')
colnames(newdata)[(ncol(newdata)-6):ncol(newdata)]=c(1,2,3,4,5,6,7)#gcn
mymldr1 = mldr_from_dataframe(newdata, labelIndices = c((ncol(newdata)-6):ncol(newdata)))
summary(mymldr1)
labelsets1 = as.data.frame(mymldr1$labelsets,stringsAsFactors = F)    
cvmodel(classifier='n2v', mymldr = mymldr1)



