library(utiml)
library(data.table)
CreateSmoteData=function(mydata,LocScale,e,bs){
  colnames(mydata)[(ncol(mydata)-6):ncol(mydata)]=c(1,2,3,4,5,6,7)
  mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
  summary(mymldr)
  labelsets = as.data.frame(mymldr$labelsets,stringsAsFactors = F)
  ls = labelsets$Var1
  save(ls,file = 'dataset/68lbs.RData')
  newlabel = matrix(0,nrow = nrow(mydata),ncol = length(ls))
  colnames(newlabel)=ls
  for(i in 1:nrow(mydata)){
    lab = mydata[i,(ncol(mydata)-6):ncol(mydata)]
    lab = paste(lab,sep = '',collapse = '')
    ind = which(ls==lab)
    newlabel[i,ind]=1
  }
  dim(newlabel)
  newlabel = data.frame(newlabel,stringsAsFactors = F)
  if(LocScale==T){
    gandata=read.table(paste0('LocScale/New_train_',e,'_',bs,'.txt'),sep = ',',quote ='',header = F,stringsAsFactors = F)
    newdata = data.frame(fea = gandata[,1:(ncol(mydata)-7)],newlabel)
    write.table(newdata,file = paste0('LocScale/',e,'_',bs,'_68lbs.txt'),sep='\t',quote=F,row.names = F,col.names = F)
    
  }else{
    gandata=read.table(paste0('dataset/New_train_',e,'_',bs,'.txt'),sep = ',',quote ='',header = F,stringsAsFactors = F)
    newdata = data.frame(fea = gandata[,1:(ncol(mydata)-7)],newlabel)
    write.table(newdata,file = paste0('dataset/',e,'_',bs,'_68lbs.txt'),sep='\t',quote=F,row.names = F,col.names = F)
    
  }
  
}

reverseSmoteData=function(LocScale,e,bs){
  load('dataset/68lbs.RData')
  if(LocScale==T){
    smote = fread(paste0('SMOTE/LocScale/',e,'_',bs,'_mir10.txt'),
                  quote ='',stringsAsFactors = F,sep='\t',header = T) 
  }else{
    smote = fread(paste0('dataset/',e,'_',bs,'_mir10.txt'),
                  quote ='',stringsAsFactors = F,sep='\t',header = T)
  }
  
  smote = data.frame(smote)
  smote.ind = list()
  sumt = c()
  for(i in 1:length(ls)){
    smote.ind[[i]]=list()
    smote.ind[[i]][[1]]=ls[i]
    smote.ind[[i]][[2]]=which(smote[,((ncol(smote)-length(ls))+i)]==1)
    sumt = c(sumt,length(which(smote[,((ncol(smote)-length(ls))+i)]==1)))
  }
  
  reverselabel = matrix(0,nrow = nrow(smote),ncol = 7)
  for(i in 1:nrow(smote)){
    lab1 = smote[i,(ncol(smote)-length(ls)+1):ncol(smote)]
    sublab1 =which(lab1[1,]==1)
    ls.tmp=ls[sublab1]
    if(length(ls.tmp) != 0){
      ls.tmp=strsplit(ls.tmp,split = '')[[1]]
      ls.ind = which(ls.tmp=='1')
      reverselabel[i,ls.ind]=1
    }else{
      reverselabel[i,]=0
    }
    
  }
  
  
  reverselabel = data.frame(reverselabel,stringsAsFactors = F)
  newdata = data.frame(fea = smote[,1:(ncol(smote)-length(ls))],label = reverselabel,stringsAsFactors = F)
  
  if(LocScale==T){
    save(newdata,file = paste0('SMOTE/LocScale/smote_',e,'_',bs,'.RData'))
  }else{
    save(newdata,file = paste0('dataset/smote_mir10_',e,'_',bs,'.RData'))
  }
  
}

#cvmodel=function(classifier,path0,path1,e,bs,a,b){
# res = list()
#  load(paste0(path0,e,'_',bs,'.RData'))
#  mydata=data.frame(newdata,stringsAsFactors = F)
#  mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
#  for(i in 1:10){
#    ds=create_holdout_partition(mymldr, c(train=0.9, test=0.1), 'iterative')
#    print(i)
#    classifier.xgb = rakel(ds$train,base.algorithm = 'XGB',k=a,m=b)
#    prediction.xgb = predict(classifier.xgb,ds$test )
#    result.xgb <- multilabel_evaluate(ds$test, prediction.xgb, labels=TRUE,
#                                      measures = c('hamming-loss','accuracy','precision','recall','F1','subset-accuracy',
#                                                   'macro-precision','macro-recall','macro-F1',
#                                                   'micro-precision','micro-recall','micro-F1'))
#    #res[[i]]=list()
#    measurements=data.frame(result.xgb$multilabel,stringsAsFactors = F)
#    res[[i]]=measurements
#  }
#  save(res,file=(paste0(path1,'FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'.RData')))
#}

CV.neg=function(path0,path1,e,bs,a,b){
  res = list()
  load(paste0(path0,e,'_',bs,'.RData'))
  mydata=data.frame(newdata,stringsAsFactors = F)
  load('negdata.RData')
  negdata=negdata[,2:374]
  negdata=cbind(negdata,1)
  mydata=cbind(mydata,0)
  
  colnames(negdata)=colnames(mydata)
  traindata=rbind(mydata,negdata)
  mymldr = mldr_from_dataframe(traindata, labelIndices = c((ncol(traindata)-7):ncol(traindata)))
  
  for(i in 1:10){
    ds=create_holdout_partition(mymldr, c(train=0.9, test=0.1), 'iterative')
    print(i)
    classifier.xgb = rakel(ds$train,base.algorithm = 'XGB',k=a,m=b)
    prediction.xgb = predict(classifier.xgb,ds$test )
    result.xgb <- multilabel_evaluate(ds$test, prediction.xgb, labels=TRUE,
                                      measures = c('hamming-loss','accuracy','precision','recall','F1','subset-accuracy',
                                                   'macro-precision','macro-recall','macro-F1',
                                                   'micro-precision','micro-recall','micro-F1'))
    measurements=data.frame(result.xgb$multilabel,stringsAsFactors = F)
    res[[i]]=measurements

  }
  save(res,file=(paste0(path1,'Finalreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'.RData')))
} 

