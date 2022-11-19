CreateSmoteData=function(mydata,LocScale,e,bs){
  colnames(mydata)[(ncol(mydata)-6):ncol(mydata)]=c(1,2,3,4,5,6,7)
  mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
  summary(mymldr)
  labelsets = as.data.frame(mymldr$labelsets,stringsAsFactors = F)
  ls = labelsets$Var1
  #save(ls,file = '68labels.RData')
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
    gandata=read.table(paste0('GAN/LocScale/New_train_',e,'_',bs,'.txt'),sep = ',',quote ='',header = F,stringsAsFactors = F)
    newdata = data.frame(fea = gandata[,1:(ncol(mydata)-7)],newlabel)
    write.table(newdata,file = paste0('SMOTE/LocScale/',e,'_',bs,'_68lbs.txt'),sep='\t',quote=F,row.names = F,col.names = F)
    
  }else{
    gandata=read.table(paste0('GAN/New_train_',e,'_',bs,'.txt'),sep = ',',quote ='',header = F,stringsAsFactors = F)
    newdata = data.frame(fea = gandata[,1:(ncol(mydata)-7)],newlabel)
    write.table(newdata,file = paste0('SMOTE/',e,'_',bs,'_68lbs.txt'),sep='\t',quote=F,row.names = F,col.names = F)
    
  }
  
}

reverseSmoteData=function(LocScale,e,bs){
  load('68lbs.RData')
  if(LocScale==T){
    smote = fread(paste0('SMOTE/LocScale/',e,'_',bs,'_mir10.txt'),
                  quote ='',stringsAsFactors = F,sep='\t',header = T) 
  }else{
    smote = fread(paste0('SMOTE/',e,'_',bs,'_mir10.txt'),
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
    save(newdata,file = paste0('SMOTE/smote_mir10_',e,'_',bs,'.RData'))
  }
  
}

CV.neg=function(path0,path1,e,bs,a,b){
  load(paste0(path0,e,'_',bs,'.RData'))
  mydata=data.frame(newdata,stringsAsFactors = F)
  load('negdata.RData')
  negdata=negdata[,2:374]
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
    classifier.xgb = rakel(ds$train,base.algorithm = 'XGB',k=a,m=b)
    prediction.xgb = predict(classifier.xgb,ds$test )
    result.xgb <- multilabel_evaluate(ds$test, prediction.xgb, labels=TRUE)
    result.xgb$mulilabel
    
    measurements=data.frame(result.xgb$multilabel,stringsAsFactors = F)
    res[[kk]]=measurements
    names(res)[kk]=paste0(e,'_',bs)
  } 
  save(res,file=(paste0(path1,'10cv/negreslist_',e,'_',bs,'_',a,'_',b,'.RData')))
  
} 

scale01=function(dfcol){
  a=min(dfcol)
  b=max(dfcol)
  dfcol=unlist(apply(data.frame(dfcol),2,function(x) ((x-a)/(b-a))))
  return(dfcol)
}

cvmodel=function(classifier,path0,path1,e,bs,a,b){
  res = list()
  load(paste0(path0,e,'_',bs,'.RData'))
  mydata=data.frame(newdata,stringsAsFactors = F)
  mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
  for(i in 1:10){
    ds=create_holdout_partition(mymldr, c(train=0.9, test=0.1), 'iterative')
    print(i)
    classifier.xgb = rakel(ds$train,base.algorithm = 'XGB',k=a,m=b)
    #labelsets = as.data.frame(classifier.xgb$labelsets)
    prediction.xgb = predict(classifier.xgb,ds$test )
    #head(prediction.xgb)
    #write.table(prediction.xgb,file = paste0(classifier,'_predicion_',i,'.txt'),
    #            sep = '\t',quote = F, col.names = F,row.names = F)
    result.xgb <- multilabel_evaluate(ds$test, prediction.xgb, labels=TRUE,
                                      measures = c('hamming-loss','accuracy','precision','recall','F1','subset-accuracy',
                                                   'macro-precision','macro-recall','macro-F1',
                                                   'micro-precision','micro-recall','micro-F1'))
    #res[[i]]=list()
    measurements=data.frame(result.xgb$multilabel,stringsAsFactors = F)
    res[[i]]=measurements
    #print(measurements)
    #if(dir.exists(paste0(path1,'10cv/ds/',e,'_',bs,'_',a,'_',b,'_',classifier))){
    #  save(ds, file =paste0(path1,'10cv/ds/',e,'_',bs,'_',a,'_',b,'_',classifier,
    #                        '/FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'_',i,'.RData'))
    #}else{
    #  dir.create(paste0(path1,'10cv/ds/',e,'_',bs,'_',a,'_',b,'_',classifier))
    #  save(ds, file =paste0(path1,'10cv/ds/',e,'_',bs,'_',a,'_',b,'_',classifier,
    #                        '/FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'_',i,'.RData'))
    #}
    #save(ds,file =paste0(path1,'10cv/ds/FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'_',i,'.RData'))
    #if(dir.exists(paste0(path1,'10cv/model/',e,'_',bs,'_',a,'_',b,'_',classifier))){
    #  save(classifier.xgb,file =paste0(path1,'10cv/model/',e,'_',bs,'_',a,'_',b,'_',classifier,
    #                                   '/FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'_',i,'.RData'))
    #}else{
    #  dir.create(paste0(path1,'10cv/model/',e,'_',bs,'_',a,'_',b,'_',classifier))
    #  save(classifier.xgb,file =paste0(path1,'10cv/model/',e,'_',bs,'_',a,'_',b,'_',classifier,
    #                                   '/FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'_',i,'.RData'))
    #}
    
    #save(classifier.xgb,file =paste0(path1,'10cv/model/FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'_',i,'.RData'))
    #names(res)[i]=paste0(e,'_',bs,'_',a,'_',b)
  }
  save(res,file=(paste0(path1,'FunVer_negreslist_',e,'_',bs,'_',a,'_',b,'_',classifier,'.RData')))
}