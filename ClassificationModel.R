cvmodel=function(classifier,mymldr){
  res = list()
  for(i in 1:10){
    ds=create_holdout_partition(mymldr, c(train=0.9, test=0.1), 'iterative')
    classifier.n2v = rakel(ds$train,base.algorithm = 'XGB',k=5,m=21)
    labelsets = as.data.frame(classifier.n2v$labelsets)
    prediction.n2v = predict(classifier.n2v,ds$test )

    write.table(prediction.n2v,file = paste0(classifier,'_predicion.xgb_',i,'.txt'),
                sep = '\t',quote = F, col.names = F,row.names = F)

    newpred = fixed_threshold(prediction.n2v, c())
    result.n2v <- multilabel_evaluate(ds$test, newpred, labels=TRUE)
    res[[i]]=list()
    res[[i]][[1]]=data.frame(result.n2v$multilabel,stringsAsFactors = F)
    res[[i]][[2]]=data.frame(result.n2v$labels,stringsAsFactors = F)
  }
  save(res,file =paste0(classifier,'_reslist.n2v.RData'))
}
library(utiml)


mydata = newdata
colnames(mydata)[(ncol(mydata)-6):ncol(mydata)]=c(1,2,3,4,5,6,7)
mymldr = mldr_from_dataframe(mydata, labelIndices = c((ncol(mydata)-6):ncol(mydata)))
summary(mymldr)
labelsets = as.data.frame(mymldr$labelsets,stringsAsFactors = F)

#####################10CV
setwd()
cvmodel(classifier = 'XGB',mymldr=mymldr)



