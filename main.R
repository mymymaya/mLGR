setwd("~/work/FinalSC/code/MLGR_py/")
library(utiml)
library(data.table)
source('Utilize.R')
#######Create Data for SMOTE based on GAN results
##########transfer 7 Labels to 68 labels
mydata = read.table('dataset/traindata.txt',sep = '\t',quote ='',header = F,stringsAsFactors = F)
CreateSmoteData(mydata,LocScale=F,400,16)
#######Create Data for RAkEL based on SMOTE results
##########transfer 68 Labels to 7 labels
reverseSmoteData(LocScale = F,e=400,bs=16)
####### RAkEL construction
######### 10-fold CV
cv.neg(classifier='XGB', path0='dataset/smote_mir10_',path1='dataset/',e=400,bs=16,a=6,b=14)
######### load final model
#load("dataset/400_16_6_14_ds.RData")
#load("dataset/400_16_6_14_model.RData")
#prediction.xgb1 = predict(classifier.xgb,ds$test,probability = F)#, thres=c(0.37,0.5,0.25,0.4,0.85,0.5,0.4) )

#result.xgb <- multilabel_evaluate(ds$test, prediction.xgb1, labels=TRUE,
#                                  measures = c('hamming-loss','accuracy','precision','recall','F1','subset-accuracy',
#                                               'macro-precision','macro-recall','macro-F1',
#                                               'micro-precision','micro-recall','micro-F1'))
#print(result.xgb$multilabel)
########################################