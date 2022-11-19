File description:

Data description:
1. Traindata.txt is the original gene features with 7 cell labels used as positive set. The feature dimension is 366D including 5D GWAS feature, 8D eQTL feature, 343D pathway feature and 10D subcellular localization feature, the features could be alculated according to section 2.2feature extraction. If the gene is related to the ith cell type, the label is set to 1. Otherwise, the labels would be 0.

note: for feature extraction, the related data are also uploaded. including:     
      sc_compare.xlsx is the pairwise differential expression analyses data from GSE86473.
      Adipose_Subcutaneous.v8.egenes.txt
      Whole_Blood.v8.egenes.txt
      Muscle_Skeletal.v8.egenes.txt
      Pancreas.v8.egenes.txt
      are used for eQTL feature extraction.
      t2d010118.txt is used for GWAS feature extraction.
      All RNA subcellular localization data.txt is used for subcellular feature extraction.
      HSA_KEGG.txt is used for pathway feature exaction.

2.T2D_multilabel_genes.RData is gene ids of positive data, including ensembl id, entrez id and gene symbl.

3.negdata.RData is the negative set we constructed for model training. The first column is gene ID (ensembl).

Run our code:

1.create folders under the working directory named as:
   'dataset/':put the traindata.txt, negdata.RData into this folder

2.Run MLGR_GAN.py:
  This step is for feature reconstruction based on GAN.
3.Run main.R : 
  First run line 1 to line 8, this process is to transform data structure (create sub-label sets) for data augmentation.
4.Run MLGR_somote.py:
  This step is for data augmentation based on SMOTE.
Run main.R:
  Run function:reverseSomteData. This process is to reverse the sub-label sets to 7 cell labels.

  Then, run function:cv.neg. This process is to record the performance of 10 fold cross validation, the 12 measurements would be recorded in Finalreslist_400_16_6_14_XGB.RData
