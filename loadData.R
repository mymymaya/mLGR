#install.packages('openxlsx')
library(openxlsx)

setwd('D:/SC/feature')
##################################################
##########   Load cell genes into LIST  ##########
##################################################

library(openxlsx)
cellstr=c('Alpha', 'Beta', 'Delta', 'Gamma', 'Acinar', 'Ductal', 'Stellate',
          'Endocrine', 'Exocrine')
loadCellGene = function(cname){
  cell = read.xlsx('sc_compare.xlsx',sheet = paste0('T2D.vs.NonT2D.',cname))
  cell = data.frame(cell$X1,cell$Associated.Gene.Name,cell$logFC, cell$PValue, abs(cell$logFC)*(-log10(cell$PValue)))
  names(cell) = c('ENSG', 'SYMBL', 'logFC','Pval','Value')
  #save(cell,file = paste0(cname,'.ref.df.RData'))
  return(cell)
}

cellGenes=list()
for(i in 1:length(cellstr)){
  cellGenes[[i]]=loadCellGene(cellstr[i])
}
names(cellGenes)=cellstr
save(cellGenes,file = 'cellGeneList.RData')
##################################################
############   Compare Cell Genes     ############
##################################################
load('cellGeneList.RData')
ttGene=c() #-->total gene ENSG 
for( i in 1:7){
  ttGene = c(ttGene, cellGenes[[i]]$ENSG)
  ttGene = unique(ttGene)
}




##################################################
############    Create tissue fea     ############
##################################################
setwd('D:/SC/')
##################################################
############    Load gene pos info    ############
##################################################
library(biomaRt)
mart = useMart('ensembl','hsapiens_gene_ensembl')
listFilters(mart) #list input id
listAttributes(mart) #list output id
gene_info = getBM(attributes = c('ensembl_gene_id','chromosome_name','start_position','end_position'),
                  filters='ensembl_gene_id',values = dgpre$V2, mart = mart)


##################################################
############      Load eqtl info      ############
##################################################
library(data.table)
filterTissue = function(pan){
  pan. = data.frame()
  for( i in 1:nrow(pan)){
    tmp=data.frame('ensembl_id' = strsplit(pan[i,1],split='\\.')[[1]][1], 'symbl'=pan[i,2],
                   'chr' = pan[i,3], 'start' = pan[i,4], 'end' = pan[i,5], 'SNP' = pan[i,19],
                   'pval' = pan[i,28], 'log2FC' = pan[i,31])
    pan. = rbind(pan.,tmp)
  }
  return(pan.)
}
mus = data.frame(fread('Muscle_Skeletal.v8.egenes.txt',sep='\t',quote='',header = T,stringsAsFactors = F))
mus. = filterTissue(mus)
adi = data.frame(fread('Adipose_Subcutaneous.v8.egenes.txt',sep='\t',quote='',header = T,stringsAsFactors = F))
adi. = filterTissue(adi)
pan = data.frame(fread('Pancreas.v8.egenes.txt',sep='\t',quote='',header = T,stringsAsFactors = F))
pan. = filterTissue(pan)
blo = data.frame(fread('Whole_Blood.v8.egenes.txt',sep='\t',quote='',header = T,stringsAsFactors = F))
blo. = filterTissue(blo)
tissue.list = list()
load('tissue.eqtl.RData')
tissue.list[[1]]=pan.
tissue.list[[2]]=blo.
tissue.list[[3]] = mus.
tissue.list[[4]] = adi.
names(tissue.list)=c('pancreas','blood','muscle','adipose')
save(tissue.list,file = 'tissue.eqtl.RData')
##################################################
############   Create tissue eqtl fea ############
##################################################
load('tissue.eqtl.RData')
load('gene_pos.RData')
load('cellGeneList.RData')
load('ttGene.RData')
pan. = tissue.list$pancreas
mapGeneTissue = function(gene,pan.){
  gene_map = data.frame()
  for (i in 1:length(ttGene)){
    pval=pan.[which(pan.$ensembl_id==ttGene[i]),7]
    FC = pan.[which(pan.$ensembl_id==ttGene[i]),8]
    if (length(pval)!= 0){
      tmp=data.frame('ENSG' = ttGene[i], 'pval' = pval, 'log2FC' = FC)
    }else{
      tmp=data.frame('ENSG' = ttGene[i], 'pval' = '0.9999999', 'log2FC' = '0.0000001')
    }
    
    gene_map = rbind(gene_map, tmp)
  }
  return(gene_map)
}
pan.map = mapGeneTissue(ttGene,pan.)
blo. = tissue.list$blood
blo.map = mapGeneTissue(ttGene, blo.)
mus. = tissue.list$muscle
mus.map = mapGeneTissue(ttGene,mus.)
adi. = tissue.list$adipose
adi.map = mapGeneTissue(ttGene,adi.)
geneMapTissue = cbind(pan.map,blo.map[,2:3])
geneMapTissue = cbind(geneMapTissue, mus.map[,2:3], adi.map[,2:3])
names(geneMapTissue)=c('ENSG', 'pan.p','pan.FC','blo.p','blo.FC','mus.p','mus.FC','adi.p','adi.FC')
save(geneMapTissue,file='fea.eqtl.RData')

#######creat dgpre tissue fea########
load('tissue.eqtl.RData')
load('dgprePosInfo.RData') 
#load('gene_pos.RData')
#load('cellGeneList.RData')
#load('ttGene.RData')
ttGene = dgpre$V2
pan. = tissue.list$pancreas

mapGeneTissue = function(gene,pan.){
  gene_map = data.frame()
  for (i in 1:length(ttGene)){
    pval=pan.[which(pan.$ensembl_id==ttGene[i]),7]
    FC = pan.[which(pan.$ensembl_id==ttGene[i]),8]
    if (length(pval)!= 0){
      tmp=data.frame('ENSG' = ttGene[i], 'pval' = pval, 'log2FC' = FC)
    }else{
      tmp=data.frame('ENSG' = ttGene[i], 'pval' = '0.9999999', 'log2FC' = '0.0000001')
    }
    
    gene_map = rbind(gene_map, tmp)
  }
  return(gene_map)
}
pan.map = mapGeneTissue(ttGene,pan.)
blo. = tissue.list$blood
blo.map = mapGeneTissue(ttGene, blo.)
mus. = tissue.list$muscle
mus.map = mapGeneTissue(ttGene,mus.)
adi. = tissue.list$adipose
adi.map = mapGeneTissue(ttGene,adi.)
geneMapTissue = cbind(pan.map,blo.map[,2:3])
geneMapTissue = cbind(geneMapTissue, mus.map[,2:3], adi.map[,2:3])
names(geneMapTissue)=c('ENSG', 'pan.p','pan.FC','blo.p','blo.FC','mus.p','mus.FC','adi.p','adi.FC')
dgpreMapTissue = geneMapTissue
save(dgpreMapTissue,file='dgpre.fea.eqtl.RData')
##################################################
############ Create   GWAS   feature  ############
##################################################
library(data.table)
gwas = fread('t2d010118.txt',sep='\t',quote='',header = T,stringsAsFactors = F)
gwas = data.frame('chr' = gwas$Chr, 'pos' = gwas$Pos, 'beta' = gwas$Beta, 'SE' = gwas$SE, 'Pval' = gwas$P)
gwasList = list()
for(i in 1:22){
  gwasList[[i]]=gwas[which(gwas$chr == i),]
}
names(gwasList)=seq(1,22,1)
save(gwasList, file = 'gwasList.RData')
gene_info = gene_info[-which(gene_info$chromosome_name=='MT'),]
gene_info = gene_info[-which(gene_info$chromosome_name=='X'),]
gene_info = gene_info[-which(gene_info$chromosome_name=='Y'),]
save(gene_info, file = 'gene_info_filter.RData')
gene_snp = data.frame()
for (i in 1:nrow(gene_info)){
  chr = gene_info[i,2]
  id = gene_info[i,1]
  sta = gene_info[i,3]
  end = gene_info[i,4]
  ref.df = gwasList[[as.numeric(chr)]]
  tmp = ref.df[which(ref.df$pos %in% seq(sta, end,1)),]
  if (nrow(tmp) >= 5){
    tmp = ref.df[sort(ref.df$Pval,index.return = TRUE)$ix,][1:5,]
    op.tmp = cbind(data.frame(id, chr, sta,end),matrix(tmp$Pval,nrow = 1, ncol = 5))
    names(op.tmp) = c('id', 'chr', 'start', 'end', 'pval.1','pval.2', 'pval.3', 'pval.4','pval.5')
  }else{
    zeronum = 5-nrow(tmp)
    pvalnum = length(tmp$Pval)
    op.tmp = cbind(data.frame(id, chr, sta,end),matrix(tmp$Pval,nrow = 1, ncol = pvalnum), matrix(0.9999999, nrow = 1, ncol = zeronum))
    names(op.tmp) = c('id', 'chr', 'start', 'end', 'pval.1','pval.2', 'pval.3', 'pval.4','pval.5')
  }
  gene_snp = rbind(gene_snp, op.tmp)
}
##################################################
############    Create KEGG feature    ########### 
##################################################
kegg = read.table('createKEGG/HSA_KEGG.txt',sep = '\t',quote = '', header = T, stringsAsFactors = F)
library(biomaRt)
mart = useMart('ensembl','hsapiens_gene_ensembl')
#listFilters(mart) #list input id
#listAttributes(mart) #list output id
ttGene = geneMapTissue$ENSG
gene_entrez = getBM(attributes = c('ensembl_gene_id','entrezgene_id'),
                    filters='ensembl_gene_id',values = ttGene, mart = mart)
#gene_kegg = getBM(attributes = c('ensembl_gene_id','kegg_enzyme'),
filters='ensembl_gene_id',values = ttGene, mart = mart)
gene_go = getBM(attributes = c('ensembl_gene_id','go_id'),
                filters='ensembl_gene_id',values = ttGene, mart = mart)
gene_go. = gene_go[-which(gene_go$go_id==""),]
save(gene_go.,file = 'feature/createGOfea/gene_go.RData')
gene_symbl = getBM(attributes = c('ensembl_gene_id','hgnc_symbol'),
                   filters='ensembl_gene_id',values = ttGene, mart = mart)

gene_entrez = gene_entrez[complete.cases(gene_entrez$entrezgene_id),]
gene.entrez.df = data.frame(table(gene_entrez$ensembl_gene_id))
gene.filt = gene.entrez.df[which(gene.entrez.df$Freq>1),1]
gene.filt. = unique(gene_entrez$ensembl_gene_id)
gene_entrez_filt = data.frame()
for(i in 1:length(gene.filt.)){
  tmp = gene_entrez[which(gene_entrez$ensembl_gene_id == gene.filt.[i]),][1,]
  gene_entrez_filt = rbind(gene_entrez_filt,tmp)
}
save(gene_entrez_filt, file = 'gene_entrez_filt.RData')

paths = unique(kegg$KEGGID) #343 paths
path.fea = cbind(gene_entrez_filt,matrix(0,nrow = nrow(gene_entrez_filt), ncol = 343))
for(i in 1:nrow(path.fea)){
  entrez = as.numeric(path.fea[i,2])
  tmp = kegg[which(kegg$ENTREZID == entrez),1]
  for(j in 1:length(tmp)){
    ind = which(paths==tmp[j])
    path.fea[i,ind+2]=1
  }
}
path.fea_ = path.fea
save(path.fea_, file = 'createKEGG/lessPath.fea.RData')
path.fea = data.frame()
for(i in 1:length(ttGene)){
  id = ttGene[i]
  if (id %in% path.fea_$ensembl_gene_id){
    tmp = as.matrix(path.fea_[which(path.fea_$ensembl_gene_id==id),3:length(path.fea_[1,])])
  }else{
    tmp = matrix(0,nrow = 1,ncol = 343)
  }
  temp = cbind(data.frame('id' = id),tmp)
  path.fea = rbind(path.fea, temp)
}
save(path.fea, file = 'createKEGG/path.fea.RData')
#################################################
######dgpre kegg fea#############################
###################################################
kegg = read.table('createKEGG/HSA_KEGG.txt',sep = '\t',quote = '', header = T, stringsAsFactors = F)
library(biomaRt)
mart = useMart('ensembl','hsapiens_gene_ensembl')
#listFilters(mart) #list input id
#listAttributes(mart) #list output id
ttGene = dgpreMapTissue$ENSG
gene_entrez = getBM(attributes = c('ensembl_gene_id','entrezgene_id'),
                    filters='ensembl_gene_id',values = ttGene, mart = mart)
gene_entrez = gene_entrez[complete.cases(gene_entrez$entrezgene_id),]
gene.entrez.df = data.frame(table(gene_entrez$ensembl_gene_id))
gene.filt = gene.entrez.df[which(gene.entrez.df$Freq>1),1]
gene.filt. = unique(gene_entrez$ensembl_gene_id)
gene_entrez_filt = data.frame()
for(i in 1:length(gene.filt.)){
  tmp = gene_entrez[which(gene_entrez$ensembl_gene_id == gene.filt.[i]),][1,]
  gene_entrez_filt = rbind(gene_entrez_filt,tmp)
}
dgpre_entrez_filt = gene_entrez_filt
save(dgpre_entrez_filt, file = 'dgpre_entrez_filt.RData')

paths = unique(kegg$KEGGID) #343 paths
path.fea = cbind(gene_entrez_filt,matrix(0,nrow = nrow(gene_entrez_filt), ncol = 343))
for(i in 1:nrow(path.fea)){
  entrez = as.numeric(path.fea[i,2])
  tmp = kegg[which(kegg$ENTREZID == entrez),1]
  for(j in 1:length(tmp)){
    ind = which(paths==tmp[j])
    path.fea[i,ind+2]=1
  }
}
dgprePathFea = path.fea
save(dgprePathFea, file = 'createKEGG/dgprePathFea.RData')
path.fea = data.frame()
path.fea_ = dgprePathFea
for(i in 1:length(ttGene)){
  id = ttGene[i]
  if (id %in% path.fea_$ensembl_gene_id){
    tmp = as.matrix(path.fea_[which(path.fea_$ensembl_gene_id==id),3:length(path.fea_[1,])])
  }else{
    tmp = matrix(0,nrow = 1,ncol = 343)
  }
  temp = cbind(data.frame('id' = id),tmp)
  path.fea = rbind(path.fea, temp)
}
dgpre.path.fea = path.fea
save(dgpre.path.fea, file = 'createKEGG/dgpre.path.fea.RData')


##################################################
############    Combine total feature  ########### 
##################################################
load('createKEGG/dgpre.path.fea.RData')
load('dgpre.fea.snp.RData')
load('dgpre.fea.eqtl.RData')
ttGene = geneMapTissue$ENSG
snpgene = gene_snp$id
total.fea = data.frame()
for(i in 1:length(ttGene)){
  id = ttGene[i]
  tissue = geneMapTissue[which(geneMapTissue$ENSG==id),2:9]
  path = path.fea[which(path.fea$id==id),2:344]
  if (id %in% snpgene){
    snp = gene_snp[which(gene_snp$id==id),5:9]
  }else{
    snp = data.frame(matrix(0,nrow = 1, ncol = 5))
    names(snp) = c('pval.1', 'pval.2','pval.3','pval.4','pval.5')
  }
  tmp = cbind(id, tissue, snp, path)
  total.fea = rbind(total.fea, tmp)
}
save(total.fea,file = 'feature/total.fea.RData')
#########################################
######## combine dgpre gene fea###########
############################################

load('createKEGG/dgpre.path.fea.RData')
load('dgpre.fea.snp.RData')
load('dgpre.fea.eqtl.RData')
ttGene = dgpreMapTissue$ENSG
geneMapTissue = dgpreMapTissue
path.fea = dgpre.path.fea
snpgene = dgpsnpfea$id
gene_snp = dgpsnpfea
total.fea = data.frame()
for(i in 1:length(ttGene)){
  id = ttGene[i]
  tissue = geneMapTissue[which(geneMapTissue$ENSG==id),2:9]
  path = path.fea[which(path.fea$id==id),2:344]
  if (id %in% snpgene){
    snp = gene_snp[which(dgpsnpfea$id==id),2:6]
  }else{
    snp = data.frame(matrix(0,nrow = 1, ncol = 5))
    names(snp) = c('pval.1', 'pval.2','pval.3','pval.4','pval.5')
  }
  tmp = cbind(id, tissue, snp, path)
  total.fea = rbind(total.fea, tmp)
}
dim(total.fea)
#####ensg is dupilicated
total.fea1 = total.fea[which(!duplicated(total.fea$id)),]
dgp.total.fea = total.fea1
save(dgp.total.fea,file = 'dgp.total.fea.RData')
