rm(list=ls())
library(tmcn)
library(tm)
library(Rwordseg)
library(wordcloud)
library(cluster)

setwd("C:\\Users\\11\\Desktop\\LDA")
txt<-read.table('0815����L�Q.txt',colClasses="character") 
#����k���@�q�@�q�����|���ťաA�H�eŪ�J�|���ť�

#���s�W��
insertedwords <- c("���T","�W�t�D��","�l�o","�B�j��","�H�p�l","�E����","���","�p���Y","�j�Y��","������","�p���N�x","�p�s�k","���ӥ�","�C�B��","���B�@","���Q�T","���]��","���өZ","������","�L�`��","���J��","�Ѫǹ�","�]�C�C","�]���G","�ֲM��","�ӧӤZ","���q��","�Q�Ű�","�v�B��","�v��r","�v����","�v�u�j","�v�s�s","�t�]�v��","�����P","�����T","�F����","�B�B��","���l�h","�̩h","�f��w","�§Ӹ�","���ӱ`","��h","�ݦ���","�ʯ�P","������","���G�Q","����","�i�ӥ�","���C��","���a�^","���w��","���j��","����","�F�q��","�F���W�H","���t","����","����","�N��","�i�g�_","�i�@�]","���Ѥ�","�i�G��","���L��","���L","�Z�T�q","�Z����","�Z�פ�","�Z�T�Q","�L�­^","�C�߷���","�C�߿P","�����P","�����","�N��","�X��","���Y��","�C�F�l","�ڶ��p","�C�߻�","�P�B�q","�x��i","�I�a����","�X��","���}��","�J�q��","ı��","�_���c","���ӷq","�x�C��","�V�M��","���y��","���M�w","�Z�Ӥ�","�k���h","�Ħh","���Įv","�{���{","�|���}","�^�s��","���L��","���j��","���Ѥ�","�^����","�ŤѩM","�G��","�{�^","�p�r","�ʤd��","�ٯ���","�ʩR��","�ʤd�Q","���j�q","�t��l","�Ԥ@��","����j��","����G��","����T��","����|��","���䤭��","��J�g","�C�߮�","�ʤd�V","�n������","�O��","�L���I�v"
                   ,"���q�D","���m�P�l","���P�h","���n","�B�z�Ȱw","���m���x","�˥�","�f�m","�����c","�]��","���u��","�P��","�����H","�׫n�s","�����H��","�ɸ�","�]�C�C","�ɸ���","�h�h","���a","���v�C�C","�H��","�j�Ӭ�","�̳J","�_�s��","�զ�","���r����","�@����","�ع�","�ɸ��w","�L�b","���","���G","�ܰs","�����","�Υ�����","�ѤU�L��","�e��","���a��","�Z�L���D","����S","������M","����","�����D","���w","���K�K","����","������","���]���D","����","�t��","���a��","�Q�k�C","�g�l�C","������","��","���u�C�k","�@�r","����@�r","�L���j��","���`�ɷ�","���ӫ�w","����","�r��","�C�]","��S","�C��","���C","����","�W�t","�D�x","�Ѭr","����","����","�_�z��","���K�C","�m�C","����L","��s�@�]��","�E���F��","�v��S��","���s��","�p�F��","�@�O","�ʪᨦ","�W�u","�_�u","����׸�","�_�z�V","���H�~��","�L���ͦ�","��d�a��","�r�ުŨ�","�O���q��","��ͨ���","�e�H���Z","�˦�f�I","�o��ѭ�","�t�Υu�v","����]�n","�������w","�a�~����","���L�H��","�Q�J�D�D","�b�Y����","�U�~�s��","�޹D","�Q���~","����","�n��","��g","�_�L","���x��","�֪L�x","��v","�k��","�ſ�","�K�j�q","�n糥�","����"
                   ,"�ɤk�C�k","�ɤk���߼C�k","��ù�a����","���k���k","��­�C�k","����x�k","���r���x","�T�L�T����","�C�P�E�|","�@����","�d���ǭ�","�g�����p�\","�L�W�j�O�S�k","�����j��","�s�H��Y�\","�f�M�P��x","�����˶äb�k","���ޥ\��","�j���]���k","������k","���{�Y�H�\","�ؤ���ͥ\","���F�U���@�k","�r��x","�K�x�ள��","�H���b","���p�x�k","�W�ѱ�","���a�M�k","�ɤk�߸g","�ள��","���u�C�k","�E���u�g")

insertWords(toTrad(iconv(insertedwords, "big5", "UTF-8"), TRUE))

word<- segmentCN(txt$V1)
word<-Corpus(VectorSource(word))

myStopWords <- c(stopwordsCN(), "�H�H","�L�D","�@�f��", "��","�F","�O","��","�o","�L","��","�b","�A","�o","�]","��","�o")

reuters <- tm_map(word, removeWords, myStopWords)
d.corpus<- tm_map(reuters , segmentCN, nature = TRUE)

d.corpu1 <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) =="userDefine"]
  })
  unlist(noun)
})
#�]�s�W�������ʬ�userDefine�A�Q�ΰj��N���J

d.vec <- sapply(d.corpu1 , paste, collapse = " ")
d.vec <- unique(d.vec)
d.corpu3 <- Corpus(VectorSource(d.vec))
dtm <- DocumentTermMatrix(d.corpu3, 
                          control = list(wordLengths=c(2,6),removeNumbers = TRUE,
                          removePunctuation  = list(preserve_intra_word_dashes = FALSE),
                          weighting = weightTf,encoding = "UTF-8"))

colnames(dtm)#�ˬd��J���W��
rownames(dtm)
#�H�U���x�s�r������excel
a<-col_sums(dtm)
write.csv(a,"C:\\Users\\11\\Desktop\\LDA\\wordfreq.csv")



library(tmcn)
library(tm)
library(Rwordseg)
library(wordcloud)
library(slam)
library(topicmodels)
library(igraph)

#����奻-���x�}�i��²��B�z�A�H�������W�v�����Q�����M�C�W�v�����Q�C�������D
#term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
#l1=term_tfidf >= quantile(term_tfidf, 0.5)
#dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]
summary(col_sums(dtm))
col_sums(dtm)#�ˬd��J���r��

fold_num = 10
kv_num =  c(5, 10*c(1:5, 10))
seed_num = 2003
try_num = 1

smp<-function(cross=fold_num,n,seed)
{
  set.seed(seed)
  dd=list()
  aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)
  for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]
  return(dd)
}

selectK<-function(dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp) # change 60 to 15
{
  per_ctm=NULL
  log_ctm=NULL
  for (k in kv)
  {
    per=NULL
    loglik=NULL
    for (i in 1:try_num)  #only run for 3 replications# 
    {
      cat("R is running for", "topic", k, "fold", i,
          as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")),"\n")
      te=sp[[i]]
      tr=setdiff(1:dtm$nrow, te) # setdiff(nrow(dtm),te)  ## fix here when restart r session
      
      # VEM = LDA(dtm[tr, ], k = k, control = list(seed = SEED)),
      # VEM_fixed = LDA(dtm[tr,], k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
      
      #       CTM = CTM(dtm[tr,], k = k, 
      #                 control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))  
      #       
      Gibbs = LDA(dtm[tr,], k = k, method = "Gibbs",
                  control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
      
      per=c(per,perplexity(Gibbs,newdata=dtm[te,]))
      loglik=c(loglik,logLik(Gibbs,newdata=dtm[te,]))
    }
    per_ctm=rbind(per_ctm,per)
    log_ctm=rbind(log_ctm,loglik)
  }
  return(list(perplex=per_ctm,loglik=log_ctm))
}

sp=smp(n=dtm$nrow, seed=seed_num) # n = nrow(dtm)

ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)

## plot the perplexity

m_per=apply(ctmK[[1]],1,mean)
m_log=apply(ctmK[[2]],1,mean)

k=c(kv_num)
df = ctmK[[1]]  # perplexity matrix
logLik = ctmK[[2]]  # perplexity matrix

logLiktest<-data.frame(k, df, logLik)
write.csv(logLiktest,"C:\\Users\\11\\Desktop\\LDA\\0818-1.csv")

# save the figure
png(paste("0818-2", ".png", sep = ''), width=10, height=10, units="in",res=700, pointsize = 12) 
matplot(k, df, type = c("b"), xlab = "Number of topics", 
        ylab = "Perplexity", pch=1:try_num,col = 1, main = '')       
legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num) 
dev.off()

png(paste("0818-3", ".png", sep = ''), width=10, height=10, units="in",res=700, pointsize = 12)
matplot(k, logLik, type = c("b"), xlab = "Number of topics", 
        ylab = "Log-Likelihood", pch=1:try_num,col = 1, main = '')       
legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num) 
dev.off()

#��0818-3��̰ܳ��I����topic�A�άO��ܹ�0818-2��perplexity�̤p��topic��

#�ӷ� http://cos.name/2013/08/something_about_weibo/
k = 40 #�@�@�n�h��topic�M�w��W�z���
SEED <- 2003

jss_TM2 <- list(
  VEM = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs = LDA(dtm, k = k, method = "Gibbs", 
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
  CTM = CTM(dtm, k = k, 
            control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))) )   

#save(jss_TM2, file = paste(getwd(), "/jss_TM2.Rdata", sep = ""))
#save(jss_TM, file = paste(getwd(), "/jss_TM1.Rdata", sep = ""))

#�H�U�s�ɮɪ��Ʀr���M�w�C�@���ߵ�(topic)�n�x�s�h�֭ӵ�
#���Ҭ��C�ؤ�k�������P��excel���x�s�A�C��excel�ɤ���40��topic�A�C��topic��10�ӵ�

termsForSave1<- terms(jss_TM2[["VEM"]], 10) 
#�̥D�n����k�O???�ݦ�������̤j�ƺ�k(VEM)�C�o�Ӥ�k�O�̥D�n�ϥΪ���k�C�bR��tomicmodels�]���Q��???�ϥ�
termsForSave2<- terms(jss_TM2[["VEM_fixed"]], 10)
termsForSave3<- terms(jss_TM2[["Gibbs"]], 10)
#��˪���k�A�p�N������ˡ]gibbs sampling�^�D�n�O�c�y�@???��������A�q���窺���Ҫ�����������@�Ǽ˥��A�H�����p��������C�N������ˤ�k�bR��lda�]���s�x�ϥΡC
termsForSave4<- terms(jss_TM2[["CTM"]], 10)
#���\�D???�����s�b�����C�Ѧ�Blei���H���X�F�������D???�ҫ��]CTM�^�A�i�H�ϥ�VEM��k���p


termsForSave1<-iconv(termsForSave1, "UTF-8")
write.csv(as.data.frame(t(termsForSave1)),"C:\\Users\\11\\Desktop\\LDA\\1.csv")

termsForSave2<-iconv(termsForSave2, "UTF-8")
write.csv(as.data.frame(t(termsForSave2)),"C:\\Users\\11\\Desktop\\LDA\\2.csv")

termsForSave3<-iconv(termsForSave3, "UTF-8")
write.csv(as.data.frame(t(termsForSave3)),"C:\\Users\\11\\Desktop\\LDA\\3.csv")

termsForSave4<-iconv(termsForSave4, "UTF-8")
write.csv(as.data.frame(t(termsForSave4)),"C:\\Users\\11\\Desktop\\LDA\\4.csv")

#####termsForSave1���ߵ����R###################################
tfs = as.data.frame(termsForSave1, stringsAsFactors =F)
tfs[,1] #�i��excel���Ĥ@��topic���r��

adjacent_list = lapply(1:5, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:5, function(i) rep(i, 9)))
edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
# edge.color="black"
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 5), function(i) rep(i, 9))); unique(E(g)$color)

png(paste("1", ".png", sep = ''), width=10, height=10, units="in",res=700, pointsize = 12)
plot(g,vertex.label= nodeLabel,  edge.curved=TRUE,vertex.label.cex =0.8,  edge.arrow.size=0.5, layout=l  )
dev.off()

#####termsForSave2���ߵ����R###################################
tfs2 = as.data.frame(termsForSave2, stringsAsFactors = F)
tfs2[,1] #�i��

adjacent_list = lapply(1:5, function(i) embed(tfs2[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:5, function(i) rep(i, 9)))
edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
# edge.color="black"
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 5), function(i) rep(i, 9))); unique(E(g)$color)

png(paste("2", ".png", sep = ''), width=10, height=10, units="in",res=700, pointsize = 12)
plot(g,vertex.label= nodeLabel,  edge.curved=TRUE,vertex.label.cex =0.8,  edge.arrow.size=0.5, layout=l  )
dev.off()

#####termsForSave3���ߵ����R###################################
tfs3 = as.data.frame(termsForSave3, stringsAsFactors = F)
tfs3[,1] #�i��

adjacent_list = lapply(1:5, function(i) embed(tfs3[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:5, function(i) rep(i, 9)))
edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
# edge.color="black"
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 5), function(i) rep(i, 9))); unique(E(g)$color)

png(paste("3", ".png", sep = ''), width=10, height=10, units="in",res=700, pointsize = 15)
plot(g,vertex.label= nodeLabel,  edge.curved=TRUE,vertex.label.cex =0.8,  edge.arrow.size=0.5, layout=l  )
dev.off()

#####termsForSave4���ߵ����R###################################
tfs4 = as.data.frame(termsForSave4, stringsAsFactors = F)
tfs4[,1] #�i��

adjacent_list = lapply(1:5, function(i) embed(tfs4[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:5, function(i) rep(i, 9)))
edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
# edge.color="black"
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 5), function(i) rep(i, 9))); unique(E(g)$color)

png(paste("4", ".png", sep = ''), width=10, height=10, units="in",res=700, pointsize = 12)
plot(g,vertex.label= nodeLabel,  edge.curved=TRUE,vertex.label.cex =0.8,  edge.arrow.size=0.5, layout=l  )
dev.off()
