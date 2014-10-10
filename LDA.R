rm(list=ls())
library(tmcn)
library(tm)
library(Rwordseg)
library(wordcloud)
library(cluster)

setwd("C:\\Users\\11\\Desktop\\LDA")
txt<-read.table('0815神鵰俠侶.txt',colClasses="character") 
#此方法為一段一段分不會有空白，以前讀入會有空白

#先新增詞
insertedwords <- c("黃蓉","獨孤求敗","子聰","丁大全","人廚子","九死生","馬鈺","小棒頭","大頭鬼","馬光佐","小王將軍","小龍女","尹志平","丘處機","王處一","王十三","公孫止","王志坦","王惟忠","無常鬼","尹克西","天竺僧","孫婆婆","孫不二","皮清雲","申志凡","馮默風","討債鬼","史伯威","史仲猛","史叔剛","史季強","史孟龍","聖因師太","尼摩星","李莫愁","達爾巴","劉處玄","朱子柳","傻姑","呂文德","祁志誠","李志常","瑛姑","弔死鬼","百草仙","陸鼎立","陸二娘","阿根","張志光","完顏萍","陸冠英","宋德方","陳大方","宋五","沙通天","靈智上人","郭靖","郭芙","郭襄","霍都","張君寶","張一氓","陳老丐","張二叔","陸無雙","楊過","武三通","武敦儒","武修文","武三娘","林朝英","耶律楚材","耶律燕","忽必烈","喪門鬼","俏鬼","蒙哥","狗頭陀","青靈子","歐陽峰","耶律齊","周伯通","洪凌波","點蒼漁隱","柔兒","郭破虜","侯通海","覺遠","柯鎮惡","趙志敬","洪七公","姬清玄","笑臉鬼","鹿清篤","崔志方","鄂爾多","薩多","黃藥師","程遙迦","魯有腳","彭連虎","韓無垢","童大海","韓老丐","彭長老","藍天和","瘦丐","程英","雷猛","裘千尺","煞神鬼","催命鬼","裘千仞","赫大通","瀟湘子","樊一翁","藏邊大醜","藏邊二醜","藏邊三醜","藏邊四醜","藏邊五醜","何沅君","耶律晉","裘千丈","南海神尼","慈恩","無色禪師"
                   ,"黃島主","赤練仙子","李仙姑","江南","冰魄銀針","赤練神掌","倒立","逆練","重陽宮","淫賊","全真派","摺扇","活死人","終南山","活死人墓","玉蜂","孫婆婆","玉蜂漿","姑姑","綢帶","祖師婆婆","寒玉","古墓派","傻蛋","斷龍石","白衣","五毒秘傳","媳婦兒","拂塵","玉蜂針","兵刃","遼國","蜈蚣","喝酒","臭蛤蟆","棒打雙犬","天下無狗","前輩","陸家莊","武林盟主","金剛杵","金絲手套","金輪","黃幫主","金針","馮鐵匠","情花","絕情谷","公孫谷主","漁網","暗器","金鈴索","淑女劍","君子劍","絕情丹","綠萼","全真劍法","劇毒","情花劇毒","俠之大者","鞠躬盡瘁","死而後已","醜鵰","毒蟒","劍魔","鵰兄","劍塚","重劍","神鵰","獨孤","蛇膽","解毒","紅花","襄陽","斷腸草","玄鐵劍","練劍","神鵰俠","西山一窟鬼","九尾靈狐","史氏兄弟","黑龍潭","小東邪","一燈","百花谷","獨臂","斷臂","心驚肉跳","斷腸崖","杞人憂天","無中生有","拖泥帶水","徘徊空谷","力不從心","行屍走肉","庸人自擾","倒行逆施","廢寢忘食","孤形只影","飲恨吞聲","六神不安","窮途末路","面無人色","想入非非","呆若木雞","萬獸山庄","穴道","十六年","五絕","南僧","西狂","北俠","中頑童","少林寺","國師","法王","嘉興","鐵槍廟","軟蝟甲","紅馬"
                   ,"玉女劍法","玉女素心劍法","天羅地網勢","美女拳法","玉簫劍法","五行掌法","五毒神掌","三無三不手","七星聚會","一陽指","千里傳音","狂風迅雷功","無上大力杵法","五輪大轉","龍象般若功","黯然銷魂掌","陰陽倒亂刃法","閉穴功夫","大伏魔拳法","潑水杖法","釋迦擲象功","壽木長生功","黃沙萬里鞭法","毒砂掌","鐵掌擒拿手","寒陰箭","風雷掌法","上天梯","陸家刀法","玉女心經","擒拿手","全真劍法","九陽真經")

insertWords(toTrad(iconv(insertedwords, "big5", "UTF-8"), TRUE))

word<- segmentCN(txt$V1)
word<-Corpus(VectorSource(word))

myStopWords <- c(stopwordsCN(), "人人","過道","一口氣", "的","了","是","不","她","他","我","在","你","這","也","那","得")

reuters <- tm_map(word, removeWords, myStopWords)
d.corpus<- tm_map(reuters , segmentCN, nature = TRUE)

d.corpu1 <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) =="userDefine"]
  })
  unlist(noun)
})
#因新增的詞詞性為userDefine，利用迴圈將其選入

d.vec <- sapply(d.corpu1 , paste, collapse = " ")
d.vec <- unique(d.vec)
d.corpu3 <- Corpus(VectorSource(d.vec))
dtm <- DocumentTermMatrix(d.corpu3, 
                          control = list(wordLengths=c(2,6),removeNumbers = TRUE,
                          removePunctuation  = list(preserve_intra_word_dashes = FALSE),
                          weighting = weightTf,encoding = "UTF-8"))

colnames(dtm)#檢查選入的名詞
rownames(dtm)
#以下為儲存字詞次數excel
a<-col_sums(dtm)
write.csv(a,"C:\\Users\\11\\Desktop\\LDA\\wordfreq.csv")



library(tmcn)
library(tm)
library(Rwordseg)
library(wordcloud)
library(slam)
library(topicmodels)
library(igraph)

#先對文本-詞矩陣進行簡單處理，以消除高頻率的詞被高估和低頻率的詞被低估的問題
#term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
#l1=term_tfidf >= quantile(term_tfidf, 0.5)
#dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]
summary(col_sums(dtm))
col_sums(dtm)#檢查選入的字詞

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

#圖0818-3選擇最高點當做topic，或是選擇圖0818-2中perplexity最小的topic數

#來源 http://cos.name/2013/08/something_about_weibo/
k = 40 #一共要多少topic決定於上述兩圖
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

#以下存檔時的數字為決定每一條脈絡(topic)要儲存多少個詞
#本例為每種方法都有不同的excel檔儲存，每個excel檔中有40個topic，每條topic有10個詞

termsForSave1<- terms(jss_TM2[["VEM"]], 10) 
#最主要的算法是???异式的期望最大化算法(VEM)。這個方法是最主要使用的方法。在R的tomicmodels包中被重???使用
termsForSave2<- terms(jss_TM2[["VEM_fixed"]], 10)
termsForSave3<- terms(jss_TM2[["Gibbs"]], 10)
#抽樣的算法，如吉布斯抽樣（gibbs sampling）主要是構造一???馬爾夫鏈，從後驗的實證的分布中抽取一些樣本，以之估計後驗分布。吉布斯抽樣方法在R的lda包中廣泛使用。
termsForSave4<- terms(jss_TM2[["CTM"]], 10)
#允許主???之間存在相關。由此Blei等人提出了相關的主???模型（CTM），可以使用VEM方法估計


termsForSave1<-iconv(termsForSave1, "UTF-8")
write.csv(as.data.frame(t(termsForSave1)),"C:\\Users\\11\\Desktop\\LDA\\1.csv")

termsForSave2<-iconv(termsForSave2, "UTF-8")
write.csv(as.data.frame(t(termsForSave2)),"C:\\Users\\11\\Desktop\\LDA\\2.csv")

termsForSave3<-iconv(termsForSave3, "UTF-8")
write.csv(as.data.frame(t(termsForSave3)),"C:\\Users\\11\\Desktop\\LDA\\3.csv")

termsForSave4<-iconv(termsForSave4, "UTF-8")
write.csv(as.data.frame(t(termsForSave4)),"C:\\Users\\11\\Desktop\\LDA\\4.csv")

#####termsForSave1的脈絡分析###################################
tfs = as.data.frame(termsForSave1, stringsAsFactors =F)
tfs[,1] #展示excel中第一條topic的字詞

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

#####termsForSave2的脈絡分析###################################
tfs2 = as.data.frame(termsForSave2, stringsAsFactors = F)
tfs2[,1] #展示

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

#####termsForSave3的脈絡分析###################################
tfs3 = as.data.frame(termsForSave3, stringsAsFactors = F)
tfs3[,1] #展示

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

#####termsForSave4的脈絡分析###################################
tfs4 = as.data.frame(termsForSave4, stringsAsFactors = F)
tfs4[,1] #展示

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

