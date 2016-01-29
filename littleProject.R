# http://nbviewer.jupyter.org/github/whizzalan/RTextMining/blob/master/lightingTalk/lightingTalk.ipynb
library(jiebaR)      # 斷詞利器
library(tm)          # 文字詞彙矩陣運算
library(slam)        # 稀疏矩陣運算
library(wordcloud)   # 文字雲
library(topicmodels) # 主題模型
library(igraph)      # 主題模型關聯

# 建立斷詞器
mixseg = worker()

## Intro jiebaR ##
messages = c("你愛她","她愛你")
lapply(messages,function(msg) mixseg <= msg)

# 讀資料檔
readSpeaker <- function(file,sep){
  lines <- readLines(file)
  allString <- do.call(paste,as.list(lines))
  sepString <- strsplit(allString,sep,fixed=TRUE)
  return(sepString[[1]])
}

# 講者清單
if(.Platform$OS.type != "unix"){
  speakerUrl <- "https://raw.githubusercontent.com/datasci-info/SinopacRCourse/master/speaker.txt"
  download.file(speakerUrl, destfile = "speaker.txt")
} else{
  speakerUrl <- "https://raw.githubusercontent.com/datasci-info/SinopacRCourse/master/speaker_big5.txt"
  download.file(speakerUrl, destfile = "speaker.txt")
} 
speakers <- readSpeaker("speaker.txt",sep="-----")
length(speakers);speakers[1]

# 斷詞
first_speaker <- mixseg <= speakers[1]
first_speaker

# 斷詞過濾器(解決perl regular expression的bug)
cutter <- function(msg){
  filter_words = c("的","在","與","及","等","是","the","and","in","a","at","he","is","of","He")
  pattern <- sprintf("[^%s]", paste(filter_words, collapse = "|^"))
  filter_seg <- grep(pattern, mixseg <= msg ,value=TRUE)
  return(filter_seg)
}
## 重新斷詞，並將字詞黏貼
segRes = lapply(speakers,cutter)
tmWordsVec = sapply(segRes,function(ws) paste(ws,collapse = " "))
tmWordsVec[1]

# 語料庫 & TDM
corpus <- Corpus(VectorSource(tmWordsVec))
tdm = TermDocumentMatrix(corpus,control = list(wordLengths = c(1, Inf)))
tdm

# 看看詞頻分的如何
dtm1 <- DocumentTermMatrix(corpus,
                           control = list(
                             wordLengths=c(1, Inf), # to allow long words
                             removeNumbers = TRUE, 
                             weighting = weightTf, 
                             encoding = "UTF-8")
)
# colnames(dtm1)
findFreqTerms(dtm1, 10) # 看一下高频词, he沒法filter掉

# 看字詞矩陣
m <- as.matrix(dtm1)
head(m)

# 畫個文字雲
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq, scale=c(6,0.5), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.01, colors=pal2)

# 利用tfidf 來處理高頻詞高估，低頻詞低估
dtm = dtm1
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1= term_tfidf >= quantile(term_tfidf, 0.5)       # second quantile, ie. median
summary(col_sums(dtm))
dim(dtm);dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm)
summary(col_sums(dtm))


# 建立主題模型 （出處參考 Ref 3）
fold_num = 10
kv_num =  seq(2,24)
seed_num = 2015
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

system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))

m_per=apply(ctmK[[1]],1,mean)
m_log=apply(ctmK[[2]],1,mean)

## Pike Your Fitting Model ##
k=c(kv_num)
#df = ctmK[[1]]  # perplexity matrix
logLik = ctmK[[2]]  # perplexity matrix

# matplot(k, df, type = c("b"), xlab = "Number of topics", 
#         ylab = "Perplexity", pch=1:try_num,col = 1, main = '')       
# legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num) 
matplot(k, logLik, type = c("b"), xlab = "Number of topics", 
        ylab = "Log-Likelihood", pch=1:try_num,col = 1, main = '')       
legend("topleft", legend = paste("fold", 1:try_num), col=1, pch=1:try_num)

## choose your k and fitting 4 model ##
k = which(logLik ==max(logLik))+1
SEED <- 2015
jss_TM2 <- list(
  VEM = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs = LDA(dtm, k = k, method = "Gibbs", 
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
  CTM = CTM(dtm, k = k, 
            control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))) )   

termsForSave1<- terms(jss_TM2[["VEM"]], 10)
termsForSave2<- terms(jss_TM2[["VEM_fixed"]], 10)
termsForSave3<- terms(jss_TM2[["Gibbs"]], 10)
termsForSave4<- terms(jss_TM2[["CTM"]], 10)

speakers[1]

#'topic graphs'
#tfs = as.data.frame(termsForSave1, stringsAsFactors = F);tfs
tfs = as.data.frame(termsForSave2, stringsAsFactors = F);tfs
#tfs = as.data.frame(termsForSave3, stringsAsFactors = F);tfs
#tfs = as.data.frame(termsForSave4, stringsAsFactors = F);tfs

adjacent_list = lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:10, function(i) rep(i, 9)))
edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
# edge.color="black"
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 10), function(i) rep(i, 9))); unique(E(g)$color)

plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, vertex.label.cex =1.25,  edge.arrow.size=0.2, layout=l)

# "[Ref 1: 宋詞主題分析](http://computational-communication.com/post/wen-ben-wa-jue/2013-09-27-topic-modeling-of-song-peom) ",
# "[Ref 2: JiebaR](http://qinwenfeng.com/jiebaR/index.html) ",
# "[Ref 3: 主題模型函數- 朱雪寧](http://cos.name/2013/08/something_about_weibo/) "