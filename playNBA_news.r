
# install.packages("tmcn.word2vec", repos="http://R-Forge.R-project.org")
# install.packages("tm")

library(tm)
getwd()
# setwd()
dir.Name <- "NBA_Pre&Rec_20160128" #在file裡面


##readLine
docs <- NULL
docs <- c(docs,list(paste(readLines(file("NBA_Pre&Rec_20160128/Mavericks-Warriors Preview")), collapse = "\n")))

myCorpus <- Corpus(VectorSource(docs))
myCorpus
str(myCorpus)
inspect(myCorpus)

words <- data.frame(c("This is a text.", "This another one."))
(ds <- DataframeSource(words))
inspect(Corpus(ds))

## creater DTM ##
myDtm <- DocumentTermMatrix(myCorpus)
dim(myDtm)
# View(as.matrix(myDtm))
dtmWords <- DocumentTermMatrix(Corpus(ds))
dim(dtmWords)
View(as.matrix(dtmWords))



## 六種常用清理英文字詞方法 ##
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- tm_map(myCorpus, removeWords, stopwords("SMART"))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
# meta(myCorpus)

length(colnames(as.matrix(DocumentTermMatrix(myCorpus))))

stopwords("english")
stopwords("SMART")

## reader DTM again ##
myDtm <- DocumentTermMatrix(myCorpus)
dim(myDtm)
# View(as.matrix(myDtm))
## another way ##
myDtm <- DocumentTermMatrix(myCorpus, control = list(weighting = weightSMART, stopwords = TRUE))

## remove Words ##
library(tm)
sentence <- "To be or not to be?"   # stemDocument(sentence)
## 只要處理過後就會太乾淨...所以要看需求
removeWords(sentence, stopwords())

## 進階文本可以用此作法！！
sentence <- "To be or not to be?"   
removeWords(sentence, stopwords("SMART"))

check_stopwords <- c("afterwards", "almost")
check_stopwords %in% stopwords() # 174
check_stopwords %in% stopwords("SMART") # 571

####
NBA_News <- Corpus(DirSource(directory = dir.Name, encoding = "UTF-8"))

NBA.dtm <- DocumentTermMatrix(NBA_News)
dim(NBA.dtm)

cleantoCorpus <- function(corpus){
  corpus <- NBA_News
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords())
  return(corpus.tmp)
}

NBA_Corpus <- cleantoCorpus(NBA_News)
NBA2.dtm <- DocumentTermMatrix(NBA_Corpus)
dim(NBA2.dtm)
# View(as.matrix(NBA2.dtm))

NBA2.dtm
NBA3.dtm <- removeSparseTerms(NBA2.dtm,0.6)
colnames(as.matrix(NBA3.dtm))


############################################################################################
# install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
dtm2 <- as.matrix(NBA3.dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing = TRUE)
words <- names(frequency)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(words[1:50], frequency[1:50], scale = c(6,0.5), max.words = 100,
          random.order=FALSE, rot.per=0.1, colors = pal2)



# #### classification ####
# libs <- c("tm", "dplyr", "class")
# lapply(libs,require, character.only=TRUE)



