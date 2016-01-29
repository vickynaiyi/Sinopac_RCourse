

if(.Platform$OS.type != "unix"){
  load("hatepolitics.rda")
  load("gossiping.rda")
} else{
  load("articles_big5.rds")
} 

library(jiebaR)
mixseg = worker()

## clean NULL ##
for (i in c(1:length(hatepolitics))){
  print(hatepolitics[[i]]$postData$Title)
  if( is.null(hatepolitics[[i]]$postData$Title) ) hatepolitics[[i]]$postData$Title <- ""
}

segRes.GD <- lapply(gossiping, function(msg) mixseg <= msg$postData$Title)
segRes.HP <- lapply(hatepolitics, function(msg) mixseg <= msg$postData$Title)

paste(segRes.GD[[1]],collapse = " ")

segRes <- append(segRes.HP,segRes.GD)

library(tm)
tmWordsVec = sapply(segRes,function(ws) paste(ws,collapse = " "))
corpus <- Corpus(VectorSource(tmWordsVec))
tdm = TermDocumentMatrix(corpus,control = list(wordLengths = c(1, Inf)))
inspect(tdm)

library(rpart)
library(rpart.plot)
library(maptree)
Features = t(as.matrix(tdm))
Labels = c(rep("HP",length(segRes.HP)),rep("G",length(segRes.GD)))
df = data.frame(Y=Labels,Features)
# View(df)


model = rpart(Y~.,data=df)
draw.tree(model)
draw.tree(model, nodeinfo = TRUE, cases = "articles")
rpart.plot(model)
model


