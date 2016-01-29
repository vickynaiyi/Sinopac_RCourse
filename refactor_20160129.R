
library(httr)
library(CSS)

####################################################
# getListPageUrls via boardName
####################################################
library(rvest)


# function input: boardName
boardName = "Gossiping"

boardUrl = sprintf("https://www.ptt.cc/bbs/%s/index.html",boardName)
res = GET(boardUrl, config=set_cookies('over18'='1')) %>% 
  content(as = "text") %>% 
  read_html(res)

res_href = res %>% 
  html_nodes(xpath = '//*[@id="action-bar-container"]/div/div[2]/a[2]') %>% 
  html_attr("href")
res_href

maxPage = as.numeric(gsub(".html","",unlist(strsplit(res_href,split = "index"))[2]))

allListPages = c("",1:maxPage)
allListUrls = sapply(allListPages,function(page){
  sprintf("https://www.ptt.cc/bbs/%s/index%s.html",boardName,page)
})
# function output: allListUrls
allListUrls

# length(allListUrls)

####################################################
# getPostUrls via listPageUrl
####################################################

# function input: listPageUrl
listPageUrl = allListUrls[10]

res <- GET(listPageUrl,set_cookies(over18=1))
node = content(res, encoding = "utf8")

node[cssToXpath(".title a")]

postUrls = cssApply(node,".title a",function(node){
  sprintf("https://www.ptt.cc%s",xmlAttrs(node)["href"])
})
# function output: postUrls
postUrls

####################################################
# getPostData via postUrl
####################################################

# function input: postUrl
postUrl <- "https://www.ptt.cc/bbs/Gossiping/M.1454027289.A.FB0.html"
res <- GET(postUrl,set_cookies(over18=1))
node = content(res, encoding = "utf8")

res = GET(postUrl, config=set_cookies('over18'='1')) %>% 
  content(as = "text") %>% 
  read_html(res)

postData = list()
postData$Board = 
  res %>% html_nodes(".article-metaline-right > .article-meta-value") %>% 
  html_text()
#  cssApply(node,".article-metaline-right > .article-meta-value",cssCharacter)

metaTemp <- res %>% html_nodes(".article-metaline > .article-meta-value") %>% 
  html_text()

# metaTemp = cssApply(node,".article-metaline > .article-meta-value",cssCharacter)

postData$Author = metaTemp[1]
postData$Title = metaTemp[2]
postData$Time = metaTemp[3]

removeNodes(node["//div[@id='navigation-container']"])
removeNodes(node["//div/div/div[@class='article-metaline']/span[@class='article-meta-value']"])

postData$Text = xmlValue(node["//div/div[@id='main-content']"][[1]])

postData$postUrl = postUrl
postData$postId = gsub("[/]|.html","",unlist(strsplit(postUrl,postData$Board))[2])

pustData = lapply(node[cssToXpath("div.push")],function(test){
  list(pushTag=xmlValue(test["span"][[1]]),
       userId=xmlValue(test["span"][[2]]),
       text=xmlValue(test["span"][[3]]),
       time=xmlValue(test["span"][[4]]))
})


pushDf = data.frame(postId = postData$postId,
                    postUrl = postUrl,
                    do.call(rbind,pustData),stringsAsFactors = FALSE)

# function output: 
list(postData=postData,pushDf=pushDf)
