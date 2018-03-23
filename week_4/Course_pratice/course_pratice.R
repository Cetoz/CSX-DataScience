library(Rfacebook)
token <- 'EAACEdEose0cBAOsMKc2s7avlFYikGgI3w1mJOJK5dV4ZAfxQTrpDNcsX6Yiv8bOWZCPog4KvD9LD3uTnMGl9ZAOfetMFR8Y3n76pKO5RlzD9KKy5JG9X341FH4eGtyhDuZCMwAXNZBaiOOUW0y3eZBeULhBhi6HI3k6SyzAZAieFts2iYc0ZBmtIgQLyibaTpZBEZD'

me <- getUsers("me", token, private_info = TRUE)
me$name

page.id <- "232716627404" 
page <- getPage(page.id, token=token,n=1000,since = '2017/01/01',until = '2017/12/31')
str(page)
View(page)
?getPage
write.csv(page,'kikuChen.csv')

install.packages('NLP')
install.packages('tm')
install.packages('jiebaRD')
install.packages('jiebaR')
install.packages('wordcloud')
install.packages('slam')
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(slam)
library(wordcloud)

docs <- Corpus(VectorSource(page$message))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我們")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "與")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "以")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "以及")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[-c(1:34),]
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

