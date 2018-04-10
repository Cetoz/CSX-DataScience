library(rvest)
library(tidytext)
library(knitr)
library(Matrix)
library(ggplot2)
library(tm)

page <- c(1:25)
urllist=list()
for(i in page){
url='http://news.ltn.com.tw/search/?keyword=孫安佐&conditions=and&SYear=2018&SMonth=01&SDay=09&EYear=2018&EMonth=04&EDay=09&page='
pasteurl <- paste0(url,i)
urllist <- rbind(urllist,pasteurl)
}
ltnnews <- list()
for(i in page){
  docs <- read_html(urllist[[i]])
  
  title <- html_nodes(docs,'.tit p') %>%
    html_text(title)
  
  link <- html_nodes(docs,'a.tit') %>%
    html_attr('href') %>%
    paste0('http://news.ltn.com.tw/',.)
  
  titleandlink <- cbind(title,link)
  ltnnews <-rbind(ltnnews,titleandlink) 
}


content <- list()
for(i in c(1:length(ltnnews[,2]))){
  newshtml <- read_html(ltnnews[[i,2]])
  
  text <- html_nodes(newshtml,'.text p') %>% 
    html_text()
  releasetime <- html_nodes(newshtml,'.text span') %>% 
    html_text()
  if(length(text)==0){
    text <- html_nodes(newshtml,'p') %>% 
      html_text()
    releasetime <- html_nodes(newshtml,'.news_content .date') %>% 
      html_text()
  }
  
  if(length(releasetime)==0){
    releasetime <- html_nodes(newshtml,'.c_time') %>% 
      html_text()
  }
  if(length(releasetime)==0){
    releasetime <- html_nodes(newshtml,'.mobile_none') %>% 
      html_text()
  }
  if(length(releasetime)==0){
    releasetime <- NA
  }
  
text <- paste(text,collapse = "")
  timeandtext <- cbind(releasetime,text)
  content <- rbind(content,timeandtext)
  }
ltnnews <- cbind(ltnnews,content)

date <- list()
for(i in c(1:length(ltnnews[,3]))){
  d <- strsplit(ltnnews[[i,3]], split = "")[[1]][c(9:10)] %>% 
    paste(collapse = "")
  date <- rbind(date,d)
}

ltnnews <- as.data.frame(ltnnews)
date <- as.character(date)
ltnnews <- cbind(ltnnews,date)
ltnnews[,4] <- unlist(ltnnews[,4])


ltnnews[,4][ltnnews[,5]=='29'|ltnnews[,5]=='30'] %>% 
  write(file = '2930.txt')
ltnnews[,4][ltnnews[,5]=='31'|ltnnews[,5]=='01'] %>% 
  write(file = '3101.txt')
ltnnews[,4][ltnnews[,5]=='02'|ltnnews[,5]=='03'] %>% 
  write(file = '0203.txt')
ltnnews[,4][ltnnews[,5]=='04'|ltnnews[,5]=='05'] %>% 
  write(file = '0405.txt')
ltnnews[,4][ltnnews[,5]=='06'|ltnnews[,5]=='07'] %>% 
  write(file = '0607.txt')
ltnnews[,4][ltnnews[,5]=='08'|ltnnews[,5]=='09'] %>% 
  write(file = '0809.txt')

##############
d.corpus <- Corpus( DirSource("C:\\Users\\FuHung\\Documents\\GitHub\\CSX-DataScience\\week_5\\data") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

mixseg = worker(stop_word = 'C:\\Users\\FuHung\\Documents\\GitHub\\CSX-DataScience\\week_5\\stop_word.txt')
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}

seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- c('0203','0405','0607','0809','2930','3101')
for( id in c(2:n) ){
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
TDM <- TDM[,c(1,6,7,2,3,4,5)]
kable(head(TDM))
kable(tail(TDM))

tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum)


idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM
tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc.tfidf[delID,1]))
kable(tail(doc.tfidf[delID,1]))
TDM = TDM[-delID,]
doc.tfidf = doc.tfidf[-delID,]

TopWords = data.frame()
for( id in c(1:n) ){
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:5],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
kable(TopWords)
#############
filename <- data.frame('2930.txt','3101.txt','0203.txt','0405.txt','0607.txt','0809.txt')
filesize <- as.data.frame(apply(filename,1,file.size)/1024)
daytag <- c('29-30','31-01','02-03','04-05','06-07','08-09')
filesize <- cbind(daytag,filesize)
filesizeplot <- ggplot(data=filesize,aes(x=daytag,y=filesize[,2])) +
  geom_bar(stat = 'identity') +
  xlab('Day')+
  ylab('size_KB')+
  scale_x_discrete(limits = daytag)
#############
