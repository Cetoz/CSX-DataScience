#擷取蔡英文FB貼文
install.packages('Rfacebook')
library(Rfacebook)
token <- 'EAACEdEose0cBAC5YZAbdxFfiOZB1qixkAsEQeZB8pPKat7ZA1k6AElkqH5qlL3HaiDN2I3nuY2cBv3jDy73gYuBzYJOE82yAr25aEHtd2wW1XhsmhrjA0oliFX5UqKYv2enRunBztnJ9gEPnuqDG16KZCUCRzBFHNujgU6MfTAwbUXZCC93D8SmbE9KZBCK4ucZD'
page.id <-46251501064
page <- getPage(page.id, token=token, n = 100)

#使用jiebaR斷詞
install.packages('jiebaR')
library(jiebaR)

cutter = worker(stop_word = 'stop_word.txt')
docs <- gsub('[0-9a-zA-Z]','',page$message)
docs <- cutter[docs]
docs<- table(docs)
docs <- data.frame(docs)
head(docs[order(docs$Freq,decreasing = TRUE),])

  #新增詞彙
  #new_user_word(cutter,)

#word cloud
install.packages('wordcloud')
library(wordcloud)
colors <- brewer.pal(5,'Dark2')
wordcloud(docs$docs,docs$Freq,
          min.freq = 10,random.order = FALSE,colors = colors,random.color = TRUE)
