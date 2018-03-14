install.packages('rvest')
library(rvest)
#讀取台大首頁
ntu_news <- read_html("http://www.ntu.edu.tw/")
#擷取新聞標題
title <- ntu_news%>%html_nodes('#news p')%>%html_text()
#擷取新聞連結
url<- ntu_news%>%html_nodes('#news a')%>%html_attr('href')
#刪除無關連結
url <- url[2:13]
#用Dataframe儲存資料
news_df <- data.frame(Title=title,Url=url)
#輸出結果
print(news_df)