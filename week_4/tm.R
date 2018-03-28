install.packages('Rfacebook')
install.packages('tm')
library(Rfacebook)
library(tm)

token <- 'EAACEdEose0cBALUXJKJ4XuROSf4tLoSLAShlV2EQ9Gc24jiXG65sPNP0rDrrNocDvmv5m2hGqTCVjzLYohZC2iyHrWZCrCbjUZCwMzV4VbamgIc0EGrjGyKolAI68gMq4H2RCemWZBBOZAnVhCd91fPlBAXr3I9oKraxrTGk6JXB9w67pmDfnE30dejhwrmZARIMfohxHPQVkqZCLU45YLN'
page.id <-46251501064
page <- getPage(page.id, token=token, n = 100)

docs <- Corpus(VectorSource(page$message))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)

docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

inspect(docs[1:3])
