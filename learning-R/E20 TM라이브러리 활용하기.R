install.packages("tm")
library(tm)
data1 <- readLines("DATA/tm_test1.txt")
data1
class(data1)
corp1 <- Corpus(VectorSource(data1))
corp1
inspect(corp1)
tdm<-TermDocumentMatrix(corp1)
tdm
m <- as.matrix(tdm)
m

corp2 <- tm_map(corp1, stripWhitespace)#여러개의 공백을 하나의 공백으로 변환
corp2 <- tm_map(corp2, tolower)
corp2 <- tm_map(corp2, removeNumbers)
corp2 <- tm_map(corp2, removePunctuation)
#copr2 <- tm_map(corp2, PlainTextDocument)
sword2 <- c(stopwords('en'), 'and', 'but', 'not')
corp3 <- tm_map(corp2, removeWords, sword2)

tdm2 <- TermDocumentMatrix(corp3)
tdm2

m2 <- as.matrix(tdm2)
m2

freq1 <- sort(rowSums(m2), decreasing = T)
head(freq1, 20)
freq2 <- sort(colSums(m2), decreasing = T)
head(freq2, 20)

library(RColorBrewer)
palete <- brewer.pal(7, "Set3")
wordcloud(names(freq1), freq = freq1, scale=c(5,1), min.freq = 1, colors = palete, random.color = T, random.order = F)

barplot(freq1, main = "tm packagetest #2", las = 2,ylim = c(0,5) )
pie(freq1, col = rainbow(10), radius = 1, main = "tm package test #2")



#워드클라우드2 패키지 설치 및 실습
install.packages("wordcloud2")
library(wordcloud2)
word_data <- readLines("DATA/애국가(가사).txt")
word_data

word_data2 <- sapply(word_data,extractNoun, USE.NAMES = F)
add_words <- c("백두산", "남산", "철갑", "가을", "하늘", "달")
word_data2
buildDictionary(user_dic = data.frame(add_words, rep("ncn", length(add_words))), replace_usr_dic = T)
library(dplyr)
word_table <- table(undata)


wordcloud2(word_data2, color = "random-light", backgroundColor = "black")
