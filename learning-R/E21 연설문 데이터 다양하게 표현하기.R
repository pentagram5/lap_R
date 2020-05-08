library(KoNLP)
library(wordcloud)

useSejongDic()
.libPaths()

data1<-readLines("DATA/steve.txt")
data1 <- sapply(data1, extractNoun, USE.NAMES = F)
data1
data2 <- unlist(data1)
data3 <- Filter(function(x){nchar(x) <= 10}, data2) 
#Filter의 입력을 data2로 하고, 10개 이하의 단어 추출 및텍스트 저장 



head(unlist(data3), 30)

for3 <- function(data3)
{
data3<-gsub("\\.", "", data3)
data3<-gsub(" ", "", data3)
data3<-gsub("\\^", "", data3)
data3<-gsub("\\'","",data3)#작은따옴표 글제거
data3<-gsub("\\d+", "", data3)#숫자제거
data3<-gsub("the", "", data3)
data3<-gsub("and", "", data3)
data3<-gsub("was", "", data3)
data3<-gsub("of", "", data3)
data3<-gsub("that", "", data3)
data3<-gsub("had", "", data3)
data3<-gsub("with", "", data3)
data3<-gsub("have", "", data3)
data3<-gsub("what", "", data3)
data3<-gsub("all", "", data3)
data3<-gsub("out", "", data3)
data3<-gsub("and", "", data3)
data3<-gsub("you", "", data3)
data3<-gsub("And", "", data3)
data3<-gsub("for", "", data3)
data3<-gsub("from", "", data3)

return(data3)
}

data3<-for3(data3)
#데이터 정제 


data3 <- Filter(function(x) {nchar(x) >= 3}, data3)
write(unlist(data3), "jops.txt")
data4 <- read.table("jops.txt")
nrow(data4)
wordcount <-table(data4)
head(sort(wordcount, decreasing = T), 50)
#data3 txt 저장 및 data4로 테이블 가져오기 

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word , word = data4, freq = Freq)
df_word
nrow(df_word)
#데이터 프레임화




top20 <-df_word %>% arrange(desc(freq)) %>% 
  head(20)

top10 <-df_word %>% arrange(desc(freq)) %>% 
  head(10)
#3d pie용 


order <- arrange(top20, freq)$word
#정제된 데이터중 빈도수 높은것 변수화

ggplot(data = top20, aes(x= word, y = freq, fill = word)) +
  ylim(0, 20) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = -0.3)
#막대 그래프 그리기

f_day <- round(top20$freq/sum(top20$freq) * 100, 1)
f_day
f_label <- paste(top20$word, "=", f_day,"%")
pie3D(top20$freq, radius = 1,main = "Steve jobs 연설문 단어", 
      col = rainbow(length(top20$freq)), cex = 0.1, 
      labels = f_label, labelcex = 0.8, 
      explode = 0.01, theta = pi/3, height = 0.1)
#pie 3D 그리기 

palete <- brewer.pal(9,"Set1")
wordcloud( words = df_word$word,  #단어
           freq = df_word$freq,   #빈도
           min.freq = 1,          #최소 단어 빈도
           max.words = 150,       #표현 단어 수 
           random.order = F,      #고빈도 단어 중앙 배치
           rot.per = .1,          #회전단어 비율
           scale = c(5, 0.2),     #단어크기범위
           colors = palete,
           random.color = T) 


wordcloud2(df_word, shape='circle', size = 0.5,  color = "random-light", backgroundColor = "black", ellipticity =0.8)
#워드크라우드 그림화

