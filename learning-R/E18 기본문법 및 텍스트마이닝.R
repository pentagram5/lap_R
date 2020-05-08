library(foreign)#foreign 패키지 설치 (spss, sas, stata 등 통계분석 s/w)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(ggmap)
library(stringr)
library(grid)
library(leaflet)


input <- scan()

input2 <- scan(what = "")# 문자 입력시 꼭 what = ""
input
input2

input3 <- readline()
input3

input4 <- readline("are u ok?")
input4
install.packages("stringr")
library(stringr)

fruit <- c('apple', 'Apple', 'banana','pineapple')
str_detect(fruit, 'A') #대문자A가 있는 단어 찾기 
str_detect(fruit, '^a') #첫글자가 소문자 a인 단어찾기 
str_detect(fruit, 'e$') #끝나는 글자가 소문자 e인 단어찾기
str_detect(fruit, '^[aA]')#시작하는 글자가 대문자 A이거나 소문자 A인 단어 찾기 
str_detect(fruit, '[aA]')#단어에 소문자 a와 대문자 A가 들어있는 단어 찾기 

str_detect(fruit, ignore.case('a'))

str_c("apple", "banana")#문자열 합치기
str_c("Fruits: ", fruit)
str_c(fruit, " name is ", fruit)
str_c(fruit, collapse = "")#fruit 내의 문자열 합치기 
str_c(fruit, collapse = "-")

str_dup(fruit, 3)#fruit 내에서 반복 출력하기 

str_length('apple')#문자열 길이 출력
str_length(fruit)

str_locate('apple','a') #틀정 문자의 위치 값 찾기 
str_locate(fruit,'a')

str_replace('apple', 'p', '*')#특정문자를 원하는 문자로 바꿀때 사용 
str_replace_all('apple', 'p', '*')#모든 'p'를 *로 변경


fruit <- str_c('apple','/','orange','/','banana')
fruit

str_split(fruit, "/")#특정 문자 기준으로 스플릿,분리 


str_sub(fruit, start = 1, end= 3)#1번부터 3번까지의 문자를 출력해달라(주소출력)
str_sub(fruit, start= 6, end = 9)
str_sub(fruit, start= -5)# - 는 뒤쪽 부터 출력 
str_sub(fruit, start = 5, end= 15)

str_trim(' apple banana berry ')# 좌우의 공백 삭제


#####텍스트마이닝! #####
#문자로 된 데이터에서 가치있는 정보를 얻어 내는 분석 기법
#sns 나 웹 사이트에 올라온 글을 분석해 사람들이 어떤 이야기를 나누고 있는지 파악할 때 활용
#형태소 분석 : 문장을 구성하는 어절들이 어떤 품사로 되어 있는지 분석 
install.packages("rJava")
install.packages("memoise")
install.packages("Sejong")
install.packages("hash")
install.packages("tau")
install.packages("RSQLite")


library(KoNLP)
library(rJava)
library(dplyr)

Sys.setenv(JAVA_HOME = "C/Program Files/java/jdk1.8.0_201")


useNIADic()#NIA딕셔너리 사용하기 -> 한글 형태별로 저장됨 
useSejongDic()#세종 사전을 이용함 

setwd("C:/Rstudy/work_R")
txt <- readLines("0420_KoNLP/hiphop.txt")

head(txt)

txt <- str_replace_all(txt, "\\W", " ") #특수 문자 제거 
txt

#명사 추출하기 
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

#가사에서 명사 추출하기 
nouns <- extractNoun(txt)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount

#e데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <-rename(df_word, 
                 word = Var1,
                 freq = Freq)

#두글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20



##워드 클라우드 만들기 
#단어의 빈도를 구름 모양으로 료현한 그래프, 빈도에 따라 글자의 크기와 색깔이 다르게 표현된다
install.packages("wordcloud")
library(wordcloud)

library(RColorBrewer)

pal <- brewer.pal(8, "Dark2") #dark2 생각 목록에서 8개 색상 추출

set.seed(1234) #난수 고정

wordcloud( words = df_word$word,  #단어
           freq = df_word$freq,   #빈도
           min.freq = 2,          #최소 단어 빈도
           max.words = 200,       #표현 단어 수 
           random.order = F,      #고빈도 단어 중앙 배치
           rot.per = .1,          #회전단어 비율
           scale = c(3, 0.2),     #단어크기범위
           colors = pal)          #색깔 목록


#파란 계열의 색상을 만들어 빈도가 높을수록 진한 파란색으로 표현하기
pal <- brewer.pal(9, "Blues")[5:9]
             

## 국정원 트윗 텍스트 마이닝

#데이터 준비하기, 한글변수명을 영어이름으로 변경하고 특수문자 제거

twitter <- read.csv("DATA/twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")

#변수명 수정

twitter <- rename(twitter, 
                  no = 번호,
                  id = 계정이름, 
                  date = 작성일,
                  tw = 내용)

#특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
twitter

#트윗에서 명사추출
nouns <- extractNoun(twitter$tw)

wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word , word = Var1, freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)
top20 <-df_word %>% arrange(desc(freq)) %>% 
  head(20)
top20

#막대그래프 만들어보기

library(ggplot2)
order <- arrange(top20, freq)$word
ggplot(data = top20, aes(x= word, y = freq)) +
  ylim(0, 2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = -0.3)

wordcloud( words = df_word$word,  #단어
           freq = df_word$freq,   #빈도
           min.freq = 10,          #최소 단어 빈도
           max.words = 200,       #표현 단어 수 
           random.order = F,      #고빈도 단어 중앙 배치
           rot.per = .1,          #회전단어 비율
           scale = c(8, 0.2),     #단어크기범위
           colors = pal)


##분석 실전
data1 <- readLines("DATA/seoul_new.txt")

data1<-str_replace_all(data1, "\\W", " ")
data1


data2 <-sapply(data1, extractNoun, USE.NAMES = F)

head(unlist(data2), 30)
data3 <-unlist(data2)
data3

#원하지 않는 내용 걸러내기
data3 <- gsub("\\d+","", data3)
data3 <- gsub("서울시","", data3)
data3 <- gsub("서울","", data3)
data3 <- gsub("요청","", data3)
data3 <- gsub("제안","", data3)
data3 <- gsub(" ","", data3)
data3 <- gsub("-","", data3)
data3

#파일로 저장한 후 테이블형태로 변환하여 부르기
write(unlist(data3), "seoul_2.txt")

data4 <- read.table("seoul_2.txt")
data4
nrow(data4)

wordcount <- table(data4)
wordcount

head(sort(wordcount, decreasing =  T), 20)


##새로 정제 해보기 
data3 <- gsub("OO","", data3)
data3 <- gsub("개선","", data3)
data3 <- gsub("문제","", data3)
data3 <- gsub("관리","", data3)
data3 <- gsub("민원","", data3)
data3 <- gsub("이용","", data3)
data3 <- gsub("관련","", data3)
data3 <- gsub("시장","", data3)
data3 <- gsub("님","", data3)
data3 <- gsub("한","", data3)
data3 <- gsub("문","", data3)
data3 <- gsub("역","", data3)
data3 <- gsub("동","", data3)
data3 <- gsub("적","", data3)
data3 <- gsub("앞","", data3)
data3 <- gsub("장","", data3)

data3

write(unlist(data3), "seoul_3.txt")
data4 <- read.table("seoul_3.txt")
data4
nrow(data4)

wordcount <- table(data4)
wordcount

head(sort(wordcount, decreasing =  T), 20)

pal <- brewer.pal(9, "Set3")

wordcloud( names(wordcount),  #단어
           freq = wordcount,   #빈도
           min.freq = 1,          #최소 단어 빈도
           random.order = F,      #고빈도 단어 중앙 배치
           rot.per = 0.25,          #회전단어 비율
           scale = c(5, 1),     #단어크기범위
           colors = pal,
           random.color = T)

#범례 표시하기 
legend(0.3,3,"서울시 응답소 요청사항 분석", cex = 0.8, fill = NA, 
       border =NA, bg = "white", text.col = "red", text.font=2, box.col="red")


#실습에 주로 사용되ㄴ패키지 함수 
##extractNoun ->한글의 명사 추출함수


#색상없이 워드 클라우드 생성하기 
wordcloud(c(letters, LETTERS, 0:9), seq(1, 1000, len = 62))
#영어 소문자와, 대문자, 숫자 0~9를 이용해 워드클라우드 생성 

#색상을 사용해 워드 클라우드 생성하기
palete <- brewer.pal(9,"Set1")
wordcloud(c(letters, LETTERS, 0:9), seq(1, 1000, len = 62), colors=palete)




###분석 실전 2
jeju <- readLines("DATA/jeju.txt")
jeju<-str_replace_all(jeju, "\\W", " ")

jejun <- extractNoun(jeju)
wordcount <- table(unlist(jejun))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word , word = Var1, freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)

palete <- brewer.pal(9,"Set1")


top20 <-df_word %>% arrange(desc(freq)) %>% 
  head(20)
order <- arrange(top20, freq)$word

ggplot(data = top20, aes(x= word, y = freq, fill = word)) +
  ylim(0, 100) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = -0.3)

wordcloud( words = df_word$word,  #단어
           freq = df_word$freq,   #빈도
           min.freq = 5,          #최소 단어 빈도
           max.words = 200,       #표현 단어 수 
           random.order = F,      #고빈도 단어 중앙 배치
           rot.per = .1,          #회전단어 비율
           scale = c(6, 0.2),     #단어크기범위
           colors = palete,
           random.color = T) 

