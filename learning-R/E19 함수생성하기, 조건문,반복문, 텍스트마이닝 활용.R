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

######기초문법->함수생성하기#####

#입력값 없는 경우 기본적인 사용자 정의함수 생성

myfunt1 <- function(){
  return(10)
}

myfunt1()

myfunt2 <- function(a){
  b<- a^2
  return(b)
}
myfunt2(10)

myfunt3 <- function(a,b){
  c <- a*b
  return(c)
}

myfunt3(10,10)

myfunt4 <- function(a,b){
  c <- a-b
  
  return(abs(c))
}#abs -> 절대값을 표현해주는 함수 


myfunt4(3,5)

#####조건문과 반복문 사용하기#####

myfunt6 <- function(a,b){
  c <- a- b
  if(c <0){
    return(-c)}
  else return(c)
}

myfunt6(2,3)

myfunt7 <- function(x)
{
  
  if(x<0){
    return(0)}
  else return(x*x)
}

myfunt7(-2)
myfunt7(2)

myfunt8 <- function(x){
  
  if(x<0){return(x*-2)}
  else if(x == 0){return(0)}
  else return(x*2)
}
myfunt8(10)

##ifelse 문 
no <-scan()

ifelse(no %%2 == 0, '짝수', '홀수')#ifelse(조건,a,b)조건이 참이면 a, 거짓이면 b

i = 1:100#i에 1부터 100까지 저장 

ifelse(i <= 50, 'hello', 'goodbye')


#기초문법 문제 풀어보기
myf1 <- function(x)
{
  if(x>5){return(1)}
  else return(0)
}


myf2 <- function(x)
{
  ifelse(x>=0,1,0)
}

myf3<- function(a, b)
{
  ifelse(a>b, a-b, b-a)
}

myf4 <-function(x)
{
    if(x >5) {return(10)}
    else ifelse(x>0, 1, 0)
}


myf4 <-function(x)
{
  ifelse(x >5, 10, ifelse(x>0,1,0)) 
}


myf4(6)
myf4(5)
myf5 <- function(x)
{
  ifelse( x == 'y' | x =='Y', 'yes', 'not yes')
}



###while 반복문
no <- 100

while(no< 5){
  print(no)
  no <- no+1
}


while(no > 0){
  print(no)
  no <- no-1
}

x <- 1
while(x<5){ x <- x+1; if(x==4) break; print(x)} #x ==4 일때 조건문 break

while(x<5){ x <- x+1; if(x==3) next; print(x)}#x==3일때 프린트 넘기기 


##for 문

for( x in 1000:1){ #for( 변수 in 시작지점:끝지점 )
  print(x) 
}

for1 <- function(x)
{
  for( i in 1:x *10)
  { #1부터 x까지 10을 곱하면서  i 출력하기 
    print(i)
  }
}
for1(10)
  

for2 <- function(x)
{
  i <- 0
  for(j in 1:x){
    i <- i+j
  }
  print(i)
}
for2(556454000)

#홀수 출력하기 
for2 <- function(x)
{
  i <- 0
  y <- 1
  for(j in 1:x){
    print(y)
    ifelse(y+2 > x, break,y<-y+2)
  }
}
for2(10)

#총합더하기 
for3 <- function(x)
{
  i<- 0
  
  for(j in 1:x){
      i <- i+j
  }
  print(i)
}
for3(100)


myf6 <- function(a, b){
  ifelse(a >0 && b >0, a*b, a+b)
}
myf6(10,3)


for4 <- function(x)
{
  
  for(i in 1:10*x)
  {
    print(paste(i, "번학생 차렷!"))   
  }
}

for4(10)
useNIADic()
useSejongDic()
setwd("C:/Rstudy/work_R")
vag <- readLines("채소.txt")
View(vag)
vag <-as.data.frame(vag)


#####분석 실전 예제->텍스트마이닝, 워드크라우딩(성형수술)#####
library(KoNLP)
library(wordcloud)
useSejongDic()
.libPaths()

data1<-readLines("DATA/remake.txt")
data1 <- sapply(data1, extractNoun, USE.NAMES = F)
data1
data2 <- unlist(data1)
data3 <- Filter(function(x){nchar(x) <= 10}, data2) 
#Filter의 입력을 data2로 하고, 10개 이하의 단어 추출 및텍스트 저장 



head(unlist(data3), 30)

data3<-gsub("쌍수", "쌍꺼풀", data3)
data3<-gsub("쌍커풀", "쌍꺼풀", data3)
data3<-gsub("메부리코", "매부리코", data3)
data3<-gsub("\\.", "", data3)
data3<-gsub(" ", "", data3)
data3<-gsub("\\'","",data3)#작은따옴표 글제거
data3<-gsub("\\d+", "", data3)#숫자제거
data3
#데이터 정제 


txt <- readLines("DATA/성형gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt
i <- 1
for(i in 1:cnt_txt){
  data3<-gsub((txt[i]),"",data3)
}
#data3의 내용중, txt[i]번쨰의 내용과 같은게 있으면 삭제 


data3 <- Filter(function(x) {nchar(x) >= 2}, data3)
write(unlist(data3), "remake_2.txt")
data4 <- read.table("remake_2.txt")
nrow(data4)
wordcount <-table(data4)
head(sort(wordcount, decreasing = T), 30)
#data3 txt 저장 및 data4로 테이블 가져오기 

df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word , word = data4, freq = Freq)
df_word
df_word <- filter(df_word, nchar(word) >= 2)
#데이터 프레임화




top20 <-df_word %>% arrange(desc(freq)) %>% 
  head(20)
order <- arrange(top20, freq)$word
#정제된 데이터중 빈도수 높은것 출력

ggplot(data = top20, aes(x= word, y = freq, fill = word)) +
  ylim(0, 20) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = -0.3)
#막대 그래프 그리기


palete <- brewer.pal(9,"Set1")
wordcloud( words = df_word$word,  #단어
           freq = df_word$freq,   #빈도
           min.freq = 1,          #최소 단어 빈도
           max.words = 200,       #표현 단어 수 
           random.order = F,      #고빈도 단어 중앙 배치
           rot.per = .1,          #회전단어 비율
           scale = c(6, 0.2),     #단어크기범위
           colors = palete,
           random.color = T) 
#워드크라우드 그림화