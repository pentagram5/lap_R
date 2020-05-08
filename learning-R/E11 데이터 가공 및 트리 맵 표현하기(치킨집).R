library(foreign)#foreign 패키지 설치 (spss, sas, stata 등 통계분석 s/w)
library(dplyr)
library(ggplot2)
library(readxl)

##### 치킨집분포도 조사해보기!#####
setwd("C:/Rstudy/work_R")
ck <- read_xlsx("DATA/chicken.xlsx")
ck
View(ck)

addr<- substr(ck$소재지전체주소, 12, 16)#db와 같이 substr(칼럼, 시작포인트, 끝포인트) 문자 잘라내기!
addr

addr_num <- gsub("[0-9]", "", addr) #숫자제거 [0-9] 숫자를 ""으로 대체하겠다(gsub -> 문자 or 숫자  대체)
addr_trim <- gsub(" ", "", addr_num) #공백제거 

#table 함수를 이용하여 숫자세기, 변수가 한개일때 도수 분표표를 만들어줄수 있다.
addr_count <- addr_trim %>%  table() %>%  data.frame()
addr_count

install.packages("treemap")
library(treemap)

#treemap(데이터, index = 인덱스 표시 열 제목, vSize = 크기를 이용할 열 제목, vColor = 컬러, title = 제목)

treemap(addr_count, index = ".", vSize = "Freq", title = "서대문구 동별 치킨집 분표표")
arrange(addr_count, desc(Freq)) %>%  head()




#커피
coff <- read_xlsx("DATA/gangnam_coffee.xlsx")
coff

addr_coff<- substr(coff$소재지전체주소, 11, 13)
addr_coff

addr_coffcount <- addr_coff %>%  table() %>% data.frame()
addr_coffcount

treemap(addr_coffcount, index = ".", vSize = "Freq", title = "강남구 동별 커피숍 분표표")
arrange(addr_coffcount, desc(Freq)) %>%  head()
