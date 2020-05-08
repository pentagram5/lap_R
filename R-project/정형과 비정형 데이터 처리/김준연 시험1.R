#####library #####
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
library(KoNLP)
library(rJava)
library(moments)
library(Hmisc)
library(prettyR)
library(MASS)
library(gmodels)
setwd("C:/Rstudy/Work_R")


test <- read.csv("4_test_data1.csv")

##1. 거주지역 별 빈도수를 구하세요 
t3able(test$resident2)

#1.서울특별시 2.인천광역시 3.대전광역시 4.대구광역시     5.시구군 
#110           46           26           15           34 

##2. 성별 빈도수를 구하세요 
table(test$gender2)
#남자 여자 
#134   97

head(test)


#3. 성별에 따른 거주지역의 분포를 시각화 하여 나타내 보세요 
gg1<-ggplot(test, aes(x=test$gender2 , fill = test$resident2))+geom_bar(position = "dodge") +
  coord_flip()
plotly::ggplotly(gg1)



#4. 거주지역에 따른 성별 분포 현황을 시각화하여 나타네 보세요 
gg2<-ggplot(test, aes(x=test$resident2 , fill = test$gender2))+geom_bar(position = "dodge") +
  coord_flip()
plotly::ggplotly(gg2)



#5. 직종별 나이 분포 현황을 출력해 보세요 
gg2<-ggplot(test, aes(x=test$age , fill = test$job2))+geom_bar(position = "dodge") +
  coord_flip()
plotly::ggplotly(gg2)
