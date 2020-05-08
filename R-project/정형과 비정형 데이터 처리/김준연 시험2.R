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
googleAPIkey<-"AIzaSyD548iSahGuxgvGqfg8P0ytIVF6ksn4NMw"  
register_google(googleAPIkey)

setwd("C:/Rstudy/work_R")


####지하철 정보 가져오기 
ec_car <- read.csv("5_car.csv")
head(ec_car)

#서울특별시로 기준잡기 
se_map <- get_googlemap("Seoul", maptype = "roadmap" , zoom=11)

ggmap(se_map) +
  geom_point(data = ec_car, aes( x = 경도, y = 위도, label = 충전소명), colour = "blue", size = 3)

37.5578176,126.9230235

leaflet(ec_car) %>% setView(lng = 126.9230235, lat = 37.5578176, zoom =10)%>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircles(lng = ec_car$경도, lat = ec_car$위도, dashArray = 'blue', color = 'blue', radius = 100,
             label = ec_car$충전소명) 

leaflet(ec_car) %>% 
  setView(lng = 127.7143303, lat = 36.1824482, zoom =7)%>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addTiles() %>% addMarkers(lng = ec_car$경도, ec_car$위도,  
                             clusterOptions = markerClusterOptions(),
    popup =  paste("<b>충전소 명 :</b> ", ec_car$충전소명, "<br>"))




#데이터 분석ㅎ ㅐ보기 

data <- read.csv("5_sales_method.csv")
data

#코딩변경
data$method2[data$method == 1]<-"집체교육"
data$method2[data$method == 2]<-"멘토링 교육"


#교육방법렬 교육을 이수한 사람들의 빈도수
table(data$method2)

#판매실적 평균값 
mean(data$performance)


#판매실적이 존재하는 인력 필터링 
data2 <- data %>%  filter(performance != 9999)

t.test(data = data2, performance ~ method2, var.equal = T)

data2_table <- as.data.frame(table(data2$method2, data2$performance))

data_1
gg <-ggplot(data2, aes(x= performance, fill = method2))+geom_bar(position = "dodge")
plotly::ggplotly(gg)
data2 %>% filter(method2 == "집체교육") %>% select(data2$performance)

data_2 <- data2 %>% filter(method2 == "멘토링 교육")
boxplot(data_1$performance, data_2$performance)


plot(data_1)
hist(data$performance)
lines(data_1$performance, col = "black", lwd = 3)



CrossTable(data2$method2, data2$performance, chisq = T)

