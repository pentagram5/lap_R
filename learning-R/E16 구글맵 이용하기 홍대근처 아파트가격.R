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
library(devtools)
googleAPIkey<-"AIzaSyD548iSahGuxgvGqfg8P0ytIVF6ksn4NMw"  
register_google(googleAPIkey)

setwd("C:/Rstudy/work_R")

apart_ex<-read_xlsx("DATA/아파트(매매)_실거래가_20200417134037.xlsx")
View(apart_ex)

####지하철 정보 가져오기 
station_data <- read.csv("DATA/지하철역별_주소_전화번호.csv")
station_data

station_code <- as.character(station_data$"구주소") %>% enc2utf8() %>% geocode()
station_code

station_codefinal <-cbind(station_data, station_code)
station_codefinal



####아파트 정보 가져오기 
apart_data <- read.csv("DATA/아파트_실거래가.csv")
class(apart_data$전용면적)
apart_data$전용면적 = round(apart_data$전용면적)
head(apart_data)

count(apart_data, 전용면적) %>%  arrange(desc(n))

apart_data_85 <- subset(apart_data, 전용면적 == "85")
apart_data_85

apart_data_85$거래금액 <- gsub(",", "", apart_data_85$거래금액)

apart_data_85_cost <- aggregate(as.integer(거래금액)~단지명, apart_data_85, mean)
apart_data_85_cost                          


apart_data_85_cost <- rename(apart_data_85_cost, "거래금액" = "as.integer(거래금액)")

apart_data_85<- apart_data_85[!duplicated(apart_data_85$단지명),]
#중복 데이터 제거 

apart_data_85 <- left_join(apart_data_85, apart_data_85_cost, by ="단지명")
apart_data_85

apart_data_85 <- rename(apart_data_85, "거래금액" = "거래금액.y")


apart_address <- paste(apart_data_85$"시군구", apart_data_85$"번지") %>% data.frame()
#데이터 프레임 형태로 변환하기 
apart_address

apart_address <- rename(apart_address, "주소" = ".")
apart_address_code <- as.character(apart_address$"주소") %>%  enc2utf8() %>%  geocode()
apart_address_code

apart_code_final <- cbind(apart_data_85, apart_address, apart_address_code) %>% 
  select("단지명", "전용면적", "거래금액", "주소", lon, lat)
apart_code_final


#홍대입구역으로 기준잡기
hd_map <- get_googlemap("hongdae station", maptype = "roadmap" , zoom=15)

ggmap(hd_map) +
  geom_point(data = station_codefinal, aes( x = lon, y = lat), colour = "red", size = 3) +
  geom_text(data = station_codefinal, aes(label = 역명, vjust = -1)) +
  geom_point(data = apart_code_final, aes(x = lon, y = lat)) +
  geom_text(data = apart_code_final, aes(label = 단지명, vjust = -1)) +
  geom_text(data = apart_code_final, aes(label = 거래금액, vjust = -1))


37.5578176,126.9230235

leaflet(station_codefinal) %>% setView(lng = 126.9230235, lat = 37.5578176, zoom =13)%>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircles(lng = ~lon, lat = ~lat, dashArray = 'black', color = 'black', radius = 100,
             label = station_codefinal$역명) %>% 
  addMarkers(lng = apart_code_final$lon, lat = apart_code_final$lat, 
             popup =  paste("<b>단지명:</b> : ", apart_code_final$단지명, "<br>",
                            "<b>거래금액</b> : ", apart_code_final$거래금액))

#%>% 
  addMarkers(lng = ~lon, lat = ~lat, popup =  paste("<b>동이름:</b> : ", addr_cofffinal$., "<br>",
                                                    "<b>커피숍 개수</b> : ", addr_cofffinal$Freq)) %>% 
  addCircleMarkers(lng = detail$lon, lat = detail$lat, radius = 0.1, opacity = 0.2, 
                   popup = paste("<b>커피숍이름</b> : ", detail$사업장명))  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
