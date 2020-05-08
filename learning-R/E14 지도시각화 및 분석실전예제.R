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


setwd("C:/Rstudy/work_R")
#####그래프 분석 예제-구글api 이용#####


#####제주도 이동경로 표시하기 #####
googleAPIkey<-"AIzaSyD548iSahGuxgvGqfg8P0ytIVF6ksn4NMw"  
register_google(googleAPIkey)##주요함수-> 구글 APIkey 등록
jeju<- read.csv("DATA/제주도여행코스_1일차.csv", header= T)
jeju1<- get_map("Hallasan", zoom = 10, maptype = "hybrid")
jeju.map <- ggmap(jeju1)+geom_point(data = jeju, aes(x =LON, y= LAT), size = 3, alpha = 0.7, col="red")

#geom_path 함수를 써서 경로를 선으로 연결하여 표시
jeju.map + geom_path(data = jeju, aes(x=LON, y= LAT), size = 1, linetype = 2, col = "blue") +
    geom_text(data = jeju, aes(x=LON, y = LAT +0.01, label = 장소),size=3)

#####지도위에 서울 지하철 3호선 경로 표시하기#####
loc <- read.csv("DATA/서울지하철3호선역위경도정보.csv", header = T)
mapp1 <- get_map("MAPO-GU ", zoom = 11, maptype = "roadmap")
kor.map <- ggmap(mapp1)+ geom_point(data = loc, aes(x = LON, y = LAT), color = "orangered",size=2, alpha= 1)
kor.map + geom_path(data = loc, aes(x=LON, y= LAT), size = 1, linetype = 2, col = "black") +
  geom_text(data = loc, aes(x =LON, y = LAT+0.01, label = 역명), size =2)




#####송파구 전체 cctv #####
songpa <- read.csv("DATA/project_songpa_data.csv", header = T)
s_m <- get_map("songpagu", zoom = 13, maptype = "roadmap")
song.map <- ggmap(s_m)+geom_point(data = songpa, aes(x = LON, y = LAT), size = 2, alpha = 0.7, color = "#980000")
song.map

s_m <- get_map(location = c(lon = 127.128335, lat = 37.5022377), zoom = 14,
               maptype = "roadmap")
song.map <- ggmap(s_m) + geom_point(data = songpa, aes( x=LON, y = LAT), size = 3, alpha = 0.7, color ="#980000" )+
       geom_point(aes(x=127.128335, y = 37.5022377), size = 110, alpha = 0.1, color = "#003399")
song.map


#####한글 검색을 위해 utf8로 변환 후 위도와 경도 데이터를 geo_code 변수에 할당 #####
geo_code <- enc2utf8("대전역") %>% geocode()#대전역의 geocode를 할당 
geo_data <- as.numeric(geo_code)
geo_code
geo_data
#리스트를 숫자로 변환하여 geo_data 변수에 할당 
get_googlemap(certer = geo_data, maptype = "roadmap", zoom = 13) %>%  ggmap()+ 
  geom_point(data = geo_code, aes(x = geo_code$lon, y = geo_code$lat))



# 지하철역별 주소 geocode로 변환하기 

station_da <- read.csv("DATA/지하철역별_주소_전화번호.csv")
str(station_da)

geo_code <- as.character(station_da$구주소) %>%  geocode()

station_final <- cbind(station_da, geo_code)
head(station_final)

gangnam_map <- get_googlemap("Gangnam-gu", maptype = "roadmap", zoom= 12)
ggmap(gangnam_map)


ggmap(gangnam_map) + geom_point(data = station_final, aes( x = station_final$lon, y = station_final$lat),
                                colour = "red", size = 3) +
                     geom_text(data = station_final, aes(label = 역명, vjust = -1))




#####R로 인터랙티브 지도 그리기 (leaflet)#####
install.packages('leaflet')
library(leaflet)

m<- leaflet() %>%  addTiles() %>%  addMarkers(lng =174.768, lat = -36.852, popup = "The birthplace of R")
#addTiles -> 기본타일을 불러와서 지도에 보여줌 
m

#setView 지도보기의 중심과 확대
leaflet() %>%  setView(lng = 126.9784, lat = 37.566, zoom = 11) %>%  addTiles()

#addProviderTiles->leaflet으로 지도를 그릴때 지도스타일을 정하는 함수 
getwd()
sb <- read.csv('DATA/starbucks.csv')
sb


leaflet(sb) %>% setView(lng = 126.9784, lat = 37.566, zoom = 11) %>% addProviderTiles('Esri.WorldTopoMap')%>% 
  addCircles(lng = ~long, lat = ~lat, color = '#006633')#addCircles -> 데이터를 원의 형태로 지도에 나타넴
  

leaflet(sb) %>% setView(lng = 126.9784, lat = 37.566, zoom = 11) %>% addProviderTiles('Esri.WorldTopoMap')%>% 
  addMarkers(lng = ~long, lat = ~lat, label = ~address)#~는 ~coloum을 쓰겠다는 표현 
  #addMarkers -> 주소와 같은 정보 추가 하기 

#####강원도으뜸음식점 지도에 표시하기 

kangwon <- read.csv("DATA/강원도으뜸음식점.csv")
kangwon
geo_code <- as.character(kangwon$소재지지번주소) %>%  geocode()
kangwon_final <- cbind(kangwon, geo_code)
kangwon_map <- get_googlemap("Kanwondo", maptype = "roadmap", zoom= 9)
kangwon_map
ggmap(kangwon_map)
str(kangwon_final)

ggmap(kangwon_map) + geom_point(data = kangwon_final, aes( x = kangwon_final$lon, y = kangwon_final$lat),
                                colour = "blue", size = 3) +
  geom_text(data = kangwon_final, aes(label = 업소명, vjust = -1))

leaflet(kangwon_final) %>% setView(lng = 128.6652533, lat = 37.5730527, zoom = 9) %>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircles(lng = ~lon, lat = ~lat, color = '#006633',)#addCircles -> 데이터를 원의 형태로 지도에 나타넴




#popupTable사용하기 
install.packages("leafpop")
library(leafpop)
kangwon_table <- kangwon_final %>% 
  select(업소명, 소재지지번주소, 전화번호, 지정번호, 주요메뉴)
leaflet(kangwon_final) %>% setView(lng = 128.6652533, lat = 37.5730527, zoom = 8) %>% 
  addProviderTiles('Esri.WorldTopoMap')%>% 
  addMarkers(lng = ~lon, lat = ~lat, popup = popupTable(kangwon_table))

#직접 입력하기 
leaflet(kangwon_final) %>% setView(lng = 128.6652533, lat = 37.5730527, zoom = 8) %>% 
  addProviderTiles('Esri.WorldTopoMap')%>% 
  addMarkers(lng = ~lon, lat = ~lat, popup =  paste("<b>업소명</b> : ", kangwon_final$업소명, "<br>",
                                                    "<b>주소</b> : ", kangwon_final$소재지도로명주소, "<br>",
                                                    "<b>전화번호</b> : ", kangwon_final$전화번호, "<br>",
                                                    "<b>지정번호</b> : ", kangwon_final$지정번호, "<br>",
                                                    "<b>주요메뉴</b> : ", kangwon_final$주요메뉴))


