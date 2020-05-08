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
apart_cost

#####강서구아파트 정보 데이터 가공하기 ######
apart_cost <- read_excel("DATA/apartment.xlsx", skip = 15)
apart_cost <- apart_cost %>% data.frame() 
#엑셀파일에서 필요한 정보만 가져와 데이터프레임화


apart_cost
apart_cost <- rename(apart_cost, "전용면적"= "전용면적...")
apart_cost$전용면적 <- as.numeric(apart_cost$전용면적)
apart_cost$전용면적 <- round(apart_cost$전용면적)
#컬럼내용 통일 및 수치 변환 

apart_cost <- apart_cost %>% 
  filter(전용면적 == 85)
#전용면적이 85인 아파트로 필터링 

apart_cost <- rename(apart_cost, "거래금액" = "거래금액.만원.")
apart_cost$거래금액 <- gsub(",", "", apart_cost$거래금액)
apart_cost_mean <- aggregate(as.integer(거래금액)~단지명, apart_cost, mean)
apart_cost_mean <- rename(apart_cost_mean, "거래금액" = "as.integer(거래금액)")
#아파트 가격 평균 

apart_cost<- apart_cost[!duplicated(apart_cost$단지명),]
#중복 제거 

dim(apart_cost)
apart_cost <- left_join(apart_cost, apart_cost_mean, by ="단지명")
#단지명으로 조인 

apart_cost
apart_cost <- rename(apart_cost, "거래금액" = "거래금액.y")
#거래금액.y 칼럼 rename

apart_cost_add <- paste(apart_cost$"시군구", apart_cost$"번지") %>% data.frame()
dim(apart_cost_add)
apart_cost_add <- rename(apart_cost_add, "주소" = ".")
#번지수 합치기 

apart_cost_add_code <- as.character(apart_cost_add$"주소") %>%  enc2utf8() %>%  geocode()
#주소 lon, lat 구하기 

apart_code_final <- cbind(apart_cost,  apart_cost_add,apart_cost_add_code ) %>% 
  select("단지명", "전용면적", "거래금액", "주소", lon, lat)
#최종 테이블 cbind





##### 5호선 지하철 역 가져오기 #####

sub <-read_xls("지하철(5호선_8호선) 역별 주소 및 우편번호(서울교통공사).xls", skip =2) 
sub <- sub %>% data.frame() 
sub <- sub %>% filter(sub$호선 == '5')
#지하철역 데이터를 가져와 5호선만 출력


sub_code <- as.character(sub$"지번주소") %>%  enc2utf8() %>%  geocode()
sub_final <- cbind(sub, sub_code)
sub_final
#geocode로 변환후 cbind

subwayIcon <- makeIcon(
  iconUrl = "https://cdn4.iconfinder.com/data/icons/lucid-transportation/24/Metro_Rail_train_subway_transit-512.png",
  iconWidth = 40, iconHeight = 40
)
#5호선 표시 아이콘 저장 

#####구글맵에 표시!!#####

leaflet(apart_code_final) %>% 
  setView(lng = 126.855853, lat = 37.5429046, zoom =13)%>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addMarkers(lng = sub_final$lon, lat = sub_final$lat,  
             label = sub_final$역명, icon =subwayIcon ) %>% 
  addTiles() %>% addMarkers(
               clusterOptions = markerClusterOptions(),
               popup =  paste("<b>주소:</b> : ", apart_code_final$주소, "<br>",
                              "<b>단지명</b> : ", apart_code_final$단지명, "<br>",
                              "<b>전용면적</b> : ", apart_code_final$전용면적, "m2<br>",
                              "<b>거래금액</b> : ", apart_code_final$거래금액, "만원<br>" ))

