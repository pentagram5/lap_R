library(foreign)#foreign 패키지 설치 (spss, sas, stata 등 통계분석 s/w)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)


#단계 구분도 패키지 설치 
install.packages("ggiraphExtra")
library(ggiraphExtra)

#####미국 주별 강력 범죄율 단계 구분도 만들기#####

str(USArrests)
head(USArrests)

library(tibble)

#USArrests 데이터는 지역명 변수가 따로없고 행이름이 지역명으로 되어 있으므료 tibble 패키지를 이용,
# 행이름을 state 변수로 바꿔 데이터 프레임 생성
crime <- rownames_to_column(USArrests, var = "state")
crime

#지도 데이터의 지역명 변수는 ㄴ모든값이 소문자이기떄문에 state값을 소문자로 수정

crime$state <- tolower(crime$state) #tolower-> 특정 열의 데이터를 소문자로 수정
str(crime)

#미국 주 지도 데이터 준비하기 
# 단계구분도 -> 위도, 경도 정보가 있는 지도 데이터가 필요, 미국 주별 위경도는 maps 패키지 state데이터에 들어있다.

library(ggplot2)
install.packages("maps")
library(maps)
install.packages("ggiraphExtra")
install.packages("maps")
install.packages("mapproj")
library(mapproj)
states_map <- map_data("state")
str(states_map)

#단계 구분도 만들기
ggChoropleth(data = crime,
             aes (fill = Murder,
                  map_id = state),
             map = states_map,
             interactive = T)


#####대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기#####

#시도별 인구 단계 구분도 만들기#
install.packages("stringi")
library(stringi)
install.packages("devtools")
library(devtools)
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)

#대한민국 시도별 인구데이터 준비하기

korpop1$name <- iconv(korpop1$name, "UTF-8", "CP949")#뷰어 한글화
tbc$name <- iconv(tbc$name, "UTF-8", "CP949")#뷰어 한글화
#korpop1 -> 시도별 인구통계 정보
str(changeCode(korpop1))
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)

str(changeCode(kormap1))

#단계 구분도 만들기
ggChoropleth(data = korpop1,       # 지도에 표현할 데이터
             aes(fill = pop,       #색깔로 표현할 변수
                 map_id = code,    #지역 기준 변수
                 tooltip = name),  #지도 위에 표시할 지역명
             map = kormap1,        #지도 데이터 
             interactive = T)      #인터렉티브



#대한민국 결핵 환자 시도별 인구 데이터 
str(tbc)
ggChoropleth(data = tbc,            # 지도에 표현할 데이터
             aes(fill = NewPts,     #색깔로 표현할 변수
                 map_id = code,     #지역 기준 변수
                 tooltip = name),   #지도 위에 표시할 지역명
             map = kormap1,         #지도 데이터 
             interactive = T)       #인터렉티브


#####구글맵 실습 #####
install.packages("ggmap")

install_github("dkahle/ggmap")
library(ggmap)
googleAPIkey<-"AIzaSyD548iSahGuxgvGqfg8P0ytIVF6ksn4NMw"  
register_google(googleAPIkey)##주요함수-> 구글 APIkey 등록

#서울 지도 가져오기 
gg_seoul <- get_googlemap("seoul", zoom =6, maptype = "roadmap")#get_googlemap, 설정한 구글맵을 가져옴
ggmap(gg_seoul)# 실제 지도를 그리기 
gg_seoul
library(stringr)

#암사동 공영주차장 표시하기 
loc<-read.csv("DATA/서울_강동구_공영주차장_위경도.csv")
kd <- get_map("Amsa-dong", zoom = 13, maptype = "roadmap")
kor.map <- ggmap(kd)+geom_point(data=loc, aes(x=LON, y=LAT),
                                size = 3, alpha=0.7, color = "red")
kor.map + geom_text(data=loc, aes( x=LON, y =LAT+0.001, label = 주차장명), size = 3)
ggsave("C:/Rstudy/work_R/r_temp/kd_공영주차장위경도.png", dpi=500)

#주차장명의 (구)는 구립, (시)는 시립 공영주차장, 구립과 시립을 red/blue로 분류

loc2 <- str_sub(loc$주차장명, start=-2, end = -2)
loc2
colors <-c()
for( i in 1:length(loc2)){
  if(loc2[i] =='구'){colors <- c(colors,"red")}
  else { colors <- c(colors,"blue")}
}

kd <-get_map("Seoul", zoom = 13, maptype ="roadmap")
kor.map <- ggmap(kd)+geom_point(data=loc, aes(x=LON, y = LAT),
                                size = 3, alpha=0.7, color = colors)
kor.map + geom_text(data=loc, aes( x=LON, y =LAT+0.001, label = 주차장명), size = 2.6)
ggsave("C:/Rstudy/work_R/r_temp/kd_공영주차장위경도_파랑,빨강.png", dpi=500)

#서울 지역별 장애인 도서관 위치 표시하기
loc3
loc3 <- read.csv("DATA/지역별장애인도서관정보.csv")
kd3 <- get_map("seoul", zoom = 3, maptype = "roadmap")
kor.map <- ggmap(kd3)+geom_point(data = loc3, aes(x=LON, y = LAT),
                                 size = 3, alpha=0.7, color = "red")
kor.map + geom_text(data=loc3, aes( x=LON, y =LAT+0.001, label = 도서관명), size = 2.6)
ggsave("C:/Rstudy/work_R/20200414 study/r_temp/kd_공영주차장위경도_파랑,빨강.png", dpi=500)


#분석 실전 예제- 지역별 인구현황 표시
library(grid)
pop <- read.csv("DATA/지역별인구현황_2014_4월기준.csv", header = T)
pop
lon <- pop$LON
lat <- pop$LAT
data <- pop$총인구수

df <- data.frame(lon,lat,data)
map1 <- get_map("Jeonju", zoom=7, maptype = 'hybrid')
map1 <- ggmap(map1)
map1 + geom_point(aes(x=lon, y= lat, colour = data, size = data), data = df)
ggsave("C:/Rstudy/work_R/20200414 study/r_temp/지역별인구현황_2014.png", dpi=1000)


#두가지 정보 한번에 표현하기
locc1 <- read.csv("DATA/지역별장애인도서관정보.csv", header=T)
locc1
locc2 <- read.csv("DATA/서울시장난감도서관위치현황.csv", header = T)
mapp1 <- get_map("seoul", zoom = 11, maptype = "roadmap")
kor.map <- ggmap(mapp1)+ geom_point(data = locc1, aes(x = LON, y = LAT), color = "red",size=4, alpha= 0.7) +
  geom_point(data = locc2, aes(x = LON, y = LAT), color = "blue",size=4, alpha= 1)
kor.map + geom_text(data = locc1, aes(x =LON, y = LAT+0.01, label = 자치구명), size =3)
ggsave("C:/Rstudy/work_R/20200414 study/r_temp/두가지정보한번에표현하기.png", dpi=500)


dd <- read_xlsx("dd.xlsx")

View(dd)
dim(dd)
ddlon <- dd %>% 
  filter(dd$업태구분명 %in% c("호프(통닭)", "통닭(치킨)"))
View(ddlon)


#서울 지하철 2,3호선 표현하기 
loc <- read.csv("DATA/서울지하철2호선위경도정보.csv", header=T)
loc2 <- read.csv("DATA/서울지하철3호선역위경도정보.csv", header=T)
loc

kor.map <- ggmap(mapp1)+ geom_point(data = loc, aes(x = LON, y = LAT), color = "forestgreen",size=2, alpha= 1) +
  geom_point(data = loc2, aes(x = LON, y = LAT), color = "orangered",size=2, alpha= 1)
kor.map + geom_text(data = loc, aes(x =LON, y = LAT+0.01, label = 역명), size =1.5) +
   geom_text(data = loc2, aes(x =LON, y = LAT+0.01, label = 역명), size =2)
ggsave("C:/Rstudy/work_R/20200414 study/r_temp/2,3호선표현하기.png", dpi=1500)

color()
