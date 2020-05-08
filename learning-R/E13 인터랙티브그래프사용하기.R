##### 인터렉티브 그래프 사용해보기#####
#->마우스 움직임에 반응하면서 실시간으로 형태가 변하는 그래프 
install.packages("plotly")
library(plotly)
library(ggplot2)
p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) + geom_point()
ggplotly(p)

#diamonds 데이터를 이용해 막대그래프 만들기 

p<-ggplot(data = diamonds, aes( x= cut, fill = clarity)) + geom_bar(position = "dodge")
p
setwd("C:/Rstudy/work_R")

#과목별 데이터를 이용해 그래프 그리기 
score <-read.csv("DATA/학생별과목별성적_국영수_new.csv")

score

score_p<-ggplot(data = score, aes( x= 이름, y = 점수, fill = 과목)) + geom_col() +
  ylim(0,300) #x와 y축에 상관되는 데이터를 막대 그래프로 표현시 geom_col, 그외는 geom_bar 

ggplotly(score_p)


#####dygraphs 패키지로 인터랙티브 시계열 그래프 만들기
install.packages("dygraphs")
library(dygraphs)
library(xts)#시간 순서 속성을 지니는 xts 데이터 타입
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)
dygraph(eco) %>% dyRangeSelector()#기간 지정하기 


#여러 값 표현하기 
#저축률
eco_a <- xts(economics$psavert, order.by = economics$date)

#실업자수 
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)

eco2 <- cbind(eco_a, eco_b)

colnames(eco2) <- c("저축률", "실업자수")
head(eco2)

dygraph(eco2) %>% dyRangeSelector()

##### 강남구 커피숍 구글맵통해서 표시하기#####

coff <- read_xlsx("DATA/gangnam_coffee.xlsx")
coff
addr_coff<- substr(coff$소재지전체주소, 11, 13)
addr_coff

addr_coffcount <- addr_coff %>%  table() %>% data.frame()
addr_coffcount

#주소를 입력받아 위도와 경도로 표기 
geo_code <- as.character(coff$소재지전체주소) %>%  geocode()
addr_cofffinal <- cbind(addr_coffcount, geo_code)
addr_cofffinal
detail <- cbind(coff, geo_code)

#동이름과 갯수 표현하기 
leaflet(addr_cofffinal) %>% setView(lng = 127.0570626, lat = 37.5026644, zoom =13)%>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircles(lng = ~lon, lat = ~lat, dashArray = 'lightcoral', color = 'lightcoral', 
             radius =  addr_cofffinal$Freq *2) %>% 
  addMarkers(lng = ~lon, lat = ~lat, popup =  paste("<b>동이름:</b> : ", addr_cofffinal$., "<br>",
                                                  "<b>커피숍 개수</b> : ", addr_cofffinal$Freq)) %>% 
  addCircleMarkers(lng = detail$lon, lat = detail$lat, radius = 0.1, opacity = 0.2, 
                   popup = paste("<b>커피숍이름</b> : ", detail$사업장명))  



#동이름과 갯수 및 커피숍 이름 표현하기 
leaflet(addr_cofffinal) %>% setView(lng = 127.0570626, lat = 37.5026644, zoom =13)%>% 
  addProviderTiles('Esri.WorldTopoMap') %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, color = 'lightcoral', fillColor = 'black', 
             fillOpacity = 10, weight = addr_cofffinal$Freq/3, 
             popup =  paste("<b>동이름:</b> : ", addr_cofffinal$., "<br>",
                            "<b>커피숍 개수</b> : ", addr_cofffinal$Freq)) 

             
             addMarkers(lng = ~lon, lat = ~lat, popup =  paste("<b>동이름:</b> : ", addr_cofffinal$., "<br>",
                                                    "<b>커피숍 개수</b> : ", addr_cofffinal$Freq)