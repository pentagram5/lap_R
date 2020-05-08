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

##t 검정 -> 두 집단의 평균에 통계적으로 유의한 차이가 있는지 알아볼때 사용하는 통계 
mpg <- as.data.frame(ggplot2::mpg)
mpg_diff <- mpg %>%    select(class, cty) %>%    filter(class %in% c("compact", "suv")) 
#컴팩트와 suv 클래스의 도시연비 추출
head(mpg_diff) 
table(mpg_diff$class) 

t.test(data = mpg_diff, cty ~ class, var.equal = T)

# 일반 휘발유와 고급 휘발유의 도시 연비 t검정
mpg_diff2 <- mpg %>%    select(fl, cty) %>%    filter(fl %in% c("r", "p")) 
t.test(data = mpg_diff2, cty ~ fl, var.equal = T)


## 상관 분석 -두 연속 변수가 서로 관련이 있는지 검정하는 통계 분석 기법 
#상관 계수 -두 변수가 얼마나 관련되어 있는지, 관련성의 정도를 나타내는 값
#– 0~1 사이의 값을 지니고 1에 가까울수록 관련성이 크다는 의미

#실업자 수와 개인 소비 지출의 상관관계 
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

# 상관 행렬 히트맵 만들기 - 여러 변수간 상관계수를 행렬로 나타낸 표 
head(mtcars)

#상관 행렬 생성
car_cor <- cor(mtcars)
round(car_cor, 2)#소수째자리에서 반올림 후 출력

install.packages("corrplot")
library(corrplot)

corrplot(car_cor)
corrplot(car_cor, method = "number")

#다양한 파라미터 지정해보기 
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")) 


corrplot(car_cor,          method = "circle",       # 색깔로 표현
         col = col(200),         # 색상 200 개 선정
         type = "lower",         # 왼쪽 아래 행렬만 표시
         order = "hclust",       # 유사한 상관계수끼리 군집화
         addCoef.col = "black",  # 상관계수 색깔
         tl.col = "black",       # 변수명 색깔
         tl.srt = 45,            # 변수명 45 도 기울임
         diag = F)               # 대각 행렬 제외


####직접 데이터 분석해보기 -지역별 미세먼지 농도 비교 
#서울시의 구 중에서 성북구와 중구의 미세먼지 비교 및 지역별 차이 검정
dustdata <- read_excel("DATA/dustdata.xlsx")

View(dustdata)
str(dustdata)

#성북구와 중구 데이터만 추출 및 확인 
dustdata_anal <- dustdata %>% 
  filter(area %in% c("성북구","중구"))
view(dustdata_anal)


count(dustdata_anal, yyyymmdd) %>% arrange(desc(n))
count(dustdata_anal, area) %>% arrange(desc(n))


dust_anal_area_sb <- subset(dustdata_anal, area == "성북구")
dust_anal_area_jg <- subset(dustdata_anal, area == "중구")

dust_anal_area_sb
dust_anal_area_jg

#PSYCH 패키지 설치 및 로드
install.packages("psych")
library(psych)

#성북구의 미세먼지량에 대한 기초 통계량
describe(dust_anal_area_sb$finedust)
#  vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1    1 122 42.75 20.14   39.5   40.34 17.05   9 128   119 1.41     2.93 1.82


#중구의 미세먼지량에 대한 기초 통계량
describe(dust_anal_area_jg$finedust)
#vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1    1 122 35.91 17.44   33.5   34.12 15.57   8 112   104 1.33     2.88 1.58

#성북구와 중구의 미세먼지 농도에 대해 boxplot을 통한 분포 차이 확인
boxplot(dust_anal_area_sb$finedust, dust_anal_area_jg$finedust,
        main = "finedust_compare", xlab = "AREA", names = c("성북구","중구"),
        ylab = "FINEDUST_PM", col = c("blue","green"))
#귀무가설 : 성북구와 중구의 미세먼지 평균은 차이가 나지 않는다.
#대립가설 : 성북구와 중구의 미세먼지 평균은 차이가 난다.

#측정소명(area)에 따라 미세먼지 농도 평균에 대한 차이를 검증
t.test(data = dustdata_anal, finedust ~ area, var.equal = T)
# p-value 0.004975( 0.005보다 낮음.) 귀무가설을 기각하고 대립가설을 채택.
# 성북구와 중구의 미세먼지 평균은 차이가난다.-결론
#var.equal = T :두집단간의 분산이 동일하다는 가정



#실습 - 구로구와 용산구 미세먼지 비교
dust_anal <- dustdata %>% 
  filter(area %in% c("구로구","영등포구"))
dust_anal

count(dust_anal, yyyymmdd) %>% arrange(desc(n))
count(dust_anal, area) %>% arrange(desc(n))

dust_anal_area_gr <- subset(dust_anal, area == "구로구")
dust_anal_area_yu <- subset(dust_anal, area == "영등포구")

describe(dust_anal_area_gr$finedust)
# vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1    1 122 39.73 20.19     37   37.48 17.05  10 135   125  1.8     5.36 1.83

describe(dust_anal_area_yu$finedust)
#  vars   n  mean   sd median trimmed   mad min max range skew kurtosis   se
#X1    1 122 44.78 20.6     42   42.54 17.79  12 137   125 1.51      3.9 1.87


boxplot(dust_anal_area_gr$finedust, dust_anal_area_yu$finedust,
        main = "finedust_compare", xlab = "AREA", names = c("구로구","영등포구"),
        ylab = "FINEDUST_PM", col = c("blue","green"))
t.test(data = dust_anal, finedust ~ area, var.equal = T)
#p-value = 0.05436 귀무가설 채택. 결론 차이 없음

gg <- ggplot(dust_anal, aes(area, finedust)) +
  geom_boxplot(fill = c("green", "blue"))
plotly::ggplotly(gg)

#ggplotly 를 이용한 이용한 표현

##구별  여러변수 중 발생에 대한 상관관계 행렬로 표현 
seoul <- read.csv("seoul.csv", header = T, stringsAsFactors = F)
seoul
str(seoul)
seoul$절도.검거 <- as.numeric(seoul$절도.검거)
seoul$절도.발생 <- as.numeric(seoul$절도.발생)
seoul$폭력.발생 <- as.numeric(seoul$폭력.발생)

seoul_mat <- seoul %>%  select(살인.발생, 강도.발생, 강간.발생, 절도.발생, 폭력.발생)
seo_cor <- cor(seoul_mat)
seo_cor
corrplot(seo_cor, method = "number")

corrplot(seo_cor,          method = "circle",       # 색깔로 표현
         col = col(200),         # 색상 200 개 선정
         type = "lower",         # 왼쪽 아래 행렬만 표시
         order = "hclust",       # 유사한 상관계수끼리 군집화
         addCoef.col = "black",  # 상관계수 색깔
         tl.col = "black",       # 변수명 색깔
         tl.srt = 45,            # 변수명 45 도 기울임
         diag = F)  

###reshape 패키지 써보기

install.packages("reshape2")
library(reshape2)
#melt함수 - 가로의 데이터를 세로로 표현
head(airquality)
names(airquality) <- tolower(names(airquality))
aq_melt <- melt(airquality, id = c("month", "day"), na.rm = TRUE)
head(aq_melt)

#dcast 함수 - 세로 데이터를 가로로 표현 ->데이터 푸ㅡ레임 형태를 변환
aq_dcast <- dcast(aq_melt, month + day ~variable)
aq_dacst2 <- dcast(aq_melt, month~variable, mean)
head(aq_dcast)
aq_dacst2
View(aq_melt)
View(aq_dcast)

#acast 함수 ->벡터, 행렬, 배열 형태를 변환 
aq_acast <- acast(aq_melt, day~month~variable)
aq_acast2 <- acast(aq_melt, month~variable, mean)
aq_acast2
aq_acast

#실습해보기
fruits <- read.csv("DATA/fruits_10.csv", header = T)
fruits

melt(fruits, id = 'year')#year를 기준으로 다른 칼럼을 variable, 칼럼 데이터내용을 value로 가져옴 
melt(fruits, id = c('year', 'name'))
mtest<-melt(fruits, id = c('year', 'name'), variable.name = 'var_name', value.name = 'val_name')
#variable과 value에 별칭 달아주기 

dcast(mtest, year+name~var_name)
#원상복귀 

dcast(mtest, name~var_name, sum)
