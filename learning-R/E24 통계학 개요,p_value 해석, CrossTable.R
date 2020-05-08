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



#####통계학 개요#####
setwd("C:/Rstudy/Work_R")

data <- read.csv("DATA/Part-III/descriptive.csv", header =  T)
data

#데이터 특성보기 
dim(data)
length(data)
summary(data)

summary(data$gender)
table(data$gender)

data <- subset(data, data$gender ==1 | data$gender == 2)
barplot(x)
prop.table(x)

round(y*100,2)

table(data$level)
x1 <- table(data$level)
barplot(x1)

survey <- data$survey
survey
summary(survey)
x1<-table(survey)
x1
hist(survey)


mean(data$cost)

#비율척도 변수의 기술통계량 
plot(data$cost)
summary(data$cost)
mean(data$cost)

#데이터 정제, 결측치 제거 및 outline 제거 
data <- subset(data, data$cost >= 2 & data$cost <= 10)
plot(data$cost)
mean(data$cost)
x <- data$cost

#표준편차 구하기 
#표준편차 : 표본의 평균에서 얼마나 떨어져 있는가 - 산포도 
sort(x)

sd(x) #표준편차 
var(x) #분산 

quantile(x, 1/4) #1 사분위수 
quantile(x, 3/4) #3 사분위수 

#패키지를 이용한 비대칭도 나타내기 
#왜도/첨도 사용을 위한 패키지 설치 
install.packages("moments")
library(moments)
cost <- data$cost

#왜도 - 평균 중심으로 기울어짐 정도
skewness(cost)

#첨도 - 표준 정규분포와 비교하여 얼마나 뽀족한가 측정 지표
kurtosis(cost)

#표준 정규분포와 비교하여 첨도가 3이며, 정규분포 곡선을 이루고,
#첨도가 3보다 크면 정규분포 보다 뾰족한 형태, 3보다 작으면
#정규 분포보다 완만한 형태이다 
hist(cost)


#패키지 이용 기술 통계량 구하기 
install.packages("Hmisc")
library(Hmisc)

#Hmisc 패키지에서 제공되는 함수 
describe(data)
describe(x)


#개별 변수 기술 통계량 
describe(data$gender)
describe(data$age)


#히스토그램 확률밀도/표준정규분포곡선
hist(cost, freq = F)

#확률 밀도 분포 곡선
lines(density(cost), col = 'blue')

#표준정규분포 곡선
x <- seq(0,8,0.1)
curve(dnorm(x, mean(cost), sd(cost)), col = 'red', add= T)

#x구간에 속한 값들에 정규분포 만들기 : dnorm(x속성값, mean, sd)


#prettyR 패키지 이용 
install.packages("prettyR")
library(prettyR)
freq(data)
freq(data$gender)

#거주지역 변수 리코딩
data$resident2[data$resident == 1] <- "특별시"
data$resident2[data$resident >=2 & data$resident <= 4] <- "광역시"
data$resident2[data$resident == 5] <- "시구군"

x <- table(data$resident2)
prop.table(x)
y<-prop.table(x)

round(y*100, 2) #백분율 적용(소수점 2자리)

#성별 변수 리코딩
data$gender2[data$gender == 1] <-"남자"
data$gender2[data$gender == 2] <-"여자"
x<-table(data$gender2)
prop.table(x)
y <- prop.table(x)
round(y*100, 2)


#나이변수 리코딩
data$age2[data$age <= 45] <-"중년층"
data$age2[data$age >=46 & data$age <= 59] <-"장년층"
data$age2[data$age >= 60] <-"노년층"

x<- table(data$age2)
prop.table(x)

y <- prop.table(x)
round(y*100, 2)

#레벨, 학력 변수 리코딩
data$level2[data$level == 1] <-"고졸"
data$level2[data$level == 2] <-"대졸"
data$level2[data$level == 3] <- "대학원졸"
x <- table(data$level2)
prop.table(x)
y <- prop.table(x)
round(y *100, 2)

#합격 여부 변수 리코딩
data$pass2[data$pass == 1] <- "합격"
data$pass2[data$pass == 2] <- "실패"
x<- table(data$pass2)
prop.table(x)
y <- prop.table(x)
round(y*100, 2)
head(data)


#MASS 패키지의 Animals 데이터 셋을 이용해 각 단계에 맞게 기술 통계량을 구하라 
install.packages("MASS")
library(MASS)
Animals
#기술 통계량
plot(Animals$brain)
x <- Animals$brain
barplot(x)
hist(x)

#평균 
mean(x)

#중위수 
median(x)

#표준편차 
sd(x) 

#분산
var(x)

#최대값
max(x)
#최소값
min(x)

describe(x)
freq(x)

##명목척도 변수인 학교유형, 합격 여부 변수에 대해 빈도 분석 수행 및 결과를 시각화 
#학교 유형 시각화

desc<-read.csv("DATA/Part-III/descriptive.csv")
desc_ty <- desc$type
desc_pa <- desc$pass
barplot(desc_ty)
hist(desc_ty)
median(desc_ty)
describe(desc_ty)
ty_table<- table(desc_ty)
prop.table(ty_table)
barplot(ty_table)
pie(ty_table)
ty_label <- round(ty_table/sum(ty_table) *100, 2)
ty_label
ty_label <- paste(ty_label,"%")
pie3D(ty_table, main = "data$type chart", col = rainbow(length(p1)),
      cex = 0.5, labels = ty_label, explode = 0.03)
legend(0.6, 0.7, c("type 1", "type 2"),
       cex = 0.7, fill = rainbow(length(p1)))


#pass유형 시각화 
barplot(desc_pa)
hist(desc_pa)
median(desc_pa)
describe(desc_pa)
pa_table <- table(desc_pa)
prop.table(pa_table)
barplot(pa_table)
pie(pa_table)

pa_label <- round(pa_table/sum(pa_table) *100, 1)
pa_label
pa_label <- paste(pa_label,"%")
pie3D(ty_table, main = "data$pass chart", col = rainbow(length(p1)),
      cex = 0.5, labels = pa_label, explode = 0.03)
legend(0.6, 0.7, c("pass", "fail"),
       cex = 0.7, fill = rainbow(length(p1)))

#gmodels 패키지를 활용한 테이블 - 교차표 작성
install.packages("gmodels")
library(gmodels)

x<-data$level2
y<-data$pass2

CrossTable(x,y)


#chi-squre
chisq.test(c(4,6,17,16,8,9))
#p값 해석 방법 - 일원 카이제곱 검정 
data <- textConnection(
  "맥주종류 관측도수 
 1  12
 2  30
 3  15
 4  7
 5  16")
 x <- read.table(data, header = T)
chisq.test(x$관측도수)
#Chi-squared test for given probabilities

#data:  x$관측도수
#X-squared = 18.375, df = 4, p-value = 0.001042


#p값이 0.05 미만이기 때문에 유의미한 수준에서 귀무가설을 기각할 수 있다.
#따라서 맥주 선호도에 차이가 있다라는 대립가설을 채택할 수 있다.
#귀무가설 : 기대치와 측정치의 차이가 없다.
#대립가설 : 기대치와 측정치의 차이가 있다. 

#이원카이 제곱 
CrossTable(x, y, chisq = TRUE)#p값이 0.05이상이기때문에 귀무가설을 채택할 수 있다. 



#####실습해보기#####

###교육수준과 흡련율 간의 관령성 분석

smoke <- read.csv("DATA/Part-III/smoke.csv", header = T)
smoke

names(smoke)


smoke$education2[smoke$education == 1]<-"대졸"
smoke$education2[smoke$education == 2]<-"고졸"
smoke$education2[smoke$education == 3]<-"중졸"

smoke$smoking2[smoke$smoking == 1] <-"과대흡연"
smoke$smoking2[smoke$smoking == 2] <-"보통흡연"
smoke$smoking2[smoke$smoking == 3] <-"비흡연"

CrossTable(smoke$education2, smoke$smoking2, chisq = T)

#Pearson's Chi-squared test 
#------------------------------------------------------------
#  Chi^2 =  18.91092     d.f. =  4     p =  0.0008182573 
# 연구가설을 채택할 수 있다.



### 교육센터의 교육방법에 따라 교육생들의 만족도에 차이가 있는지 검정
# 귀무가설 -> 교육방법에 따라 만족도에 차이가 없다.
# 연구가설 -> 교육방법에 따라 만족도에 차이가 있다.

study <- read.csv("DATA/Part-III/homogenity.csv", header = T)
str(study)

#코딩 변경
study$method2[study$method == 1]<- "대졸"
study$method2[study$method == 2]<- "고졸"
study$method2[study$method == 3]<- "중졸"

study$survey2[study$survey == 1] <- "매우만족"
study$survey2[study$survey == 2] <- "만족"
study$survey2[study$survey == 3] <- "보통"
study$survey2[study$survey == 4] <- "불만족"
study$survey2[study$survey == 5] <- "매우불만족"

#교차  분할표 작성
CrossTable(study$method2, study$survey2, chisq = T)

#Pearson's Chi-squared test 
#------------------------------------------------------------
#Chi^2 =  6.544668     d.f. =  8     p =  0.5864574 
#p값이 0.05 이상이므로, 귀무가설을 채택할 수 있다. 


###직업 유형에 따른 응답 정도에 차이가 있는가를 단계별로 검정하시오.
# 귀무가설 -> 직업유형에 따라 응답정도에 차이가 없다.
# 연구가설 -> 직업유형에 따라 응답정도에 차이가 있다.
#파일 가져오가 
response <- read.csv("DATA/Part-III/response.csv", header = T)
str(response)

#코딩변경
response$job2[response$job == 1]<-"학생"
response$job2[response$job == 2]<-"직장인"
response$job2[response$job == 3]<-"주부"

response$response2[response$response == 1]<-"무응답"
response$response2[response$response == 2]<-"낮음"
response$response2[response$response == 3]<-"높음"
t.test(response)

CrossTable(response$job2, response$response2, chisq = T)
#Statistics for All Table Factors


#Pearson's Chi-squared test 
#------------------------------------------------------------
#Chi^2 =  58.2081     d.f. =  4     p =  6.900771e-12 
#p-value가 0.05이하로, 연구가설을 채택할 수 있다. 
#따라서 직업 유형에 따라 응답정도에 차이가 존재한다. 


repon_table<-data.frame(table(response$job2, response$response2))       


gg <-ggplot(response, aes(x= job2, fill = response2))+geom_bar(position = "dodge")
plotly::ggplotly(gg)


ggplot(repon_table, aes(Var1, Var2)) +geom_point(aes(size = Freq, color = Var2))+geom_text(aes(label=Freq)) +theme_bw()+ xlab("직업 유형")+ylab("응답정도")+scale_size_continuous(range = c(5, 35))


plotly::ggplotly(gg2) 

