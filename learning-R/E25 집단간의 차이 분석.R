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

one <- read.csv("Part-III/one_sample.csv")
one
summary(one)
view(one$survey)
x <- one$survey
summary(x)
length(x)
table(x)
library(prettyR)
freq(x)

# binom.test() 함수 - 명목척도(y/n) 대상 - 그래프는 좌우 대칭인 종 모양 곡선
help(binom.test)

#단일집단 비율검정
#만족율 기준 검정
#양측검정
binom.test(c(186,14), p=0.8)
binom.test(c(136,14),p=0.8,alternative="two.sided",conf.level=0.95)

#단측검정
binom.test(c(136,14), p=0.8, alternative="greater", conf.level=0.95)

#불만족율 20%이상을 기준으로 양측검정과 단측검정
#양측검정
binom.test(c(14,136), p=0.2)
binom.test(c(14,136),p=0.2, alternative="two.sided", conf.level=0.95)

#단측검정
binom.test(c(14,136),p=0.2, alternative = "greater" , conf.level=0.95)
#불만족율 20%보다 크지 않다
binom.test(c(14,136), p=0.2, alternative = "less", conf.level = 0.95)
#p-value = 0.0003179는 불만족율 20%보다 적다

hdtv <- read.csv("Part-III/hdtv.csv", header = T)
hdtv
buy <- hdtv$buy
buy
summary(buy)
table(buy)
binom.test(c(40,10), p=0.85)
binom.test(c(40,10), p=0.85, alternative = "two.sided", conf.level = 0.95)
binom.test(c(10,40), p= 0.15, alternative = "less", conf.level = 0.95)

#단일집단 평균검정
#1개 집단의 평균과 어떤 특정한 값과 차이가 있는지 검증.

#국내에서 생산된 노트북 평균 사용 시간이 5.2시간으로 파악된 상황에서 A회사에서 생산된 노트북 평균 사용시간과 차이가 있는지를 검정하기 위해서 A회사 노트북 150대를 랜덤으로 선정하여 검정을 실시.
#가정 : 기존 노트북 평균 사용시간 vs A회사 노트북 평균 사용시간

data <- read.csv("Part-III/one_sample.csv", header = T)
head(data)
x <- data$time   #노트북 사용시간
head(x)

summary(x)
mean(x)
mean(x, na.rm=T)    #NA 제외방법1
x1 <- na.omit(x)    #NA 제외방법2
x1
#정규분포 검정 - 평균에 대한 검정
shapiro.test(x1)    #p-value는 정규분포(유의수준 0.05보다 크면 정규분포로봄) 
t.test(x1)
wilcox.test(x1)
hist(x1)
#양측검정
t.test(x1, mu=5.2)   #mu=평균을말함.(그리스로마자), x1은 표본집단평균, mu는 모집단의평균
#정제데이터와 5.2시간 비교
t.test(x1, mu=5.2, alter="two.side", conf.level = 0.95)
#점추정, 구간추정
#단측검정
t.test(x1, mu=5.2, alter="greater", conf.level=0.95)    #A회사 노트북의 평균사용시간이 5.2시간보다 길다.
result <- t.test(x1, mu=5.2, alter="greater", conf.level = 0.95)
names(result)
str(result)

#it교육센터에서 PT를 이용한 프레젠테이션 교육방법과 실시간 코딩 교육방법을 적용하여 교육을실시.
#2가지 교육방법중 더 효과적인 교육 방법을 조사하기 위해서 교육생 300명을 대상으로 설문을 실시.
it <- read.csv("C:/grey/work_R/Data/Part-III/two_sample.csv", header = T)
it
head(it)
its <- table(it$method, it$survey)
its

x <- it$method
y <- it$survey
x;y

#데이터확인 - 교육방법 1과 2 모두 150명 참여
table(x)
#교육방법 만족 / 불만족
table(y)

#두 집단 비율 차이 검증 - 양측검정
prop.test(c(110,135),c(150,150))   #방법A만족도와 방법B만족도 차이검정
prop.test(c(110,135),c(150,150), alternative = "two.side", conf.level = 0.95)

#단측검정
prop.test(c(110,135),c(150,150), alter = "greater", conf.level = 0.95)
#방법A가 방법B에 비해 만족도가 낮은것으로 판단.

####집단간의 차이 분석 
#1) 단일 집단 비율 검정 - binom.test()
#2) 한집단 평균 검정 -shapiro.test()/ 정규 분포 -t.test()
# 비정규 분포 - wilcox.test()
#3)두 집단 비율 검정 - 두집단 subset 생성 ->prop.test()
#4)두 집단 평균 검정 -var.test / 동질성 분포 -t.test()
#  비동질성 분포 - wilcox.test() -웰콕스 검정 
#5)세 집단 평균 검정 - bartlett.test(바틀렛) / 동질성 분포 - aov()
#비동질성 분포 -kroskal.test()



#두 집단 평균 검정

data <- read.csv("Part-III/two_sample.csv", header = T)
print(data)
head(data)
summary(data)

#subset작성
result <- subset(data, !is.na(score), c(method, score))
#c(method, score) data의 전체변수중 두 변수만 추출
#!is.na NA가 아닌것만 추출
#위에서 정제된 데이터를 대상으로 subset 생성

#데이터 분리
#교육방법 별로 분리
a <- subset(result, method==1)
b <- subset(result, method==2)
#교육방법에서 점수 추출
a1 <- a$score
b1 <- b$score

#기술통계량 -> 평균값적용 -> 정규성 검정 필요
length(a1);
length(b1);

#분포모양 검정 : 두 집단의 분포모양 일치 여부 검정
#귀무가설 : 두 집단간 분포 모양이 동질적이다.
#두 집단간 동질성 비교(분포모양 분석)
var.test(a1,b1)    #p-value = 0.3002 -> 차이가없다.

#가설 검정 - 두 집단 평균 차이 검정
t.test(a1,b1)
t.test(a1,b1, alter = "two.sided", conf.int = T, conf.level = 0.95)
#p-value = 0.0411 - 두 집단간 평균에 차이가 있다.

t.test(a1,b1,alter = "greater", conf.int = T, conf.level = 0.95)
#p-value = 0.9794 : a1을 기준으로 비교 -> a1이 b1보다 크지 않다.

t.test(a1,b1,alter = "less", conf.int = T, conf.level = 0.95)
#p-value = 0.02055   : a1이 b1보다 작다.


#대응 두 집단 평균 검정
data <- read.csv("Part-III/paired_sample.csv", header = T)
data

#데이터 정제
result <- subset(data, !is.na(after), c(before,after))
result

#동일한 사람에게 두번 질문
x <- result$before
y <- result$after
x;y
length(x)
length(y)
mean(x)
mean(y)
var.test(x,y,paired=T)

#가설검정
t.test(x,y,paired=T)
t.test(x,y,paired=T, alter="greater",conf.int=T,conf.level=0.95)
t.test(x,y,paired=T, alter="less",conf.int = T, conf.level = 0.95)

#세집단 평균 검정
data <- read.csv("Part-III/three_sample.csv", header = T)
data <- subset(data, !is.na(score), c(method,score))

#차트이용 - ontline보기(데이터 분포 현황 분석)
plot(data$score)
barplot(data$score)
boxplot(data$score)
mean(data$score)

#outline제거 - 평균(14)이상 제거
length(data$score)
data2 <- subset(data,score <= 14)
length(data2$score)

#정제된 데이터 보기
x <- data2$score
boxplot(x)
plot(x)
bp <- boxplot(data2$score)

#세집단 subset작성
data2$method2[data2$method==1] <- "방법1"
data2$method2[data2$method==2] <- "방법2"
data2$method2[data2$method==3] <- "방법3"
table(data2$method2)

#교육방법에 따른 시험성적평균
x <- table(data2$method2)
head(data2)
y <- tapply(data2$score, data2$method2, mean)
out <- data.frame(교육방법=x, 시험성적=y)
out
#tapply(x, index, function) - x평균을 구할 변수, index그룹변수, function함수

#동질성 검정 - 정규성 검정
bartlett.test(score~method, data=data2)  #bartlett.test(종속변수~독립변수)

#분산검정
aov

data2$method2 <- factor(data2$method2)    #factor() : method가 집단 구성 변수 라는것을 명시

#aov(종속변수~독립변수, data=data set)
result <- aov(score~method2,data=data2)
names(result)
summary(result)    #aov()의 결과값은 summary()함수를 사용해야 p-value확인

#사후검정 - TukeyHSD
TukeyHSD(result)

plot(TukeyHSD(result))    #그래프보기(lwr~upr변수 이용)




####A사에서 개발 중인 다이어트 식품의 효과가 있는지 검정하세요.

#조건1) 파일-diet_effect
#조건2) 변수-before : 다이어트 식품 복용 전 몸무게
#after : 다이어트 식품 복용 후 몸무게

# 파일 부르기 
diet <- read.csv("Part-III/diet_effect.csv", header =  T)
head(diet)
 
#1) 복용전과 후의 참가자들의 빈도수와 몸무게 평균값을 구하고 분포를 
#그래프로 나타내 보세요.(그래프)
#-boxplot
boxplot(diet$before)
boxplot(diet$after)

boxplot(diet$before, diet$after)

#barplot
barplot(diet$before)
barplot(diet$after)

#평균
mean(diet$before)
#[1] 65.53992

mean(diet$after)
#[1] 90.76783
#-> 그래프와 평균치를 확인했을때, after 컬럼 데이터에 이상치가 존재함을 알 수 있다. 

#이상치 제거 
view(diet2) # 이상치 확인, after에 999.0이라는 값이 존재함으로 필터링 해준다
diet2 <- diet %>%  filter(diet$after != 999.0)

#결측치 제거 확인
barplot(diet2$after)
mean(diet2$before)
#[1] 65.61968

mean(diet2$after)
#[1] 57.94016


#   2) 2개 몸무게 집단 분포 모양이 비숫한지를 검정하세요.
boxplot(diet2$before, diet2$after)
#두집단의 평균을 검정해보기 위해  var.test 사용
var.test(diet2$before, diet2$after)
#F test to compare two variances

#data:  diet2$before and diet2$after
#F = 0.70567, num df = 248, denom df = 248, p-value = 0.006234
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.5498608 0.9056408
#sample estimates:
#  ratio of variances 
#0.7056744 

#test 결과, p_value 가 0.05이하이기 때문에 비동질성 분포도를 띈다. 따라서 
#데이터간의 관계파악을 위해 wilcox.test() -웰콕스 검정 을 실시한다. 




#3) 완수한 시험참가자들만의 데이터를 추출하여 복용 전과 복용 후에 효과 
#차이가 있는지 검정하세요.(중도 포기자-999)
wilcox.test(diet2$before, diet2$after)

#Wilcoxon rank sum test with continuity correction

#data:  diet2$before and diet2$after
#W = 43582, p-value = 4.673e-15
#alternative hypothesis: true location shift is not equal to 0

#결과에 따라 p-value = 4.673e-15 이기떄문에 복용전과 복용후의 효과차이는 존재한다. 