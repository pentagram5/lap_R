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

googleAPIkey<-"AIzaSyD548iSahGuxgvGqfg8P0ytIVF6ksn4NMw"  
register_google(googleAPIkey)

#####R내장함수로 데이터 추출하기#####
exam <-read.csv("DATA/csv_exam.csv")
exam 

##행번호로 행 추출하기
exam[] #조건없이 전체 데이터 출력 

exam[1,] #1행만 추출하기 
exam[2,] #2행만 추출하기 

exam[exam$class == 1, ]#class가 1인 행 추출출

exam %>% filter(class == 1)

exam[exam$math >= 80, ]#수학점수가 80 이상인  행 추출출

exam %>% filter(math >= 80)

#end 나 or 연산자도 표현가능하다 
exam[exam$class == 1 & exam$math >= 50,]

exam[exam$english < 90 | exam$science < 50,]



##열만 추출하기 

exam[,1]
exam[,2]
exam[,3]
exam[,4]
exam[,5]
exam

exam[,"class"]

exam[,c("class", "math", "science")]


##행, 변수 동시에 추출하기 

exam[4,"english"]

exam[exam$id == 1, c("id", "class", "math", "science")]

#행 부등호 조건, 열 변수명 
exam[exam$math >= 50, c("english","science")]


#####dplyr과 내장함수의 차이 #####

#수학점수 50점 이상, 영어 점수 80 점 이상인 학생들의 대상으로 전과목 총평균을 구하라
#내장함수
exam$tot <- (exam$math + exam$english+ exam$science) /3 
aggregate(data = exam[exam$math >= 50 & exam$english >=80,], tot~class, mean)
            #aggregate(데이터셋, 계산열~기준열, 연산함수) 계산열 = 연산함수를 이용해 계산될 열 값,
                                                          #기준열 = group_by의 기준이 될 열


#dplyr
exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  mutate( tot  = (math + science+english)/3) %>% 
  group_by(class) %>% 
  summarise(mean = mean(tot))

#####혼자서 해보기-mpg 테이블을 이용해 suv, compact의 통합연비 구해보기#####
mpg <- as.data.frame(ggplot2::mpg)
mpg

#내장함수
mpg$toty <- (mpg$cty + mpg$hwy)/2
aggregate(data = mpg[mpg$class == 'suv' | mpg$class == 'compact',], toty~class, mean)

#dplyr
mpg %>% 
  filter(class == 'suv' | class == 'compact') %>% 
  group_by(class) %>% 
  summarise(mean = mean(toty))



#####변수 타입#####

#연속변수 - Numaric타입
#값이 연속적이고 크기를 의미, 더하기 빼기, 평균 구하기 등 산술가능 ex)키, 몸무게 소독

#범주변수 -Factor 타입
#값이 대상을 분류하는 의미를 지녀 산술연산 불가능 

#변수 타입간 차이 알아보기 
var <- c(1,2,3,1,2)
var2 <- factor(c(1,2,3,1,2))
var2 #값의 범주를 뜻하는 levels 가 출력됨 
var
var+2
var2+2 #factor타입은 산술연산 불가능 
class(var)
class(var2) #변수타입 확인하기 


levels(var2) #factor 타입의 변수의 구성범주 확인하기 


#문자로 구성된 factor 변수\

var3 <- c("a", "b","b","c")
var4 <- factor(c("a","b","b","c"))
var3
var4

#함수마다 적용가능한 변수타입이 다름 
mean(var1)
mean(var2)#에러 출력

#변수 타입 바꾸기
var2 <- as.numeric(var2)#factor 타입 numeric으로 바꾸기 
mean(var2)
class(var2)
var1 <-as.factor(var1)#numeric 타입 factor으로 바꾸기  
var1


#혼자서 해보기 

class(mpg$drv)
factor(mpg$drv)

mpg$drv = as.factor(mpg$drv)
mpg$drv





#####데이터 구조#####
#벡터 -> 1차원, 한가지 변수 타입 
#데이터 프레임 -> 2차원, 다양한 변수
#매트릭스-> 2차원, 한가지 변수 
#어레이 -> 다차원, 2차원이상 매트릭스
#리스트 -> 다차원, 서로다른 데이터 구조 포함 

#벡터 ->하나 또는 여러개의 값으로 구성된 데이터 구조, 한가지 타입 
a<-1 
a
b<-"hello"
class(b)

#데이터 프레임 -> 2차원, 다양한 변수

x1 <- data.frame(var1 = c(1,2,3,4),
                 var2 = c( 10,2,1,3))
x1

#매트릑스 ->2차원에 한가지 변수
x2 <- matrix(c(1:12), ncol = 2)
x2

class(x2)

x <- c(1,2,3,4,5,6)
matrix(x, nrow = 2, ncol = 3)
matrix(x, nrow = 3, ncol = 2)
matrix(x, nrow = 3, ncol = 2, byrow =T)



#어레이 -> 2차원 이상으로 구성된 매트릭스, 한가지 타입만 가능
x3 <- array(1:20, dim = c(2,5,2))#1부터 20까지 2행, 5열로 구성된 2개의 매트릭스를 생성하라 
x3



#리스트 생성하기 -> 모든 데이터 구조를 포함하는 데이터구조

x_list <- list( f1 = a,
                f2 = x1,
                f3 = x2,
                f4 = x3)
x_list



#boxplot() 출력 결과물에서 값 추출하기 
mpg <- ggplot2::mpg
x <- boxplot(mpg$cty)
x$stats[,1]
x$stats[,1][3]
x$stats[,1][2]




#####실전-->테이블이나 csv, 엑셀 파일불러오는 환경설정 공부하기#####

fruit <- read.table("DATA/fruits.txt", header = T)
View(fruit)
fruit

fruit2 <- read.table("DATA/fruits.txt", skip = 3)
fruit2
fruit3 <- read.table("DATA/fruits.txt", skip = 1, nrow= 2)
fruit3
fruit4 <- read.table("DATA/fruits.txt", skip = 2, nrow = 2)
fruit4
fruit5 <- read.csv("DATA/fruits_4.csv", header = F)
fruit5
fruit6 <- read.csv("DATA/fruits_4.csv", header = F, col.names = c('NO','NAME','PRICE', 'QTY'))
