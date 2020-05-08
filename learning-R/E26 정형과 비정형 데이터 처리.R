#####library#####
library(foreign)
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
library(rJava)
library(RJDBC)
library(rJava)
library(RJDBC)
library(party)
library(datasets)

googleAPIkey<-"AIzaSyD548iSahGuxgvGqfg8P0ytIVF6ksn4NMw"  
register_google(googleAPIkey)

setwd("C:/Rstudy/work_R")


#####정형과 비정형 데이터처리 #####

###정형 데이터 (RDB-Oracle)
##오라클 DB와 연동하기 

#패키지 설치 
install.packages("rJava")
install.packages("RJDBC")

#DB연동 정보 저장 
id <-'hr'
pwd <- 'hr'
url <- 'jdbc:oracle:thin:@localhost:1521:XE'

#DB 연동
drv <- JDBC("oracle.jdbc.driver.OracleDriver", "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")

#CONNECTION 객체 생성
conn <- dbConnect(drv, url, id, pwd)

#쿼리입력 
query <- dbGetQuery(conn, "SELECT * from test")
#출력
query

#문제풀어보기 
query <- dbGetQuery(conn, "SELECT PROFNO, NAME, D.DNAME, P.DEPTNO
                           FROM PROFESSOR P, DEPARTMENT D 
                           WHERE P.DEPTNO = D.DEPTNO")
query


#다른 유횽으로 사용해보기
sqlstr <- 'SELECT MONTHS_BETWEEN(sysdate, hiredate) AS DAY, SAL FROM EMP'
emp <- dbGetQuery(conn, sqlstr)

p <- ggplot(emp, aes(x = DAY, y = SAL)) + geom_line()
ggplotly(p)



####분류 분석 
###분류 분석? - 다수의 속성 또는 변수를 갖는 객체를 사전에 정해진 그룹또는 범주 중의 하나로 분류해 분석하는 방법

##분류분석 특징
#종속 변수 존재, 종속변수(예측에 FOCUS를 두는 변수), 규칙을 기반으로 의사결정트리 생성, 비모수 검정
#단점 : 유의 수준 판단 기준 없음 

##party 패키지 적용 분류분석해보기
install.packages("party")
library(party)#ctree() 제공
library(datasets)

str(airquality)
formula <- temp ~ solar.r +wind + ozone
#temp = 종속변수, 

#formula 를 이용해 분류모델 생성
air_ctree <- ctree(formula, data = airquality)
air_ctree

plot(air_ctree)

#분류 조건 subset 작성/ 확인 
result <- subset(airquality, ozone <= 37 & wind >15.5)
summary(result$temp)



####iris 데이터로 데이터 샘플링 해보기 
##학습데이터와 검증데이터 샘플링
result <- sample(2, nrow(iris), replace = T, prob = c(0.7,0.3))
table(result)

train <- iris[result == 1,]
test <- iris[result == 2,]
train
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
#Species -> 종속 변수 

#분류모델 예측 -> 트리형태로 
iris_ctree <- ctree(formula, data = train)
plot(iris_ctree)

#분류모델 예측 -> 테이블 형태로
table(predict(iris_ctree), train$Species)

#분류 모댈 풀로팅
plot(iris_ctree, type = "simple")

####고속도로 주행 거리에 미치는 영향변수 
str(mpg)
mpg_re <- sample(2, nrow(mpg), replace = T, prob = c(0.7,0.3))
table(mpg_re)
set.seed(1234)

train <- mpg[mpg_re ==1,]
test <- mpg[mpg_re ==2,]
train

formula <- hwy ~ displ+cyl+year 
mpg_ctree <- ctree(formula, data = train)
mpg_ctree2 <- ctree(formula, data = test)

plot(mpg_ctree)
plot(mpg_ctree2)

plot(mpg_ctree, type = "simple")
plot(mpg_ctree2, type = "simple")


