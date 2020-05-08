##### 그래픽 기초#####
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
var1 <-c(1,2,3,4,5)
plot(var1)

var2 <- c(2,2,2)
plot(var2)

x <-1:3 # x-> 1,3
y <- 3:1 # y -> 3,1
plot(x,y)

plot(x,y, xlim=c(1,10), ylim=c(1,5))

plot(x,y, xlim=c(1,10), ylim=c(1,5),
     xlab = "x축 값", ylab = "y축 값",
     main = "plot teat")


v1 <- c(100,130,120,160,150)
plot(v1, type = 'o', col ='red', ylim = c(0,200),axes = FALSE, ann = FALSE)

axis(1, at = 1:5 ,
     lab = c("M","T","W","THU","FRI"))
axis(2, ylim = c(0,200))

title(main = "fruit", col.main = "red", font.main = 4)
title(xlab = "day", col.lab ="black")
title(ylab = "price", col.lab = "blue")

v1

par(mfrow = c(1,3)) # par->그래프의 배치 조정하기 c(n, m) n:행의 갯수, m: 열의 갯수
plot(v1, type="o")
plot(v1, type = "s")
plot(v1, type = "l")


par(mfrow = c(2,3))


par(mfrow = c(1,3))
pie(v1)
plot(v1, type = "o")
barplot(v1)

#여백조정하기
a<- c(1,2,3)
plot(a, xlab = "aaa")

par(mgp = c(0,1,0)) #<-mpg = c(제목위치, 지표값위치, 지표선위치)

par(mgp = c(3,1,0))

par(mgp = c(3,2,0))
plot (a, xlab = "aaa")

par(mgp = c(3,2,1))

par(oma = c(0,2,0,0)) #oma <- 그래프 전체의 여백 조정하기

plot (a, xlab = "aaa")


#여러개의 그래프를 중첩으로 그리기
par(mfrow = c(1,1)) #1개만 출력 

v1 <- c(1,2,3,4,5)
v2 <- c(5,4,3,2,1)
v3 <- c(3,4,5,6,7)
plot(v1, type = "s", col = "red", ylim=c(1,5))
par(new = T)
plot(v2, type = "o", col = "red", ylim=c(5,1))

plot(v1, type = "s", col = "red", ylim=c(1,10))
lines(v2, type = "o", col = "blue", ylim = c(1, 5))
lines(v3, type = "l", col = "green", ylim = c(1,15))


# 범례추가하기
legend(4,9, c("v1","v2","v3"), cex= 0.9, col = c("red", "blue", "green"), lty = 1)
# legend( x 축위치, y 축위치, cex = 글자크기, col = 색상, pch = 크기, lty = 선모양 )

#기본 bar 그래프 그리기
x<- c(1,2,3,4,5)

barplot(x)

barplot(x, horiz = T) #가로로 출력하기기

x <- matrix(c(5,4,3,2), 2,2)
barplot(x, beside = T, names = c(5,3), col = c("green", "yellow")) # 그룹으로 묶어서 출력시키기 

barplot(x, names = c(5,3), col = c("green", "yellow"), ylim = c(0,12)) # 하나의 막대 그래프로 출력하기기

par(oma=c(1,0.5,1,0.5))#oma <- 그래프 전체의 여백 조정하기
barplot(x, names = c(5,3), col = c("green", "yellow"), horiz = T)# 그룹으로 묶어서 가로로 출력시키기


#여러 막대 그래프를 그룹으로 묶어서 한꺼번에 출력하기
v1 <-c(100,120,140,160,180)
v2 <- c(120,130,150,140,170)
v3 <- c(140,170,120,110,160)

qty <- data.frame(BANANA = v1, CHERRY = v2, ORANGE = v3)
qty

barplot(as.matrix(qty), main = "Fruit's sales qty",
        beside = T, col = rainbow(nrow(qty)), ylim = c(0,499))
legend(14,400,c("MON", "TUE", "WED", "THU", "FRI"), cex= 0.8, fill = rainbow(nrow(qty)))

#조건을 주고 그래프 그리기
#peach 값이 200이상이ㄹ경우 red, 180 - 199 는 옐로우, 그이하는 그린 색으로 출력하라는 코드 입니다.

peach <- c(180,200,250,198,170)
colors2 <- c()
for(i in 1:length(peach))
  (if(peach[i] >= 200)
    { colors2 <- c(colors2, "red")}
   else if (peach[i] >= 180)
     {colors2 <- c(colors2, "yellow")}
   else 
   {colors2 <- c(colors2, "green")})
   
colors2

barplot(peach, main = "Peach sales qty",
        names.arg= c("MON","TUE","WED","THU","FRI"), col =colors2)

#pie 기초
par(mfrow = c(1,1), oma = c(0.5,0.5,0.1,0.1))
p1 <- c(10,20,30,40)
pie (p1, radius =1)

#시작각도를 90도로 지정하기기
pie(p1, radius = 1, init.angle = 90)

#색깔과 label 명을 지정하기
pie (p1, radius = 1, init.angle = 90, col = rainbow(length(p1)),
     label = c("Week 1", "week 2", "week 3", "week 4"))

#수치 값을 함께 출력하기
pct <- round(p1/sum(p1) * 100, 1)
lab <- paste(pct, " %")
pie(p1, radius = 1, init.angle = 90, col = rainbow(length(p1)),
                                                   label =lab )
legend(1,1.1, c("week 1", "week 2", "week 3", "week 4"),
       cex = 0.5, fill = rainbow(length(p1)))

#범례를 생략하고 그래프에 바로 출력하기
pct <- round(p1/sum(p1)* 100, 1)
lab1 <- c("week 1", "week 2", "week 3", "week 4")
lab2 <- paste(lab1, "\n", pct, " %")
pie(p1, radius = 1, init.angle = 90, col = rainbow(length(p1)), label = lab2)

#pie3D 함수
install.packages("plotrix")
library(plotrix)

p1<-c(10,20,30,40,50)
f_day <- round(p1/sum(p1) * 100, 1)
f_label <- paste(f_day,"%")
pie3D(p1, main = "3d pie chart", col = rainbow(length(p1)),
      cex = 0.5, labels = f_label, explode = 0.03)
legend(0.6, 1.2, c("MON","TUE","WED","THU","FRI"),
       cex = 0.7, fill = rainbow(length(p1)))




setwd("C:/Rstudy/work_R")
str(score)

#####학생별과목별성적_국영수_new.csv 로 아래와 같은 그래프를 그리세요 #####

score <- read.csv( file = "DATA/학생별과목별성적_국영수_new.csv")

ggplot(data = score, aes(x = 이름, y = 점수, fill = 과목)) + geom_col()
