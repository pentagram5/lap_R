
library("ggplot2")
##### qplot 써보기#####
library("ggplot2")
x <- c("a","a","b","c")
x
qplot(data=mpg, x = hwy)
qplot(data = mpg, x = cty)
qplot(data = mpg, x = drv, y = hwy)
qplot(data = mpg, x = drv, y =hwy, geom = "line")
qplot(data = mpg, x = drv, y =hwy, geom = "line")
qplot(data = mpg, x = drv, y =hwy, geom = "boxplot")
qplot(data = mpg, x = drv, y =hwy, geom = "boxplot", colour = drv)
##### 문제 1 평균구하기#####
x1 <- c(80,60,70,50,90)
mean(x1)
x2 <-mean(x1)
x2

##### 데이터프레임 만들기 #####
english <- c(90,80,60,70)
math <- c(50,60,100,20)
df_midterm <- data.frame(english,math)
df_midterm

class <-c(1,1,2,2)
df_midterm <- data.frame(english,math,class)
df_midterm
df_mean<-data.frame(mean(df_midterm$english), mean(df_midterm$math))
df_mean

df_midterm2 <- data.frame(english = c(90,80,60,70),
                          math = c(50,60,100,20),
                          class =c(1,1,2,2))
df_midterm2

View(df_midterm2)
str(df_midterm2)
제품 = c("사과","딸기"," 수박")
가격 = c(1800,150,3000)
판매량 = c(24,98,13)
df_shop <- data.frame(제품 ,가격 ,판매량 )
df_shop
df_shopmean <- data.frame(mean(가격), mean(판매량))
df_shopmean

