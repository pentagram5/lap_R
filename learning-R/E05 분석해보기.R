library(ggplot2)
library(dplyr)# rename을 위해 필요한 패키지

#####분석도전문제#####
# ggplot2에는 미국동북중부지역의 인구통계 정보를 담은 데이터가 포함되었다. 이를 통해 데이터를 분석해본다.
midwest <- as.data.frame(ggplot2::midwest)
midwest
str(midwest)
dim(midwest)
midwest <- rename(midwest, total=poptotal, asian = popasian)
midwest$asianper <- (midwest$asian/midwest$total) *100
hist(midwest$asianper)#asianper의 히스토그램 구하기

midwest$asianper
mean(midwest$asianper)
midwest$asianmean <- ifelse(midwest$asianper > mean(midwest$asianper), "Large","small")
table(midwest$asianmean)
qplot(midwest$asianmean)
