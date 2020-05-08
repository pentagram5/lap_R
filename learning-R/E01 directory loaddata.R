##### 외부데이터 불러오기 #####
# 디렉토리 지정 = 지정장소에서 작업을 실시, 데이터파일 불러오기
setwd("C:/Rstudy/work_R")
#현재 디렉토리 주소 출력
getwd()

#지정한 디렉토리에 있는 factor_test txt데이터파일불러오기
txt1<-read.csv("DATA/factor_test.txt")
txt1

#엑셀파일 불러오기
df_exam <-read_excel("DATA/excel_exam.xlsx")
df_exam
mean(df_exam$english)
mean(df_exam$science)

#엑셀파일의 첫번째 행을 변수명으로 인식 후 불러오기
df_exam_novar <- read_excel("DATA/excel_exam_novar.xlsx", col_names = F)
df_exam_novar

#엑셀파일의 특정 sheet 불어오기
df_exam_sheet <- read_excel("DATA/excel_exam_sheet.xlsx", sheet=3)
df_exam_sheet

#csv 파일 불어오기 -> 범용데이터형식, 값사이를 쉼표로 구분, 용량이 작다
df_csv_exam <-read.csv("DATA/csv_exam.csv")
df_csv_exam

#문자가 들어있는 파일 불러올때 stringsAsFactors = F
df_csv_exam1 <- read.csv("DATA/csv_exam.csv", stringsAsFactors = F)
df_csv_exam1

#데이터 프레임을 csv파일로 저장하기
df_midterm <- data.frame(english = c(90,80,60,70),
                         math = c(50,60,100,20),
                         class =c(1,1,2,2))
write.csv(df_midterm, file = "DATA/df_midterm.csv")

df_fluit <- data.frame(No = c(1,2,3,4),
                       과일 = c("사과","포도","키위", "망고"))
df_fluit
write.csv(df_fluit, file = "DATA/df_fluit.csv")


#####데이터 파악하기#####
#exam데이터 파악하기
exam <- read.csv("DATA/csv_exam.csv")
View(exam)

head(exam) #앞에서부터 6행까지출력
head(exam, 10) #앞에서부터 10행까지 출력

tail(exam) #뒤에서부터 6행까지 출력
tail(exam, 10) #뒤에서부터 10행까지 출력

dim(exam) #데이터의 행과 열 출력력
str(exam) #데이터 속성확인

summary(exam) #요약통계량 출력

#mpg 데이터 파악하기

mpg <- as.data.frame(ggplot2::mpg)#패키지(ggplot2)의 데이터(mpg)를 불러서 mpg라는 이름으로 저장
head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)

##### 데이터 수정하기 #####
#dplyr 패키지 설치, 로드
install.packages("dplyr")
library(dplyr)

#데이터 프레임 생성 및 수정
df_raw <-data.frame(var1 = c(1,2,1),
                    var2 = c(2,3,2))
df_raw

df_new <- df_raw #데이터 복사

df_new <- rename(df_new, v2 = var2) #var2를 v2로 수정
df_new


#실습
mpg_copy <- as.data.frame(ggplot2::mpg)
mpg_copy1 <- mpg_copy
mpg_copy1 <- rename(mpg_copy1, city=cty, highway=hwy)
head(mpg_copy1)


#파생변수 만들기
df <-data.frame(var1 = c(4,3,8),
                  var2 = c(2,6,1))
df$var_sum <- df$var1 + df$var2 #var_sum 파생변수 생성,(var1+var2)

df$var_mean <-(df$var1+df$var2)/2 #var_mean 파생변수 생성 
df

#mpg 통합 연비 변수 만들기
mpg$total <- (mpg$cty+mpg$hwy)/2
head(mpg)

#기준값 정하기
summary(mpg$total)
hist(mpg$total)

#조건문으로 합격판정변수 만들기
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail") #total이 20이상이면 pass, 아니면 fail을 넣는 조건 변수 
head(mpg,20)

#빈도표로 합격 판정 자동차 수 살펴보기
table(mpg$test) #연비 합격 빈도표 생성성


#막대그래프로 빈도 표현하기
library(ggplot2)
qplot(mpg$test)

#중첩 조건문 활용하기 - 연비 등급
mpg$grade <- ifelse(mpg$total >=30, "A", ifelse(mpg$total>= 20, "B","C"))
head(mpg)
qplot(mpg$grade)

#원하는 만큼 범주만들기
mpg$grade <- ifelse(mpg$total >=30, "A", 
                    ifelse(mpg$total>= 25, "B",
                           ifelse(mpg$total>=20,"C","D")))

