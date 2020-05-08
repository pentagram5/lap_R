#####자유자재로 데이터 가공하기#####
library(dplyr)
#데이터 전처리-원하는 형태로 데이터 가공
# 데이터 전처리 -> Preprocessing- dplyr 패키지 가장많이 사용
# filter() 행추출
# select() 열추출
# arrange() 정렬
# mutate() 변수추가
# summarize () 요약
setwd("C:/Rstudy/work_R")
getwd()
exam <- read.csv("DATA/csv_exam.csv")

exam

#파이프 연산자 -> cirt + shift + m->직접적으로 데이터프레임의 수정을 하진 않음
exam %>% filter(class == 1) # filter 에 조건을 입력할때 파이프연산자 사용 필요!
exam %>% filter(class == 2) # 2반만 추출
exam %>% filter(class != 1) # 1반이 아닌 행 추출
exam %>% filter(class != 2)
exam %>% filter(math > 50)
exam %>% filter(math <50)
exam %>% filter(math == 50)
exam %>% filter(english >= 80)
exam %>% filter(english <= 80)
exam %>% filter(class == 1 & math >= 50)
exam %>% filter(class == 2 & english >= 80) #and 연산자 -> &
exam %>% filter(math >= 90 | english >= 90) #or 연산자 -> |
exam %>% filter(class ==  1 | class ==2 )
exam %>% filter(class ==  1 | class ==2 | class == 5)

# %in% in 연산자
exam %>% filter(class %in% c(1,3,5)) #class 가 1,3,5 중에 속한 행 추출(매칭연산자)

# 추출한 행으로 데이터 만들기
class1 <- exam %>% filter(class ==1) #exam에서 클래스가 1인 행으로 class1 데이터 구성
class2 <- exam %>% filter(class ==2) #exam에서 클래스가 2인 행으로 class1 데이터 구성


# R에서 사용하는 기호, ^,**-> 제곱 , %/% -> 나눗셈의 몫. %%나눗셈의 나머지

#혼자서 해보기

mpg <- as.data.frame(ggplot2::mpg)
mpg %>% filter(displ <= 4)

hwyferdispl <- mpg %>%  filter(displ <= 4)
hwyferdispl <- mean(hwyferdispl$hwy)
hwyferdisp2 <- mpg %>%  filter(displ >= 5)
hwyferdisp2 <- mean(hwyferdisp2$hwy)

audi1 <- mpg %>% filter(manufacturer =="audi")
toyota1 <- mpg %>% filter(manufacturer =="toyota")
mean(audi1$cty)
mean(toyota1$cty)
sss <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(sss$hwy)


#필요한 변수만 추출하기(select)
exam %>% select(math)
exam %>% select(class, math, english)

exam %>% select(-math) #-데이터 빼고 추출

exam

#함수 조합하기
exam %>% filter(class==1) %>% select(english)

exam %>% 
  filter(class==1) %>% 
  select(english) # Enter 키로 가독성 증가

exam %>% 
  select(id,math) %>% 
  head(10) #앞부분 10행만 추출


exam %>% 
  select(id,math) %>% 
  tail(5) #뒷부분 5행만 추출


#혼자서 해보기 

mpgnew <- mpg %>% select(class, cty)
mpgnew
mpgnewsuv <- mpg %>% filter(class == "suv") %>% select(cty)
mpgnewcom <- mpg %>% filter(class == "compact") %>% select(cty)
mpgset <- c(suvctyavg = mean(mpgnewsuv$cty), comctyavg = mean(mpgnewcom$cty))
mpgset
mpgset <- data.frame( suv11 = mean(mpgnewsuv$cty),
                      comctyavg = mean(mpgnewcom$cty) )

#Arrange()- 정렬
exam %>% arrange(math) #math 오름차순 정렬렬
exam %>% arrange(desc(math)) #내림차순 정렬 
exam %>% arrange(class, math)
exam %>% arrange(class, desc(math))
mpg

mpg %>% 
  filter(manufacturer=="audi") %>% 
    select(model, hwy) %>% 
      arrange(desc(hwy)) %>%
        head(5)         

#mutate() - 변수 추가해서 추출(파생변수 X!!)

exam %>% 
  mutate(total = math+english+science) %>%  #총합 변수 추가
  head

exam %>% 
  mutate( total =math+english+science,
          mean = (math+english+science)/ 3) %>% 
  head

exam1 <- exam %>% 
  mutate( test = ifelse(science >= 60, "pass", "fail")) %>% 
  head

exam %>% 
  mutate(total = math+english+science) %>% 
  arrange(total) %>% head

#mpg 데이터 분석 
mpgcop <- mpg

mpgcop <- mpgcop %>% 
    mutate(total = hwy+cty)

mpgcop$ally <- (mpgcop$total) /2

mpgcop %>% select(manufacturer, ally) %>% 
  arrange(desc(ally)) %>% 
    head(3)


mpg %>% 
  mutate(total=hwy+cty, ally = total/2) %>% 
    select(manufacturer,model, ally)%>% 
      arrange(desc(ally)) %>% 
        head(3)


#summarise_요약하기 

exam %>%  summarise(mean_math=mean(math)) #math평균 산출출

exam %>% 
  group_by(class) %>%           #class 별로 분리
  summarise(mean_math=mean(math))

exam %>% 
  group_by(class) %>% 
  summarise( mean_math = mean(math),#수학점수 평균
             sum_math = sum(math),#점수 합계
             median_math = median(math),#수학 중앙값
             n = n())#학생수

mpg %>% 
  group_by(manufacturer ,drv) %>% 
    summarise(mean_cty = mean(cty)) %>% 
      arrange(desc(mean_cty)) %>% 
      head(10)

str(mpg)

mpg %>% 
  group_by(manufacturer, class)%>%
    filter(class =="suv") %>% 
      summarise(mean_cthwt = mean( (cty+hwy)/2)) %>% 
        arrange(desc(mean_cthwt))  %>% 
          head(5)
         


#데이터 합치기

test1 <- data.frame(id=c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85)) #중간고사

test2 <- data.frame(id=c(1,2,3,4,5),
                    final = c(70,83,65,95,80)) #기말고사

#id 기준으로 합치기 -join
total <- left_join(test1, test2, by = "id") #id 기준으로 합쳐 total에 할당
total


#다른 데이터 활용해 변수 추가하기

#반별 담임교사 명단 생성
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim","lee","park","choi","jung"))
exam_new <- left_join(exam, name, by = "class")
exam_new

#세로로 합치기

group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))
group_b <- data.frame(id=c(6,7,8,9,10),
                      test = c(70,83,65,95,80))

group_all <- bind_rows(group_a, group_b)#데이터를 세로로 합쳐서 group_all 에 할당 -> bind_rows
group_all


str(mpg)
gas <-data.frame(fl = c("c","d","e","p","r"),
                 price_f1 = c(2.35,2.37,2.11,2.76,2.22),
                 stringsAsFactors = F);#문자를 factor타입으로 변환하지 않도록 설정하는 파라미터
mpg_gas <- left_join(mpg, gas, by = "fl")
str(mpg_gas)
summary(mpg_gas)
table(mpg_gas$fl)
