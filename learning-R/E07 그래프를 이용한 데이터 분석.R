install.packages("foreign")
library(foreign)#foreign 패키지 설치 (spss, sas, stata 등 통계분석 s/w)
library(dplyr)
library(ggplot2)
library(readxl)
setwd("C:/Rstudy/work_R")
getwd()
raw_welfare <- read.spss( file = "DATA/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
View(raw_walfare)

welfare<-raw_welfare
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3, 
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion =h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region =h10_reg7)


#####성별에 따른 월급차이#####

class(welfare$sex)
table(welfare$sex)

#이상치 결측 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
#결측치 확인
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)

qplot(welfare$sex)

#월급 변수 검토 및 전처리

class(welfare$income)
summary(welfare$income)

qplot(welfare$income) +xlim(0,1000)

#이상치 결측 처리 -> 1~9998
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)

table(is.na(welfare$income))
      
#성별 월급 평균표 만들기
sex_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income

# 월급 평균표를 이용해 그래프 만들기(geom_col())
ggplot(data = sex_income, aes(x= sex, y = mean_income))+geom_col()

##### 나이에 따른 월급차이 #####
class(welfare$birth)
table(welfare$birth)
qplot(welfare$birth)
#결측치 확인
table(!is.na(welfare$birth))

#이상치 결측처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))
str(welfare$birth)

welfare$age <- 2015 - welfare$birth+1
summary(welfare$age)

#나이에 따른 월급 평균표 만들기
age_income <- welfare %>% 
  filter(!is.na(income)) %>%
  group_by(age) %>% 
  summarise(mean_income = mean(income))

ggplot(data = age_income, aes(x=age, y= mean_income))+
  geom_line()

#연령대에 따른 월급차이 분석하기
#연령대 나누기
welfare <- welfare %>% 
  mutate(ageg = ifelse(age <30, "young", ifelse(age<=59, "middle", "old")))
qplot(welfare$ageg)


#연령대별 월급 평균표 만들기
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income)) 


#범주 순서 지정 : scal_x_discrete(limits = c())

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))


#####성별 월급차이는 연령대별로 다를까?#####

#연령대 및 성별 월급 평균표 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
sex_income

#그래프 만들기 
ggplot( data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#성별에 따라 분리한 막대 만들기
#geom_col()의 position 파라미터를 "dodge"로
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

#나이 및 성별 월급 차이 분석하기
#성별 연령별 월급 평균표 만들기
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)
ggplot(data = sex_age, aes(x = age, y= mean_income, col= sex)) +geom_line()


##### 직업에 따른 월급차이 #####
#변수 검토하기 
class(welfare$code_job)
table(welfare$code_job)

#직업 코드표 불러오기
library(readxl)
list_job <- read_excel("DATA/Koweps_Codebook.xlsx", col_names = T, sheet =2)
list_job

#welfare에 직업명 결합
welfare<- left_join(welfare, list_job, id = "code_job")
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

#직업별 수익 평균
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

# 최고수익탑 10 
top10 <- job_income %>% 
   arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data = top10, aes( x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_filp()

##### 종교 유무에 따른 이혼율 #####
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

#혼인 상태 변수 검토 및 전처리하기

class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage", 
                                 ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
qplot(welfare$group_marriage)

religion_divorce <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))
religion_divorce

#이혼 추출
divorce <- religion_divorce %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)
divorce

ggplot(data = religion_divorce, aes( x = religion, y = pct)) + geom_col() +ylim(0, 10)



#####한국복지패널데이터를 이용한 문제 #####

#결혼 유무를 체크할 파생변수 
welfare$marriagech <- ifelse(welfare$marriage == 1, "marriage", 
                                ifelse(welfare$marriage == 5,"non-marrige", NA ))
##결혼 유/무와 소득의 관계확인
marriage_income <- welfare %>% 
  select(marriagech, income) %>% 
  filter(!is.na(marriagech) & !is.na(income)) %>% 
  group_by(marriagech) %>% 
  summarise(mean_income = mean(income))

marriage_income
#결혼 유/무와 소득의 관계확인 그래프 
ggplot(data = marriage_income, aes( x= marriagech, y = mean_income)) +geom_col()

##결혼 유무와 남녀 소득의 관계 확인
marriage_sex_income <- welfare %>% 
  select(marriagech, sex, income) %>% 
  filter(!is.na(marriagech) & !is.na(income)) %>% 
  group_by(marriagech, sex) %>% 
  summarise(mean_income = mean(income))

marriage_sex_income 

##셜혼 유무와 남녀 소득의 관계 확인 그래프 
ggplot( data = marriage_sex_income, aes(x = marriagech, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") 


