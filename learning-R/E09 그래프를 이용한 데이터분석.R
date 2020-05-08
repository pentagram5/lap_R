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

welfare <- rename(welfare,
                  sex = h10_g3, 
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion =h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region =h10_reg7)

welfare$age <- 2015 - welfare$birth+1

welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$code_region)

welfare <- welfare %>% 
  mutate(ageg = ifelse(age <30, "young", ifelse(age<=59, "middle", "old")))
qplot(welfare$ageg)

class(welfare$code_region)

welfare$ageg


#지역코드 목록 만들기
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))

list_region

welfare <- left_join(welfare, list_region, id = "code_region")

welfare %>% 
  select(code_region, region) %>% 
  head

region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group *100, 2))
head(region_ageg)

ggplot(data = region_ageg, aes(x = region, y = pct, fill =ageg)) +
  geom_col() +
  coord_flip()



#막대 정렬하기 : 노년층 비율 높은 순
#노년층 비율 내림차순 정렬
list_order_old <- region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)
list_order_old

ggplot(data = list_order_old, aes(x = region, y = pct, fill =ageg)) +
  geom_col() +
  coord_flip()

#지역명 순서 변수 만들기
order <- list_order_old$region
order

ggplot( data = region_ageg, aes(x= region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)


#연령대 순으로 막대 색깔 나열하기
class( region_ageg$ageg)

levels(region_ageg$ageg)
region_ageg$ageg <- factor(region_ageg$ageg, level = c("old","middle","young"))
class(region_ageg$ageg) #팩터형으로 변환 -> 문자형을 팩터형으로 변환 

region_ageg$ageg

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)

