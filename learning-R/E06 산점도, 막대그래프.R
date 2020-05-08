#산점도 : 데이터를 x축과 y축으로 표현(점으로)
library(ggplot2)
library("dplyr")

mpg <- as.data.frame(ggplot2::mpg)

ggplot(data = mpg, aes(x=displ, y=hwy))#aes뒤로 x축과 y축을 형성할 컬럼(속성) 을 지정해준다.

ggplot(data = mpg, aes(x=displ, y=hwy)) + geom_point()#x와 y가 만나는 지점을 point해줌


#- 축 범위 조정하기
ggplot(data = mpg, aes(x=displ, y=hwy)) +geom_point() + xlim(3,6) #x축에 범위 설정 ->displ - 3~6까지

ggplot(data = mpg, aes(x=displ, y=hwy)) +
  geom_point() + 
   xlim(3,6) + 
     ylim(20,30)#y축에 범위설정

ggplot(data = mpg, aes(x=displ, y=hwy)) +
  geom_point(colour = "red", size = 3) + #산점도 그래프 특성바꾸기(포인트 색, 크기)
  xlim(3,6) + 
  ylim(20,30)


# 꺽은선 그래프 
ggplot(data = mpg, aes(x=displ, y=hwy)) +
  geom_line(colour = "red", size = 3) + 
  xlim(3,6) + 
  ylim(10,30)

#꺽은선과 산점도 합치기 
ggplot(data = mpg, aes(x=displ, y=hwy)) +
  geom_line(colour = "red") + 
  geom_point(colour = "blue", size = 3) +
  xlim(3,6) + 
  ylim(10,30)
### 산점도, 꺽은선 그래프는 두변수의 관계 파악!




#ggplot -> 최종보고용, 색 크기 폰트등 세부조작 가능 그래프
#qplot -> 전처리 단계 데이터 확인용 문법간단, 기능단순

#혼자서 해보긔
ggplot(data = mpg, aes(x=cty, y=hwy)) +

  geom_point(colour = "blue", size = 3) 
midwest <- rename(midwest, poptotal=total, popasian=asian)


ggplot(data = midwest, aes(x =poptotal, y=popasian))+
  geom_point(colour = "blue", size = 2) +
  xlim(0,500000) + 
  ylim(0,10000)


#막대그래프 -> 평균 막대 그래프 만들기
### 막대그래프는 하나의 변수에 대해 각 값의 빈도 파악에 사용!

#1. 집단별 평균표 만들기
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_mpg

# 2. 그래프 생성하기
ggplot(data = df_mpg, aes( x = drv, y = mean_hwy)) + 
  geom_col()

ggplot(data = df_mpg, aes( x = reorder(drv, -mean_hwy), y = mean_hwy)) + 
  geom_col() # 크기 순으로 정렬하기(내림차순)

ggplot(data = df_mpg, aes( x = reorder(drv, mean_hwy), y = mean_hwy)) + 
  geom_col() # 크기 순으로 정렬하기(오름차순)

ggplot(data = df_mpg, aes( x = reorder(drv, mean_hwy), y = mean_hwy)) + 
  geom_col(fill = c('red','blue','pink')) ##막대 컬러 설정

# 막대그래프 2 -> 빈도 막대 그래프

ggplot(data = mpg, aes(x=drv)) + geom_bar() # 값의 개수(빈도)로 막대의 길이를 표현한 그래프 

ggplot(data = mpg, aes(x=manufacturer)) + geom_bar()

ggplot(data = mpg, aes(x=hwy)) +geom_bar()

#geom_col -> 데이터를 요약한 평균표를 먼저 만든 후 평균표를 이용해 그래프 생성
#geom_bar -> 별로로 표를 만들지 ㅇ낳고 원자료를 이용해 바로 그래프 생성

# 혼자서 해보기
df_mpg <- mpg %>% 
  filter(class =="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(5)


ggplot(data = df_mpg, aes( x = reorder(manufacturer, -mean_cty), y = mean_cty)) + 
  geom_col(fill = c('red','blue','gold','pink','grey'))

ggplot(data = mpg, aes( x= class ))+ geom_bar(fill = c('red','blue','gold','pink','grey','black','green'))

#시계열 그래프 만들기

ggplot(data = economics, aes(x=date, y = unemploy)) + geom_line()

ggplot(data = economics, aes(x=date, y = psavert)) + geom_line()

p<- ggplot(data = economics, aes(x=date, y = psavert)) + geom_line( color = "#00AFBB", size =1)
p

#시계열 그래프 기간정해서 출력하기
min <- as.Date("2002-1-1")
max <- as.Date("2010-1-1")

p + scale_x_date(limits = c(min,max))


# 꺽은 선그래프에  수직선 추가하기!
ggplot(data = economics, aes(x=date, y= psavert))  + 
  geom_line() +
  geom_hline(yintercept = mean (economics$psavert))


###상자 그림 - 집단간 분포 차이 표현하기
# 상자그림 - 데이터의 분포를 직사각형 상자 모양으로 표현한 그래프

ggplot(data = mpg, aes(x=drv, y= hwy)) +geom_boxplot()

# 상자내 굵은선 -> 하위 50% 위치 값(중앙값)

df_mpgnew <- mpg %>% 
  filter(class %in% c("compact","subcompact","suv"))
ggplot(data = df_mpgnew, aes(x=class, y = cty)) +geom_boxplot()

# text 추가하기
# 산점도를 그리고 데이터 레이블 입력하기
ggplot(airquality, aes(x=Day, y= Temp)) +
  geom_point()+
  geom_text(aes(label = Temp, vjust = 0, hjust = 0))

#산점도에 사각형 그리기 (annotate())
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()+
  annotate("rect", xmin=3, xmax=4, ymin=12, ymax = 21, alpha =0.7, fill="skyblue") #alpha = 투명도


#산점도에 사각형 및 화살표  그리기

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()+
  annotate("rect", xmin=3, xmax=4, ymin=12, ymax = 21, alpha =0.7, fill="skyblue") +
  annotate("segment", x=2.5, xend=3.7, y=10, yend = 17, color="red", arrow=arrow()) 

#산점도에 사각형 및 화살표 및 텍스트 추가
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()+
  annotate("rect", xmin=3, xmax=4, ymin=12, ymax = 21, alpha =0.7, fill="skyblue") +
  annotate("segment", x=2.5, xend=3.7, y=10, yend = 17, color="red", arrow=arrow()) +
  annotate("text", x= 2.5, y=10, label="point")


##https://rpubs.com/kimwoohyung/ggplot2 참고하기 


