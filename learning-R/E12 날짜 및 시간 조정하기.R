#####기초 문법 - 날짜와 시간 #####

Sys.Date()
Sys.time()
date()
as.Date("2014-11-01")#문자형을 날짜로 강제 변환하기 

as.Date("01-11-2014", format = "%d-%m-%Y")  #날짜 형태 지정 

as.Date("2014년 11월 01일", format = "%Y년 %m월 %d일")  #날짜 형태 지정 

as.Date("01112014", format = "%d%m%Y")# 년도에 대문자 Y

as.Date("011114", format = "%d%m%y")#년도에 소문자 y

as.Date(10, origin = "2014-11-10")#주어진 날짜 기준으로 10일 후의 날짜

#날짜 계산하는법 
as.Date("2014-11-30") - as.Date("2014-11-15")

as.Date("2014-11-01") + 5


#POSIXct-> 초단위로 계산되는 날짜 
as.POSIXct("2014-11-01 20:00:00") - as.POSIXct("2014-11-01 18:00:00")
