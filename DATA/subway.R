library(dplyr)      # dplyr 패키지 로드
library(readxl)    # readxl 패키지 로드

# dustdata.xlsx 파일을 불러온 후 dustdata_anal에 할당
dustdata_anal <- read_excel("C:/Rstudy/dustdata.xlsx")

View(dustdata_anal)
str(dustdata_anal)
