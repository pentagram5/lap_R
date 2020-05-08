setwd("C:/Rstudy/work_R")
df_examex <- read_excel("DATA/excel_exam.xlsx")
df_examex <- head(df_examex, 20)
df_examex
df_examex$s_total <- df_examex$math+df_examex$english+df_examex$science
df_examex$s_total
df_examex$s_mean <- (df_examex$math+df_examex$english+df_examex$science)/3

df_examex$s_mean
df_examex$test1 <- ifelse(df_examex$s_mean >= 60, "pass", "fail")
df_examex$test1

table(df_examex$test1)
qplot(df_examex$test1)

df_examex$test2 <- ifelse(df_examex$s_mean >= 80, "A", ifelse(df_examex$s_mean >= 70,"B","C"))
head(df_examex$test2, 10)
tail(df_examex$test2, 7)

table(df_examex$test2)
qplot(df_examex$test2)
