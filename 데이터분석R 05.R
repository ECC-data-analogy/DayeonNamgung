exam <- read.csv("csv_exam.csv")

#1.데이터 파악하기
head(exam) #앞에서부터 6행까지 출력

head(exam, 10) #앞에서부터 10행까지 출력

tail(exam) #뒤에서부터 6행까지 출력

tail(exam, 10) #뒤에서부터 10행까지 출력

View(exam) #데이터 뷰어 창에서 exam 데이터 확인하기

dim(exam) #행, 열 출력

str(exam) #데이터 속성 확인

summary(exam) #요약 통계량 출력


#mpg 데이터 파악하기
install.packages("ggplot2")

mpg <- as.data.frame(ggplot2::mpg) #프레임 형태로 불러오기기

head(mpg)
tail(mpg)
dim(mpg)
str(mpg)
summary(mpg)





#2.변수명 바꾸기
df_raw <- data.frame(var1 = C(1,2,1),
                     var2 = c(2,3,2))

install.packages("dplyr")
library(dplyr)

df_new <- df_raw #복사본 생성

df_new <- rename(df_new, v2 = var2) #var2를 v2로 수정





#3. 파생변수 만들기
df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,1))

df$var_sum <- df$var1 + df$var2 #var_sum 파생변수 생성
df$var_mean <- (df$var1 + df$var2)/2 #var_mean 파생변수 생성





#mpg 통합 연비 변수 만들기
mpg$total <- (mpg$cty + mpg$hwy)/2 #통합 연비 변수 생성
head(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total) #히스토그램 생성

mpg$test <- ifelse(mpg$total >= 20, "pass", "fail") #20이상이면 pass, 아니면 fail
head(mpg, 20)
table(mpg$test) #연비 합격 빈도표 생성

library(ggplot2)
qplot(mpg$test) #연비 합격 빈도 막대 그래프 생성





#중첩 조건문 활용하기
mpg$grade <- ifelse(mpg$total >=30, "A", ifelse(mpg$total >= 20, "B","C"))
head(mpg, 20)
table(mpg$grade)
qplot(mpg$grade)

mpg$grade2 <- ifelse(mpg$total >=30, "A", 
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D")))