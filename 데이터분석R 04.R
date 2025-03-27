#1.데이터 프레임 만들기
english<- c(90, 80, 60, 70) #영어 점수 변수 생성

math<- c(50, 60, 100, 20) #수학 점수 변수 생성

#english, math로 데이터 프레임 생성해서 df_midterm에 할당
df_midterm<- data.frame(english, math)

class<- c(1, 1, 2, 2) #반에 대한 정보추가

df_midterm<- data.frame(english, math, class)


#분석하기
mean(df_midterm$english) #df_midterm의 english로 평균 산출





#2.데이터 프레임 한 번에 만들기
df_midterm2<- data.frame(science=c(90,80,60,70),
                         korean=c(50,60,100,20),
                         class=c(1,1,2,2))





#3.외부 데이터 이용하기
install.packages("readxl")

#엑셀 파일
library(readxl)
df_exam <- read_excel("excel_exam.xlsx")

df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F) #첫 번째 행을 데이터로 인식
df_exam_sheet <- read_excel("excel_exam_sheet.xlsx", sheet = 3) #3번째 시트의 데이터 불러오기


#CSV 파일
df_csv_exam <- read.csv("csv_exam.csv")
write.csv(df_midterm, file = "df_midterm.csv")

#RDS 파일
load("df_midterm.rds")
save(df_midterm, file = "df_midterm.rds")
