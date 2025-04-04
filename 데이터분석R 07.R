#1. 결측치 정제하기

#결측치 찾기
df<- data.frame(sex = c("M", "F", NA, "M", "F"),
                score = c(5, 4, 3, 4, NA))

is.na(df) #결측치 확인
table(is.na(df)) #결측치 빈도 출력
table(is.na(df$sex))
mean(df$score) #평균 산출
sum(df$score) #합계 산출

#결측치 제거하기
library(dplyr)
df %>% filter(is.na(score)) #score가 NA인 데이터만 출력
df %>% filter(!is.na(score)) #score 결측치 제거

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
mean(df_nomiss)

df_nomiss2 <- na.omit(df) #모든 변수에 결측치 없는 데이터 추출

#함수의 결측치 제외 기능 이용하기
mean(df$score, na.rm = T) #결측치 제외하고 평균 산출

exam <- read.csv("csv_exam.csv")
exam[c(3, 8, 15), "math"] <-NA #3, 8, 15행의 math에 NA할당
exam %>% summarise(mean_math = mean(math, na.rm = T),
                   sum_math = sum(math, na.rm = T),
                   median_math = median(math, na.rm = T)) #math 결측치 제외하고 평균, 합계, 중앙값 산출

#평균값으로 결측치 대체하기
mean(exam$math, na.rm = T)
exam$math<- ifelse(is.na(exam$math), 55, exam$math) #math가 NA면 55로 대체





#2. 이상치 정제하기
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
table(outlier$sex) #이상치 확인하기
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex) #결측 처리하기
outlier$sex <- ifelse(outlier$score > 5, NA, outlier$score)

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score)) #결측치 제외 후 성별에 따른 score 평균


#극단적인 값 제거
install.packages("ggplot2")
mpg <- as.data.frame(ggplot2::mpg)

boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats #상자 그림 통계치 출력
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy >37, NA, mgp$hwy) #12-37 벗어나면 NA 할당
mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))