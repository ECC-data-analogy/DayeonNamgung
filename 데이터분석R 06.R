#1. 조건에 맞는 데이터만 추출하기
install.packages("dplyr")
library(dplyr)
exam<- read.csv("csv_exam.csv")

exam %>% filter(class == 1) #class가 1인 경우만 추출 및 출력
exam %>% filter(class != 3) #3반이 아닌 경우
exam %>% filter(math < 50) #수학 점수가 50점 미만인 경우

exam %>% filter(class == 1 & math < 50) #1반이면서 수학 점수가 50점 미만인 경우
exam %>% filter(english >= 90 | math >= 90) #영어점수가 90점 이상이거나 수학 점수가 90점 이상인 경우
exam %>% filter(class == 1|class == 3|class == 5) #1, 3, 5반에 해당하면 추출
exam %>% filter(class %in% c(1, 3, 5)) #1, 3, 5반에 해당하면 추출

class1 <- exam %>% filter(class == 1)
mean(class1$math) #1반 수학점수 평균




#2. 필요한 변수만 추출하기
exam %>% select(math) #math 추출
exam %>% select(class, math, english) #class, math, english 추출
exam %>% select(-math) #math 제외

exam %>% 
  filter(class == 1) %>% 
  select(english) #class가 1인 행만 추출한 다음 english 추출

exam %>%
  select(id, math) %>%
  head #id, math의 앞부분 6행까지 추출





#3. 순서대로 정렬하기
exam %>% arrange(math) #math 오름차순 정렬
exam %>% arrange(desc(math)) #math 내림차순 정렬





#4. 파생변수 추가하기
exam %>% 
  mutate(total = math + english + science) %>%
  head #총합 변수 추가, 일부 추출
exam %>% 
  mutate(total = math + english + science, 
         mean = (math + english + science)/3) %>%
  head #총합 변수 추가, 총 평균 변수 추가, 일부 추출

exam %>% 
  mutate(test = ifelse(science >=60, "pass", "fail")) %>%
  head #mutate에 ifelse적용하기
exam %>% 
  mutate(total = math + english + science) %>%
  arrange(total) %>%
  head #총합 변수 추가, 총합 변수 기준 정렬, 일부 추출





#5. 집단별로 요약하기
exam %>% summarise(mean_math = mean(math)) #math 평균 산출
exam %>% 
  group_by(class) %>%
  summarise(mean_math = mean(math)) #class별로 분리, math 평균 산출
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math), 
            median_math = median(math),
            n = n ()) #class별로 분리, math 평균, 합계, 중앙값, 학생 수 산출

install.packages("ggplot2")
mpg <- as.data.frame(ggplot2::mpg)

mpg %>% 
  group_by(manufacturer, drv) %>%
  summarise(mean_cty = mean(cty)) %>%
  head(10) #회사별, 구동 방식별 분리, cty 평균 산출, 일부 출력

mpg %>%
  group_by(manufacturer) %>%
  filter(class == "suv") %>%
  mutate(tot = (cty+hwy)/2) %>%
  summarise(mean_tot = mean(tot)) %>%
  arrange(desc(mean_tot)) %>%
  head(5) #회사별로 "suv"자동차의 도시 및 고속도로 통합 연비 평균을 구해 내림차순으로 정렬하고, 1~5위까지 출력하기





#6. 데이터 합치기
#가로로 합치기
test1 <- data.frame(id = c(1, 2, 3, 4, 5), 
                    midterm = c(60, 80, 70, 90, 85))
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

total<- left_join(test1, test2, by = "id") #id를 기준으로 합쳐 total에 할당

#세로로 합치기
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 85))
group_b <-data.frame(id = c(1, 2, 3, 4, 5),
                     test = c(70, 83, 65, 95, 80))

group_all <- bind_rows(group_a, group_b) #데이터 합쳐서 group_all에 할당
