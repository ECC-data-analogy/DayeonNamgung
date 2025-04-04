#데이터 분석 준비- '한국복지패널데이터'
install.packages("foreign") #foreign 패키지 설치
install.packages("dplyr")
install.packages("ggplot2")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", 
                         to.data.frame = T) #데이터 불러오기
welfare <- raw_welfare #복사본 만들기
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10, 
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9, 
                  code_region = h10_reg7)





#성별에 따른 월급 차이
class(welfare$sex)
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) #이상치 결측 처리
welfare$sex <- ifelse(welfare$sex == 1, "male","female") #성별 항목 이름 부여

class(welfare$income)
summary(welfare$income)
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income) #이상치 결측 처리

sex_income <- welfare %>%
  filter(!is.na(income))%>%
  group_by(sex) %>%
  summarise(mean_income = mean(income)) #성별 월급 평균본 만들기
ggplot(data = sex_income, aes(x = sex, y = mean_income)) +geom_col() #그래프





#나이와 월급 관계
class(welfare$birth)
summary(welfare$birth) #이상치 확인 
table(is.na(welfare$birth)) #결측치 확인
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth) #이상치 결측 처리

welfare$age <- 2015 - welfare$birth + 1 #나이 파생변수 만들기기
summary(welfare$age)

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income)) #나이에 따른 월급 평균본
head(age_icome)
ggplot(data = age_income, aes(x = age, y = mean_income)) +geom_line() #그래프





#연령대에 따른 월급 차이
welfare <- welfare %>%
  mutate(ageg = ifelse(age<30, "young", 
                       ifelse(age <= 59, "middle", "old"))) #연령대 변수 검토 및 전처리
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income)) #연령대에 따른 월급 평균본
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col() #그래프
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col() + scale_x_discrete(limits = c("young", "middle", "old")) #나이 순 정렬





#연령대 및 성별 월급 차이
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income)) #연령대 및 성별 월급 평균본
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old")) #그래프
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_alpha_discrete(limits = c("young", "middle", "old")) #남자, 여자 막대 분리

sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income)) #성별 연령별 월급 평균본
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line() #그래프





#직업별 월급 차이
class(welfare$code_job)
table(welfare$code_job)

library(readxl) #전처리
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
welfare <- left_join(welfare, list_job, by = "code_job")
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(10)

job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income)) #직업별 월급 평균본

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10) #월급 내림차순 정렬, 상위 10개 추출
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip() #그래프

bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10) #하위 10개 추출
ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850) #그래프





#성별 직업 빈도
job_male <- welfare %>%
  filter(!is.na(job) & sex == "male") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) #남성 직업 빈도 상위 10개 추출
job_female <- welfare %>%
  filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) #여성 직업 빈도 상위 10개 추출
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip() #남성 직업 빈도 상위 10개 직업
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip() #여성 직업 빈도 상위 10개 직업






#종교 유무에 따른 이혼율
class(welfare$religion)
table(welfare$religion)
welfare$religion <- ifelse(welfare$religion ==1, "yes", "no") #전처리

class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA)) #이혼 여부 변수 만들기
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n),
         pct = round(n/tot_group*100, 1)) #종교 유무애 따른 이혼율 표
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>% #집단별 빈도 구함
  group_by(religion) %>%
  mutate(pct = round(n/sum(n)*100, 1)) #종교 유무애 따른 이혼율 표
divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct) #이혼 추출
ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col() #그래프

ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n),
         pct = round(n/tot_group*100, 1)) #연령대별 이혼율 표
ageg_divorce <- ageg_marriage %>%
  filter(ageg != "young" & group_marriage == "divorce") %>%
  select(ageg,pct) #초년 제외, 이혼 추출
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) +geom_col() #그래프

ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>%
  group_by(ageg, religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n),
         pct = round(n/tot_group*100, 1)) #연령대, 종교 유무, 결혼 상태별 비율표
df_divorce <- ageg_religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(ageg, religion, pct) #연령대 및 종교 유무별 이혼율 표
ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) + geom_col(position = "dodge") #그래프






#지역별 연령대 비율
class(welfare$code_region)
table(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울", 
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도")) #지역 코드 목록 만들기
welfare <- left_join(welfare, list_region, by = "code_region") #지역명 변수 추가
welfare %>%
  select(code_region, region) %>%
  head

region_ageg <- welfare %>%
  group_by(region, ageg) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n),
         pct = round(n/tot_group*100,2)) #지역별 연령대 비율표
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() #그래프

list_order_old <- region_ageg %>%
  filter(ageg == "old") %>%
  arrange(pct) #노년층 비율 내림차순 정렬
order <- list_order_old$region #지역명 순서 변수 만들기
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order) #그래프

class(region_ageg$ageg)
levels(region_ageg$ageg)
region_ageg$ageg<- factor(region_ageg$ageg, 
                          level = c("old", "middle", "young")) #factor 타입 변환, 순서 지정
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order) #그래프
