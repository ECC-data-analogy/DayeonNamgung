#1. 미국 주별 강력 범죄율 단계 구분도 만들기
install.packages("mapproj")
install.packages("ggiraghExtra")
library(ggiraghExtra)

str(USArrests)
head(USArrests)

library(tibble)
crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)
str(crime) #새 데이터 프레임 만들기

install.packages("maps")
library(ggplot2)
states_map <- map_data("state")
str(states_map) #미국 주 지도 데이터 준비하기

ggChoropleth(data = crime, #지도에 표현할 데이터
             aes(fill = Murder, #색깔로 표현할 변수
                 map_id = state), #지역 기준 변수
             map = states_map) #지도 데이터 #단계 구분도 만들기
ggChoropleth(data = crime, #지도에 표현할 데이터
             aes(fill = Murder, #색깔로 표현할 변수
                 map_id = state), #지역 기준 변수
             map = states_map, #지도 데이터
             interactive = T) #인터랙티브 #인터랙티브 단계 구분도 만들기







#2. 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기
install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardi0moon/kormaps2014")
library(kormaps2014)

str(korpop1) #대한민국 시도별 인구 데이터 준비하기
library(dplyr)
korpop1 <- rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동) #변수명 영문자로 수정
str(kormap1) #대한민국 시도 지도 데이터 준비하기

ggChoropleth(data = korpop1, #지도에 표현할 데이터
             aes(fill = pop, #색깔로 표현할 변수
                 map_id = code, #지역 기준 변수
                 tooltip = name), #지도위에 표시할 지역명
             map = kormap1, #지도 데이터
             interactive = T) #인터랙티브 #단계 구분도 만들기

str(tbc) #지역별 결핵 환자 수
ggChoropleth(data = tbc, #지도에 표현할 데이터
             aes(fill = NewPts, #색깔로 표현할 변수
                 map_id = code, #지역 기준 변수
                 tooltip = name), #지도위에 표시할 지역명
             map = kormap1, #지도 데이터
             interactive = T) #인터랙티브 #단계 구분도 만들기