#1. 산점도
library(ggplot2)

ggplot(data = mpg, aes(x = displ, y = hwy)) # x축은 displ, y축은 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y = hwy))+ geom_point() #산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3,6) +
  ylim(10, 30) #x축 범위 3-6, y축 범위 10-30으로 지정





#2. 막대 그래프
library(dplyr)
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) +geom_col() #그래프 생성
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) +geom_col() #크기 순으로 정렬

ggplot(data = mpg, aes(x = drv)) + geom_bar() #빈도 막대 그래프
ggplot(data = mpg, aes(x = hwy)) + geom_bar()





#3. 선 그래프
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()





#4. 상자 그림
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()
