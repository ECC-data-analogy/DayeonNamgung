#1. plotly 패키지로 인터랙티브 그래프 만들기
install.packages("plotly")
library(plotly)

library(ggplot2)
p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) + geom_point() #ggplot2로 그래프 만들기
ggplotly(p) #인터랙티브 그래프 만들기

p <- ggplot(data = dismonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplotly(p) #인터랙티브 막대 그래프 만들기






#2. dygraphs 패키지로 인터래티브 시계열 그래프 만들기
install.packages("dygraphs")
library(dygraghs)

economics <- ggplot2::economics
head(economics) #economics데이터 불러오기

library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco) #xts타입으로 변경

dygraph(eco) #인터랙디브 시계열 그래프 생성

dygraph(eco) %>% dyRangeSelector() #날짜 범위 선택 기능

eco_a <- xts(economics$psavert, order.by = economics$date) #저축률
eco_b <- xts(economics$unemploy/1000, order.by = economics$date) #실업자 수
eco2 <- cbind(eco_a, eco_b) #데이터 결합
colnames(eco2) <- c("psavert", "unemploy") #변수명 바꾸기
head(eco2)
dygraph(eco2) %>% dyRangeSelector() #그래프 만들기