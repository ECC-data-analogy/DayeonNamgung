#1. t 검정- 두 집단의 평균 비교
mpg <-as.data.frame(ggplot2::mpg)
library(dplyr)

#compact 자동차와 suv 자동차의 도시연비 t검정
mpg_diff <- mpg %>%
  select(class, cty) %>%
  filter(class %in% c("compact", "suv"))

head(mpg_diff)
table(mpg_diff$class)

t.test(data = mpg_diff, cty ~ class, var.equal = T)

#일반 휘발유와 고급 휘발유의 도시연비 t검정
mpg_diff2 <- mpg %>%
  select(fl, cty) %>%
  filter(fl %in% c("r", "p")) #r:regular, p:premium

table(mpg_diff2)

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)






#2. 상관분석 - 두 변수의 관계성 분석
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

#상관행렬 히트맵 만들기
head(mtcars)

car_cor <- cor(mtcars) #상관행렬 생성
round(car_cor, 2) #소수점 셋째 자리에서 반올림해 출력

install.packages("corrplot")
library(corrplot)
corrplot(car_cor) #상관행렬 히트맵 생성

corrplot(car_cor, method = "number") #상관계수 표현

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(car_cor,
         method = "color", #색깔로 표현
         col = col(200), # 색상 200개 선정
         type = "lower", # 왼쪽 아래 행렬만 표시
         order = "hclust", # 유사한 상관계수끼리 군집화
         addCoef.col = "black", #상관계수 색깔
         tl.col = "black", #변수명 색깔
         tl.srt = 45, #변수명 45도 기울임
         diag = F) #대각 행렬 제외


