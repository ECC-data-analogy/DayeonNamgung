# 1. R 내장 함수로 데이터 추출하기
exam <- read.csv("csv_exam.csv")

exam[] #조건 없이 전체 데이터 출력
exam[1,] #1행 추출
exam[2,] #2행 추출

exam[exam$class == 1,] #class가 1인 행 추출
exam[exam$math >= 80,] #수학 점수가 80점 이상인 행 추출

exam[exma$class == 1 & exam$math >= 50,] #1반이면서 수학 점수가 50점 이상
exam[exam$english ,90 | exam$science <50,] #영어 점수가 90점 미만이거나 과학 점수가 50점 미만

exam[,1] #첫 번째 열 추출
exam[,2] #두 번째 열 추출
exam[,3] #세 번째 열 추출

exam[,"class"] #class 변수 추출
exam["math"] #math 변수 추출

exam[,c("class", "math", "english")] #class, math, english 변수 추출

exam[1,3] #행, 변수 모두 인덱스
exam[5,"english"] #행 인덱스, 열 변수명
exam[exam$math >= 50, "english"] #행 부등호 조건, 열 변수명
exam[exam$math >= 50, c("english", "science")] #행 부등호 조건, 열 변수명

#dplyr과 내장 함수 차이
exam$tot <- (exam$math + exam$english + exam$science)/3
aggregate(data = exam[exam$math >= 50 & exam$english >=80,], tot-class, mean) #내장 함수 코드
exam %>%
  filter(math >= 50 & english >= 80) %>%
  mutate(tot = (math + english + science)/3) %>%
  group_by(class) %>%
  summarise(mean = mean(tot)) #dplyr 코드






#2. 변수 타입
#facotr 타입 변수
var1 <- factor(c(1,2,3,1,2)) #factor 변수 생성
var1+2 #연산 안됨
class(var1) #변수 타입 확인
levels(var1) #factor 변수의 구성 범주 확인
var2 <- factor(c("a", "a", "b", "c")) #문자로 된 facotr변수 생성
mean(var1) #mean은 numeric 변수만 적용 가능
var1 <- as.numeric(var1) #numeric 타입으로 변환






#3. 데이터 구조
#벡터
a <- 1
b <- "Hello"

#데이터 프레임임
x1 <- data.frame(var1 = c(1,2,3),
                 var2 = c("a", "b", "c"))

#메트릭스
x2 <- matrix(c(1:12), ncol =2) #1~12로 2열

#어레이
x3 <- array(1:20, dim = c(2,5,2)) #1~20으로 2행.5열.2차원

#리스트
x4 <- list(f1= a, #벡터
           f2= x1, #데이터 프레임
           f3= x2, #매트릭스
           f4= x3) #어레이

mpg <- gglot2::mpg
x <- boxplot(mpg$cty)
x$stats[,1] #요약 통계량 추출
x$stats[,1][3] #중앙값 추출
x$stats[,1][2] #사분위수 추출

