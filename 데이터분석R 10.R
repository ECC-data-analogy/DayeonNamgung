#1. 힙합 가사 텍스트 마아닝
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary") #KoNLP 의존성 패키지 설치하기

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch")) #KoNLP 패키지 설치하기

download.file(
  url = "https://github.com/youngwoos/Doit_R/raw/master/Data/scala-library-2.11.8.jar",
  destfile = paste0(.libPaths()[1],"/KoNLP/Java/scala-library-2.11.8.jar")) #scala-library-2.11.8.jar 파일 다운로드하기

txt <- readLines("hiphop.txt") #형태소 사전 설정하기

install.packages("stringr")
library(stringr)
txt <- str_replace_all(txt, "\\W", " ") #특수 문자 제거하기

extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다") #명사 추출하기
noun <- extractNoun(txt) #가사에서 명사 추출
wordcount <- table(unlist(nouns)) #추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
df_word <- as.data.frame(wordcount, stringsAsFactors = F) #데이터 프레임으로 변환
df_word <- rename(df_word, 
                  word = Var1,
                  freq = Freq) #변수명 수정

df_word <- filter(df_word, nchar(word) >= 2) #두 글자 이상 단어 추출
top_20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20) #상위 20개 단어 추출

#워드 클라우드 만들기
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(8, "Dark2") #Dark2 색상 목록에서 8개 색상 추출
set.seed(1234) #난수 고정하기
wordcloud(word = df_word$word, #단어
          freq = df_word$freq, #빈도
          min.freq = 2, #최소 단어 빈도
          max.words = 200, #표현 단어 수
          random.order = F, #고빈도 단어 중앙 배치
          rot.per = .1, #회전 단어 비율
          scale = c(4, 0.3), #단어 크기 범위
          colors = pal) #색상 목록

pal <- brewer.pal(9, "Blues")[5:9] #색상 목록 생성
set.seed(1234) #난수 고정
wordcloud(word = df_word$word, #단어
          freq = df_word$freq, #빈도
          min.freq = 2, #최소 단어 빈도
          max.words = 200, #표현 단어 수
          random.order = F, #고빈도 단어 중앙 배치
          rot.per = .1, #회전 단어 비율
          scale = c(4, 0.3), #단어 크기 범위
          colors = pal) #색상 목록






#2. 국정원 트윗 텍스트 마이닝
twitter <- read.csv("twitter.csv")

twitter <- rename(twitter, 
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용) #변수명 수정
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ") #특수문자 제거

nouns <- extractNoun(twitter$tw) #트윗에서 명사추출
wordcount <- table(unlist(nouns)) #추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
df_word <- as.data.frame(wordcount) #데이터 프레임으로 변환
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq) #변수명 수정
df_word <- filter(df_word, nchar(word) >= 2) #두 글자 이상 단어만 추출
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20) #상위 20개 추출

#단어 빈도 막대 그래프 만들기
library(ggplot2)
order <- arrange(top20, freq)$word #빈도 순서 변수 생성
ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0, 2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) + #빈도순 막대 정렬
  goem_text(aes(label = freq), hjust = 0.1) #빈도 표시

#워드 클라우드 만들기
pal <- brewer.pal(9, "Blues")[5:9] #색상 목록 생성
set.seed(1234) #난수 고정하기
wordcloud(word = df_word$word, #단어
          freq = df_word$freq, #빈도
          min.freq = 10, #최소 단어 빈도
          max.words = 150, #표현 단어 수
          random.order = F, #고빈도 단어 중앙 배치
          rot.per = 0, #회전 단어 비율
          scale = c(6, 0.5), #단어 크기 범위
          colors = pal) #색상 목록
