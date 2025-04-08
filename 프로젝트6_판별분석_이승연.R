# setwd("L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2\\데이터\\ml-1m")
# data: https://grouplens.org/datasets/movielens/1m/


genre <- read.csv("L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2\\데이터\\genre.csv")
library(proxy)


#  이상치 제거  -----------------------------------------------------------------------

  
library(dplyr)

# 이상치를 제거하는 함수를 정의합니다.
remove_outliers_3IQR <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25)
  Q3 <- quantile(df[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% 
    filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
}

# 모든 장르 컬럼에 대해 이상치를 제거합니다.
cleaned_genre <- genre
for(column in names(genre)[-1]) { # 첫 번째 컬럼은 행 번호이므로 제외합니다.
  cleaned_genre <- remove_outliers_3IQR(cleaned_genre, column)
}

# 결과를 확인합니다.
print(cleaned_genre)
boxplot(cleaned_genre)

genre <- cleaned_genre
genre <- genre[,-1]
boxplot(genre)

#   -----------------------------------------------------------------------

# 거리행렬 생성
gen.D = genre 
gen.Z = scale(gen.D, center = TRUE, scale = TRUE) # 수치형이기에 표준화

rownames(gen.D) = genre$subject # 변수 이름 지정
head(gen.D) # 거리행렬 확인


(gen.e.dist = dist(gen.Z, method = "euclidean", diag = TRUE, upper = FALSE,
                   by_rows = TRUE))
# Ward의 방법 
(gen.hclust.e.w = hclust(gen.e.dist, method = "ward.D")) # 군집분석
plot(gen.hclust.e.w, main = "Ward 방법") # 덴드로그램
rect.hclust(gen.hclust.e.w, k = 6, border = "red")

# 계층적 군집화 모델을 사용하여 7개의 군집으로 데이터 자르기
gen.clusters <- cutree(gen.hclust.e.w, k = 6)

# 군집 레이블을 원본 데이터프레임에 추가
gen.D$Cluster <- gen.clusters
head(gen.D)

# 군집별 개체수  
table(gen.clusters)

# 표준화 자료의 평균
aggregate(gen.Z, by = list(gen.clusters), FUN = mean)

# 전처리 최종 변수: gen.D
# write.csv(gen.D, "genrecluster.csv")
dim(gen.D)
str(gen.D)

#   -----------------------------------------------------------------------
library(biotools)
# label 지정
gen.D.lab = gen.D$Cluster

# 나머지 변수 지정
gen.D.x = gen.D[,-18]

gen.D.boxM = boxM(gen.D.x,gen.D.lab)
print(gen.D.boxM)
print(gen.D.boxM$cov)
print(gen.D.boxM$pooled)

library(MASS)
# 모공분산행렬의 동일성에 대한 검정
# 귀무가설을 기각하기에 이차 판별분석 수행.
gen.qda = qda(Cluster ~ ., data = gen.D)
print(gen.qda)

pred.q <- predict(gen.qda, gen.D)
gen.pred.q <- cbind(gen.D, pred.q$posterior,pred.q$class)
gen.ctbl.q <- table(gen.D$Cluster, pred.q$class) # 정오분류표
gen.ctbl.q
library(DescTools)
Desc(gen.ctbl.q, digits = 2)



#   -----------------------------------------------------------------------

# 시각화

library(klaR)
gen.D$Cluster <- as.factor(gen.D$Cluster)
par(mar = c(5, 4, 4, 2) + 0.1) # 예를 들어, 마진 설정 조정
partimat(Cluster ~ ., data = gen.D, method = "qda")










