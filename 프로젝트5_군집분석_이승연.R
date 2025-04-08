setwd("L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2\\데이터\\ml-1m")
# data: https://grouplens.org/datasets/movielens/1m/

# 데이터 불러오기 및 결측치 확인 -------------------------------------------------------

movlens = read.csv("movlens.csv")
library(psych)
movlens <- na.omit(movlens) # 결측치 삭제
dim(movlens) # [1] 422204     27
sum(is.na(movlens)) # 결측치 삭제 확인




# 데이터 전처리 -----------------------------------------------------------------
#         -----------------------------------------------------------------------

# 사용할 변수:
# Action,Adventure,Animation,...,Thriller,War(One Hot Encoding한 Genre 변수들)/n : 정규화된 Genre 변수



# 변수: Genre --------------------------------------------------------------

library(tidyverse)
## UserID 기준 장르 별 합산
Genre_sum <- movlens %>%
  select(UserID, Action:War) %>% 
  group_by(UserID) %>% 
  summarise(across(everything(), sum))


# UserID별 영화 평가 수 (도수) 계산 
user_count <- count(movlens, UserID)



# 각 장르 변수들을 UserID별 영화 평가 수(n)로 나누기
GenRat_normalized <- Genre_sum %>%
  left_join(user_count, by = "UserID") %>%
  mutate(across(Action:War, ~ .x / n))%>%
  relocate(n, .before = Action)


# 변수: Rating_mean ---------------------------------------------------------

# UserID별 평점 평균
Rating_mean <- movlens %>% group_by(UserID) %>%  summarize(Rating_mean = mean(Rating, na.rm = TRUE))

# Rating_mean 데이터 병합
GenRat_final <- merge(GenRat_normalized, Rating_mean, by = "UserID")
head(GenRat_final)

genre <- GenRat_final[,3:19]
str(genre)


# 0인 값을 가지고 있는 행 삭제
rows_with_zero <- which(apply(genre, 1, function(row) any(row == 0)))
genre <- genre[-rows_with_zero, ]


str(genre)

write.csv(genre,"L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2\\데이터\\genre.csv")
genre <- read.csv("L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2\\데이터\\genre.csv")



# 군집분석  -----------------------------------------------------------------------



library(proxy)
# 거리행렬 생성
gen.D = genre # 변수 선택
gen.Z = scale(gen.D, center = TRUE, scale = TRUE) # 수치형이기에 표준화
rownames(gen.D) = genre$subject # 변수 이름 지정
head(gen.D) # 거리행렬 확인



##### 유클리드 거리를 이용하여 군집분석 #####

(gen.e.dist = dist(gen.Z, method = "euclidean", diag = TRUE, upper = FALSE,
                   by_rows = FALSE))
# 최단 연결법
par(mfrow = c(1,1))
(gen.hclust.e.s = hclust(gen.e.dist, method = "single")) # 군집분석
plot(gen.hclust.e.s, main = "최단연결법") # 덴드로그램
rect.hclust(gen.hclust.e.s, k = 7, border = "red")

# 최장 연결법
(gen.hclust.e.c = hclust(gen.e.dist, method = "complete")) # 군집분석
plot(gen.hclust.e.c, main = "최장연결법") # 덴드로그램
rect.hclust(gen.hclust.e.c, k = 7, border = "red")

# 중심 연결법
(gen.hclust.e.ce = hclust(gen.e.dist, method = "centroid")) # 군집분석
plot(gen.hclust.e.ce, main = "중심연결법") # 덴드로그램
rect.hclust(gen.hclust.e.ce, k = 7, border = "red")

# 중위수 연결법
(gen.hclust.e.m = hclust(gen.e.dist, method = "median")) # 군집분석
plot(gen.hclust.e.m, main = "중위수연결법") # 덴드로그램
rect.hclust(gen.hclust.e.m, k = 7, border = "red")


# 평균 연결법
(gen.hclust.e.a = hclust(gen.e.dist, method = "average")) # 군집분석
plot(gen.hclust.e.a, main = "평균연결법") # 덴드로그램
rect.hclust(gen.hclust.e.a, k = 7, border = "red")


# Ward의 방법 
(gen.hclust.e.w = hclust(gen.e.dist, method = "ward.D")) # 군집분석
plot(gen.hclust.e.w, main = "Ward 방법") # 덴드로그램
rect.hclust(gen.hclust.e.w, k = 7, border = "red")


# 계층적 군집화 모델을 사용하여 7개의 군집으로 데이터를 자릅니다
gen.clusters <- cutree(gen.hclust.e.w, k = 7)

# 군집 레이블을 원본 데이터프레임에 추가합니다
gen.D$Cluster <- gen.clusters

# 결과 확인
head(gen.D)

# 확인: gen.clusters의 길이가 gen.D의 행 수와 일치하는지 확인
print(length(gen.clusters))
print(nrow(gen.D))


# 최장연결법, 평균연결법, Ward의 방법이 적합해보인다.
# 최단연결법, 중심연결법, 평균연결법은 

# 군집 개수 결정 ----------------------------------------------------------------


library(factoextra)

# factoextra::fvis_nbclust 
fviz_nbclust(gen.Z, kmeans, method = "wss", k.max = 10) # 군집 선택하기 어려워보인다.
fviz_nbclust(gen.Z, kmeans, method = "silhouette", k.max = 10)# 2개가 적절해보인다.
fviz_nbclust(gen.Z, kmeans, method = "gap_stat", nboot = 500) # 5개가 적절해보인다. 


library(NbClust)

# NbClust
(gennb.e = NbClust(data = genre, distance = "euclidean", method = "kmeans",min.nc = 2, max.nc = 10))

# -------------------------------------------------------------------------


# k-평균 군집분석
# K = 7로 설정
gen.kmean.e = kmeans(genre, centers = 7, nstart = 500)
gen.kmean.e$iter # 반복횟수
gen.kmean.e$size # 군집별 개체수
gen.kmean.e$centers # 군집별 중심(평균벡터)출력


# 군집 번호
gen.cluster.e = gen.kmean.e$cluster
(gen.clust.e = data.frame(genre,gen.cluster.e))

# 군집화 결과 그래프 표현
library(cluster)
clusplot(gen.Z, clus =  gen.cluster.e,labels = 5,lines =0,color = TRUE, shade = TRUE, cex = 1.5)




