setwd("L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2\\데이터\\ml-1m")
# data: https://grouplens.org/datasets/movielens/1m/

# 데이터 불러오기 및 결측치 확인 -------------------------------------------------------

movlens = read.csv("movlens.csv")

library(psych)
str(movlens) # 데이터 구조 확인
dim(movlens) # [1] 422205     27

sum(is.na(movlens)) # 결측치 확인
which(rowSums(is.na(movlens)) > 0) # 422205 열에 결측치가 있음을 알 수 있다.
movlens <- na.omit(movlens) # 결측치 삭제
dim(movlens) # [1] 422204     27
sum(is.na(movlens)) # 결측치 삭제 확인



# 데이터 전처리 -----------------------------------------------------------------
#         -----------------------------------------------------------------------

# 사용할 변수:
# UserID
# n : UserID별 영화 평가 수 (도수)
# Action,Adventure,Animation,...,Thriller,War(One Hot Encoding한 Genre 변수들)/n : 정규화
# Rating_mean: UserID 별 평점의 평균
# Gender
# Age
# Occuplation


# 변수: Genre,n --------------------------------------------------------------

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



# Sampling ----------------------------------------------------------------
# 30개의 UserID 샘플링
# 데이터 크기가 크기에 샘플링을 진행했다.

set.seed(927)
sampling <- sample(nrow(GenRat_final), 30)
GenRatsam <- GenRat_final[sampling, ]
head(GenRatsam)




# 변수: Gender, Age, Occupation ---------------------------------------------
# 추가적인 분석 및 시각화 요소를 적용하기 위해서 Gender, Age, Occupation 변수 추가.
str(movlens)
 
add <-  movlens[c("UserID","Gender","Age","Occupation")]
add <- distinct(add)
GenRatsam <- merge(GenRatsam, add, by = "UserID")


# 최종 데이터
head(GenRatsam)
dim(GenRatsam)
str(GenRatsam)

write.csv(GenRatsam,"GenRatsam.csv")


# PCA ---------------------------------------------------------------------
#   -----------------------------------------------------------------------


library(psych)


# 상관행렬 -------------------------------------------------------------------
genre <-  GenRatsam[,3:20] # 장르변수와 평점평균만을 추출
cor(genre) # 상관행렬 출력
cor.plot(genre) # 상관행렬 그래프
# Action과 Adventure 장르는 양의 상관관계를 가지는 것으로 보이며,
# Animation과 Children.S 장르 또한 양의 상관관계를 갖는 것으로 보인다.
# Comedy 장르는 Sci.Fi, Thriller 장르와 음의 상관관계를 갖는 것으로 보인다. 


# 주성분분석 -------------------------------------------------------------------
# 장르변수와 평점 평균을 변수로 지정.
# 공분산행렬 scale = FALSE
# 상관행렬 scale = TRUE 
GenRatsam.prcomp = prcomp(~Action+Adventure+Animation+Children.s+Comedy+
                            Crime+Documentary+Drama+Fantasy+Film.Noir+
                            Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Rating_mean,
                          data = GenRatsam, center = TRUE, scale = TRUE) 

GenRatsam.prcomp
GenRatsam.prcomp$sdev # 주성분의 표준편차
GenRatsam.prcomp$sdev^2 # 고유값 출력
summary(GenRatsam.prcomp)# 설명분산 요약
# 첫번째 주성분으로는 0.2429의 설명력을, 두번째 주성분으로는 0.449의 누적설명력으로 
# 2개의 주성분은 데이터의 50%를 설명한다.
# 데이터를 임의로 30개로 선택했기에 데이터의 설명력이 높다면 지나치게 데이터를 잘 설명하는거다.
# 과적합(overfitting)의 가능성도 있기 때문.




# 보유 주성분 개수에 관한 판정 --------------------------------------------------------
# 스크리 도표(scree plot)

screeplot(GenRatsam.prcomp, type = "line")
abline(h = 1,col = 'red') 
# 경사가 급격하게 변하는 곳을 기준으로 주성분 개수를 2개로 선정했다. 



# 주성분 적재계수(loadings) ------------------------------------------------------

# 주성분 분석 결과에서 로딩값 추출
loadings_matrix <- GenRatsam.prcomp$rotation[, 1:2]

# 로딩값 출력
print(loadings_matrix)

# 주성분에 대한 의미부여
# PC1 : 연령대
# + : Action, Crime, Thriller
# - : Animation, Children.s, Comedy
# 아이들과 함께 보기 힘든든 영화라고 의미부여할 수 있다.


# PC2 : 영화의 전반적인 분위기
# + : Adventure
# - : Drama, Romance
# 드라마/ 로맨스 장르에 비해 잔잔하지 않은 분위기라고 의미부여할 수 있다. 


# 주성분 점수 ------------------------------------------------------------------

# 주성분점수 출력
GenRatsam.score <- cbind(GenRatsam, GenRatsam.prcomp$x[,1:2])
print(GenRatsam.score)

# 주성분점수 그래프

# 개체 번호별 그래프
plot(GenRatsam.score[c("PC1","PC2")],main = "개체 번호별 주성분 점수 그래프")
abline(h=0,v=0)
text(GenRatsam.score[c("PC1","PC2")], labels = rownames(GenRatsam.score), pos=2)

# PC1의 관점
GenRatsam.score[8,]
GenRatsam.score[30,]


# PC2의 관점
GenRatsam.score[21,]
GenRatsam.score[8,]


# 성별 그래프
plot(GenRatsam.score[c("PC1","PC2")],main = "성별 주성분 점수 그래프")
abline(h=0,v=0)
text(GenRatsam.score[c("PC1","PC2")], labels = GenRatsam$Gender, pos=2)


# 나이별 그래프
plot(GenRatsam.score[c("PC1","PC2")],main = "나이별 주성분 점수 그래프")
abline(h=0,v=0)
text(GenRatsam.score[c("PC1","PC2")], labels = GenRatsam$Age, pos=2)
# 25세부터 34세 연령을 가진 사람들이 군집을 이루고 있다는 것을 알 수 있다.


# 직업별 그래프
plot(GenRatsam.score[c("PC1","PC2")],main = "직업별 주성분 점수 그래프")
abline(h=0,v=0)
text(GenRatsam.score[c("PC1","PC2")], labels = GenRatsam$Occupation, pos=2)
# 무직인 사람들이 중앙에서서 군집을 이루고 있다는 것을 알 수 있다.




# 주성분분석 행렬도(Biplot) -------------------------------------------------------

biplot(GenRatsam.prcomp)
abline(h=0, v=0, lty=2)

# 8번 개체는 첫번째 주성분에 있어서 가장 낮은 음의 값을 가지며
# 두번째 주성분에 있어서 가장 높은 양의 값을 가진다.
# 8번 개체를 형성하는데 있어서 Animation, Children.S의 장르가 가장 많은 영향을 미쳤다.

# Chileren.S Animation 장르가 비슷한 것으로 보이며,
# Mystery, film.Noir 장르가 비슷한 것으로 보인다. 



