setwd("L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2")
# data: https://grouplens.org/datasets/movielens/1m/

# 데이터 불러오기 및 결측치 확인 -------------------------------------------------------

genre <- read.csv("genre.x.csv")
genre <- genre[,-1]

str(genre)# Action, Adventure, Animation, Children.s, Musical, Mystery, Romance, Sci.Fi


genre.X <- genre[,c("Adventure","Animation","Children.s","Musical")] # 가족 친화적 장르
genre.Y <- genre[,c("Action","Mystery","Romance","Sci.Fi")] # 일반 장르

# stat::cancor함수, CCA::cc함수, yacca::cca함수 
# 함수들의 입력자료로 자료행렬 자체를 사용하며 공분산행렬이나 상관행렬 등을 입력자료로 사용할 수는 없음.


library(CCA)
genrecor= matcor(genre.X, genre.Y)


#각 변수그룹의 상관행렬
genrecor$XYcor

# 정준상관계수 출력
genre.cc = cc(genre.X,genre.Y)
round(genre.cc$cor,digits = 3) 

# 첫번째 상관계수가 0.897으로 두 변수 그룹이 높은 상관관계를 가진다고 할 수 있다.
# 첫 번째 정준변량이 두 변수 그룹 간의 관계를 가장 잘 나타낸다

# 정준계수 출력
genre.cc$xcoef
genre.cc$ycoef
round(genre.cc$xcoef*sapply(genre.X,sd),digits = 4) # X집단의 표준화된 정준계수
round(genre.cc$ycoef*sapply(genre.Y,sd), digits = 4) # Y집단의 표준화된 정준계수

# 표준화된 정준계수로부터, X그룹의 첫 정준변량은 -0.9630으로 Adventure가 상대적으로 큰 영향을 주었고,
# Y그룹의 첫 정준변량은 Action이 상대적으로 많은 영향을 주었다.

 
# 정준점수: 각 관측치의 정준변량에서의 점수
genrexscore = genre.cc$scores$xscores # X 정준점수
colnames(genrexscore) = paste0(c("Adventure","Animation","Children.s","Musical")) # X그룹 이름 입력
genreyscore = genre.cc$scores$yscores # Y 정준점수
colnames(genreyscore) = paste0(c("Action","Mystery","Romance","Sci.Fi")) # Y그룹 이름 입력
genrescore = cbind(genrexscore,genreyscore) # 결합
round(genrescore, digits = 3) 




# 정준적재: 정준 변량과의 단순상관계수
genre.cc$xcoef*sapply(genre.X,sd)   # x-집단, 표준화된(standardized) 정준계수
genre.cc$ycoef*sapply(genre.Y,sd)   # y-집단, 표준화된(standardized) 정준계수

round(genre.cc$scores$corr.X.xscores, digits = 3)   # x-정준적재 
# 첫번째 정준 변량과 Adventure는 -0.894로 가장 높은 상관계수를 갖고 있다.
# Adventure가 첫 번째 정준변량에 큰 영향을 미친다
round(genre.cc$scores$corr.Y.yscores, digits = 3)   # y-정준적재 
# 첫번째 정준 변량과 Action은 -0.976로 가장 높은 상관계수를 갖고 있다.
# Action이 첫 번째 정준변량에 큰 영향을 미친다



# 정준적재 플롯

genre.loading = rbind(genre.cc$scores$corr.X.xscores,
                      genre.cc$scores$corr.Y.yscores)
plot(genre.loading[,1:2],pch=1,col="red",xlim=c(-1,1),ylim=c(-1,1), main = "정준적재플롯")
abline(v=0,h=0,lty=2)
text(genre.loading[,1:2],labels=rownames(genre.loading),pos=4,col="blue")
# 첫번째 정준변량
# X 그룹에서 'Adventure'와 Y 그룹에서 'Action'이 각각 첫 번째 정준변량에 대한
# 가장 큰 음의 정준적재 값을 가지며,이는 이 두 변수가 첫 번째 정준변량에 가장 큰 영향을 미친다는 것을 의미.
# Adventure와 Action은 이 그래프상에서 음의 방향으로 가장 멀리 떨어져 있다. 
# 이는 두 변수 그룹 간의 주요한 상관관계를 나타낸다.

# 두번째 정준변량 
# X 그룹에서 'Animation'은 두 번째 정준변량에 대한 가장 큰 음의 정준적재 값을 가진다.
# 반면, Y 그룹에서 'Mystery'는 두 번째 정준변량에 대한 가장 큰 양의 정준적재 값을 가진다.
# 이 두 변수는 두 번째 정준변량에 큰 영향을 미친다.

# 점들이 원점에 가까울수록 해당 변수가 정준변량에 큰 영향을 미치지 않는다는 것을 의미.
# 점들이 서로 가까울수록 그 변수들 간의 상관관계가 강하다는 것을 의미.
# 이 플롯에서는 대부분의 변수들이 첫 번째 정준변량에 대해 음의 방향으로 위치하고 있다. 
# 이는 두 변수 그룹 간의 주요한 상관관계가 음의 방향이라는 것을 나타낸다.


# 교차적재 : 어느 한 관찰 변수가 다른 집단 변수들을 대표하는 정준변수들에 의해 얼마나 설명되는가

round(genre.cc$scores$corr.X.yscores, digits = 3) # x-교차적재
round(genre.cc$scores$corr.Y.xscores, digits = 3) # y-교차적재

# x-교차적재
# Adventure는 첫 번째 정준변량과 -0.802로 큰 음의 관련성이 있다.

# y-교차적재
# Action과 Sci.Fi는 첫 번째 정준변량과 큰 음의 관련성이 있다.

# 공헌도 
yacca::cca(genre.X,genre.Y)$xcanvad # X-변수들의 변이에 대한 정준변량들의 공헌도 
# 첫 번째 정준변량은 약 26.5%의 변이를 설명.
yacca::cca(genre.X,genre.Y)$ycanvad # Y-변수들의 변이에 대한 정준변량들의 공헌도 
# 첫 번째 정준변량은 약 42%의 변이를 설명

# 두 변수집단 간의 독립성 검정
library(CCP)
rho = genre.cc$cor
N = nrow(genre)
p = ncol(genre.X)
q = ncol(genre.Y)
p.asym(rho, N, p, q, tstat = "Wilks")
# 첫번째 정준변량만이 두 집단 간의 유의한 관계를 가지는 것으로 보인다.






