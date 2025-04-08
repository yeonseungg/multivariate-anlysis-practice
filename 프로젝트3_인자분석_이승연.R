setwd("L:\\SeungYeon\\학교자료\\2023\\다변량자료분석2\\데이터\\ml-1m")
# data: https://grouplens.org/datasets/movielens/1m/

# 데이터 불러오기 및 결측치 확인 -------------------------------------------------------

GenRatsam = read.csv("GenRatsam.csv", header = T)
library(psych) 
head(GenRatsam)
GenRatsam <- GenRatsam[,-1] # 필요없는 변수 제거
str(GenRatsam)


# 사용할 변수:
# UserID
# n : UserID별 영화 평가 수 (도수)
# Action,Adventure,Animation,...,Thriller,War(One Hot Encoding한 Genre 변수들)/n : 정규화
# Rating_mean: UserID 별 평점의 평균
# Gender
# Age
# Occuplation




# FA ---------------------------------------------------------------------
#   -----------------------------------------------------------------------


# 상관행렬 -------------------------------------------------------------------
genre <-  GenRatsam[,3:19] # 장르변수와 평점평균만을 추출
str(genre)
dim(genre)
cor.genre <- cor(genre) # 상관행렬 출력
cor.plot(genre) # 상관행렬 그래프
# Action과 Adventure 장르는 양의 상관관계를 가지는 것으로 보이며,
# Animation과 Children.S 장르 또한 양의 상관관계를 갖는 것으로 보인다.
# Comedy 장르는 Sci.Fi, Thriller 장르와 음의 상관관계를 갖는 것으로 보인다. 


# KMO
KMO(genre)
# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = genre)
# Overall MSA =  0.5
# MSA for each item = 
#   Action   Adventure   Animation  Children.s      Comedy       Crime Documentary       Drama 
# 0.57        0.63        0.56        0.71        0.47        0.46        0.10        0.42 
# Fantasy   Film.Noir      Horror     Musical     Mystery     Romance      Sci.Fi    Thriller 
# 0.40        0.46        0.30        0.67        0.64        0.63        0.66        0.34 
# War 
# 0.42

# 0.5 이상인 변수만 추출(Action,Adventure,Animation,Children.s,Musical,Mystery,Romance,Sci.Fi)

genre.x <- genre[c("Action","Adventure","Animation","Children.s","Musical","Mystery","Romance","Sci.Fi")]
head(genre.x) 
cor.genre <- cor(genre.x)
cor.genre


# Bartlett의 구형성 검정
cortest.bartlett(cor.genre, n = nrow(genre.x))

# $chisq
# [1] 137.3873

# $p.value
# [1] 2.196504e-16   H0: 공통인자가 존재하지 않는다. 귀무가설 기각. 

# $df
# [1] 28



# -------------------------------------------------------------------------
# 고유값 계산
eigenvalues <- eigen(cor.genre)

# Scree plot 그리기
plot(eigenvalues$values, type="b", main="Scree Plot", xlab="Number of Factors", ylab="Eigenvalue", pch=19, ylim=c(0, max(eigenvalues$values)))
abline(h=1, col="red", lty=2)

# Scree plot결과에 따라서 2개의 인자로 결정. 


# -------------------------------------------------------------------------
# 인자분석


# 주성분분석법
fa_none_pca <- principal(genre.x, cor = "cor", nfactors=2, rotate="none")   
print(fa_none_pca)


# 주축인자법
fa_none_pa <- fa(r= cor.genre, nfactors=2, fm ="pa" , rotate="none")   
print(fa_none_pa)


# 최대우도법
fa_none_ml <- fa(r= cor.genre, nfactors=2, fm ="ml" , rotate="none")   
print(fa_none_ml)

 
# 분산
# 주성분분석법(PC): PC1이 전체 분산 중에서 0.35를 설명하며, PC2는 0.30를 설명한다. 누적 분산은 0.66이다.
# 주축인자법 (PA): PA1이 전체 분산 중 0.33을 설명하고, PA2는 0.27를 설명한다. 누적 분산은 0.60이다.
# 최대우도법 (ML): ML1이 0.31의 분산을 설명하며, ML2는 0.29를 설명한다. 누적 분산은 0.59이다.

# 적합도 (Fit)
# 주성분분석법: RMSR은 0.09이며, off diagonal values 기반의 적합도는 0.94이다.
# 주축인자법: RMSR은 0.04이며, off diagonal values 기반의 적합도는 0.99이다.
# 최대우도법: RMSR은 0.05이며, off diagonal values 기반의 적합도는 0.98이다.
# 세 방법 모두 유사한 결과를 제시하지만 주축인자법의 결과가 좀 더 우수한 적합도를 보인다.



# 주축인자법

# PA1
# Animation (0.77), Children's (0.79), Musical (0.54):첫번째 인자는 아동 및 가족 친화적인 장르로 의미 부여할 수 있다.
# Animation, Children's,Musical 장르는 주로 아이들과 가족을 대상으로 하는 요소들이 포함될 가능성이 높다.
# Action (-0.73), Sci.Fi (-0.58):첫번째 인자에 음의 방향으로 적재되어 있으며,
# Action, Sci.Fi 장르는 아동 및 가족 친화적이지 않은 어른이 좋아할 요소들이 포함할 가능성이 높다.

# PA2
# Adventure (0.81), Children's(0.60), Animation(0.56), :두번째 인자는 밝은 분위기의 장르로 의미 부여할 수 있다.
# Adventure, Children's, Animation 장르는 모험적이고 밝은 분위기의 요소를 포함한다.
# Romance (-0.39), Mystery (-0.25): Romance, Mystery 장르는 두번째 인자에 음의 방향으로 적재되어 있으며,
# 미스터리하면서도 밝은 분위기의 요소가 적을 가능성이 높다.

# -------------------------------------------------------------------------
# 인자분석 / 인자 적재 플롯
# 인자분석 방법은 주축인자법을 사용, 인자 회전에 따른 차이 확인 

par(mfrow = c(1,3))
# 회전 = none
fa_none <- fa(r= cor.genre, nfactors=2, fm = "pa", rotate="none")   
print(fa_none)
biplot(x = fa_none$loadings[,c(1,2)], y = fa_none$loadings[,c(1,2)],
       xlabs = colnames(cor.genre), ylabs = colnames(cor.genre), main = "rotate = none")
abline(h=0, v=0, col="red", lty=2)



# 회전 = varimax
fa_varimax <- fa(r= cor.genre, nfactors=2, fm = "pa", rotate="varimax")   
print(fa_varimax)
biplot(x = fa_varimax$loadings[,c(1,2)], y = fa_varimax$loadings[,c(1,2)],
       xlabs = colnames(cor.genre), ylabs = colnames(cor.genre), main = "rotate = varimax")
abline(h=0, v=0, col="red", lty=2)


# 회전 = promax
fa_promax <- fa(r= cor.genre, nfactors=2, fm = "pa", rotate="promax")   
print(fa_promax)
biplot(x = fa_promax$loadings[,c(1,2)], y = fa_promax$loadings[,c(1,2)],
       xlabs = colnames(cor.genre), ylabs = colnames(cor.genre),main = "rotate = promax")
abline(h=0, v=0, col="red", lty=2)




# 주성분분석  -----------------------------------------------------------------------

GenRatsam.prcomp = prcomp(genre.x ,center = TRUE, scale = TRUE) 

GenRatsam.prcomp
GenRatsam.prcomp$sdev # 주성분의 표준편차
GenRatsam.prcomp$sdev^2 # 고유값 출력
summary(GenRatsam.prcomp)# 설명분산 요약


# 주성분분석 biplot
biplot(GenRatsam.prcomp, main = "주성분분석")
abline(h=0, v=0, lty=2)


# 인자분석 biplot
biplot(x = fa_none$loadings[,c(1,2)], y = fa_none$loadings[,c(1,2)],
       xlabs = colnames(cor.genre), ylabs = colnames(cor.genre), main = "인자분석")
abline(h=0, v=0, col="red", lty=2)

