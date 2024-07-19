# <다변량분석 (4) - 주성분분석(2)>
rm(list=ls())
# 데이터 제작 ----------------------------------------------------------------
library(MASS)
D = diag(c(1, 2, 0.4, 0.5))
data = c(1, 0.6, 0.1, 0.3,
         0.6, 1, 0.2, 0.4,
         0.1, 0.2, 1, 0.5,
         0.3, 0.4, 0.5, 1)
rr <- matrix(data, nrow=4, byrow=TRUE)
Sigma = D^{1/2} %*% rr %*% D^{1/2}
mu = rep(0, 4)
n = 100
d1 = mvrnorm(n, mu, Sigma)
d2 = d1
d1 = scale(d1)
eeR = eigen(cov(d1))

# 주성분분석 패키지 - prcomp ----------------------------------------------------------------
d1.pc = prcomp(d1); d1.pc
par(mfrow=c(1,1))
plot(d1.pc)
summary(d1.pc)

## prcomp 객체의 구조
str(d1.pc)
d1.pc$sdev   # 표준편차
d1.pc$center # 평균값
d1.pc$scale  # 표준화 여부

## 표준편차 = 고유값의 제곱근인지 확인
d1.pc$sdev # 표준편차
sqrt(eigen(cor(d1))$values) # 고유값의 제곱근

## 자료의 축소 : 설명력이 작은 고유벡터 제거
### 고유값들을 tmp에 담고, 그것들을 반올림한 것을 tmp2에 담는다.
tmp = eigen(cor(d1))$vectors
tmp2 = round(tmp,3); tmp2
### 고유값이 0.3보다 작으면 (= 설명력이 낮으면) 제거
tmp3 = matrix(as.character(tmp2), 4)
tmp3[abs(tmp2 < 0.3)] = ''
as.data.frame(tmp3)
## 표준편차
apply(d1, 2, sd)

## biplot 그리기
par(mfrow=c(1,2))
biplot(d1.pc, pch=19, cex=0.6, scale=T, lwd=3) # 표준화 o -> 원
biplot(d1.pc, pch=19, cex=0.6, scale=F, lwd=3) # 표준화 x -> 타원

# 주성분분석 패키지 - princomp  ----------------------------------------------------------------
d1.pc2 = princomp(d1)
str(d1.pc2)
## 
loadings(d1.pc2)

## 두 함수의 비교
d1.pc
d1.pc2

## biplot 그리기
biplot(d1.pc2, pch=19, cex=0.6, scale=F, lwd=3)

# 데이터제작 : 두 개의 모집단  ----------------------------------------------------------------
library(MASS)
data = c(1, 0.6, 0.1, -0.1,
         0.6, 1, 0.2, -0.2,
         0.1, 0.2, 1, 0.7,
         -0.1, 0.2, 0.7, 1)
rr = matrix(data, nrow = 4, byrow=TRUE)
D = diag(c(1, 2, 0.4, 0.2))
Sigma = D^{1/2} %*% rr %*% D^{1/2}
nn = 10000
mm = 20
mu = rep(0, dim(rr)[1])
# mu2 = rep(7, dim(rr)[1])
mu2 = c(7, 0, 1, 2)
d1 = rbind(mvrnorm(nn - mm, mu, Sigma), mvrnorm(mm, mu2, Sigma))
d1 = scale(d1)
class(d1)
R = cov(d1)
eeR = eigen(R)
cumsum(eeR$values) / sum(eeR$values) * 100
round(eeR$vectors, 2)

## biplot으로 이상치 찾기
P = data.frame(d1 %*% eeR$vectors[, 1:2])
plot(P$X1, P$X2, pch=19, col="grey", cex=0.2,
     xlab="1st pc", ylab="2nd pc", bty="l")
