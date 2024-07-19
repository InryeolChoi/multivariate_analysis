# <다변량분석 (5) - 요인분석 (1)>
# 기본설정
rm(list=ls())
set.seed(2023)

# 요인분석 - 패키지 없이 --------------------------------------------------------------
## 자료 만들기
D = diag(c(1, 2, 0.4, 2))
data = c(   1,  0.6, 0.1, -0.1,
          0.6,    1, 0.2, -0.2,
          0.1,  0.2,   1,  0.7,
         -0.1, -0.2, 0.7,  1)
rr = matrix(data, nrow = 4, byrow = T)
Sigma = D^{1/2} %*% rr %*% D^{1/2}

n = 10000
mu = rep(0, dim(rr)[1])
set.seed(2023)
d1 = scale(mvrnorm(n, mu, Sigma))

## 자료의 공분산행렬 R 도출
R = cov(d1); R
eeR = eigen(R); eeR
U = eeR$vectors
L = diag(eeR$values)

## R의 1st, 2nd 자료 = delta^2
m = 2
Um = U[, 1:m]
Lm = L[1:m, 1:m]
Rm = Um %*% Lm %*% t(Um); Rm
round(R - Rm, 3)

## R의 3rd, 4th 자료 = psi
Umr = U[, -(1:m)]
Lmr = L[-(1:m), -(1:m)]
Rmr = Umr %*% Lmr %*% t(Umr)
round(Rmr, 3)
diag(diag(Rmr))

## delta * delta ^ T 만들어보기
ddT = R - diag(diag(Rmr))

## 고유값 분해 후 다시 변수축소
eeR_2 = eigen(ddT) 
## 설명비율로 변수 고르기
l1 = eeR_2$values
cumsum(l1) / sum(l1)
## 2개를 고르는 것으로 보고 근사값 도출
U = eeR_2$vectors[, 1:2]
L = diag(eeR_2$values[1:2])
approx.dd.T = U %*% L %*% t(U)

## 원래의 R과 비교해보기
round(approx.dd.T + diag(diag(Rmr)), 3)
round(R, 3)

## psi와 delta 얻기
psi = diag(diag(Rmr))
Delta = U %*% sqrt(L)

## 
round(solve(t(D) %*% solve(psi) %*% D), 3)

## 첫번째 인자점수 (GLS로 도출)
f = solve(t(D) %*% solve(psi) %*% D) %*% t(D) %*% solve(psi) %*% d1[1,]
f

# 자료 만들기(2) ---------------------------------------------------------------
# 위샤트분포를 이용한 공분산행렬 생성
library(MASS)
pp = 10
set.seed(2022)
Sigma = matrix(rWishart(1, pp, diag(pp)), pp)
R = diag(1/sqrt(diag(Sigma))) %*% Sigma %*% diag(1/sqrt(diag(Sigma)))
# R; Sigma; det(Sigma)

## 자료행렬 생성
n = 10000
mu = rep(0, pp)
set.seed(2022)
d1 = mvrnorm(n, mu, Sigma)
df1 = data.frame(d1)
names(df1) = paste('X', 1:pp, sep='')

Sigma
cov(d1)
eeS = eigen(cor(d1))
round(cumsum(eeS$values / sum(eeS$values)) * 100, 2)

## 주성분분석
d1.pca = princomp(d1, cor=T)
summary(d1.pca)
## scree plot 
plot(1:10, d1.pca$sdev ^ 2)

## 인자분석
mm = 4
d1.fa = factanal(d1, mm)
loadings(d1.fa)

round(cor(d1), 2)
D1 = d1.fa$loadings[1:pp, 1:mm]
P1 = diag(d1.fa$uniquenesses)
round(D1 %*% t(D1) + P1, 2)

# 인자분석의 필요성 체크 --------------------------------------------------------------------
## 1. kmo 지수
# install.packages("psych")
library(psych)
KMO(d1)

## kmo 지수의 원리
s1 = diag(sqrt(1 / diag(solve(R))))
q1 = s1 %*% solve(R) %*% s1
sum_r2 = sum((R - diag(diag(R))) ^ 2)
sum_q2 = sum((q1 - diag(diag(q1))) ^ 2)
kmo_value = sum_r2 / (sum_r2 + sum_q2)
kmo_value

## 2. 구형성 검정
cortest.bartlett(R, mm)

## 구형성 검정 예시
set.seed(42)
x <- matrix(rnorm(1000),ncol=10)
r <- cor(x)
cortest.bartlett(r)
