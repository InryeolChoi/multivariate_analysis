# <다변량분석 (5) - 요인분석 (1)>
# 기본설정
library(MASS)
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