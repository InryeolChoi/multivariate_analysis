# <다변량분석 (5) - 요인분석 (1)>
# 기본설정
library(MASS)
rm(list=ls())
set.seed(2023)

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
mm = 10
cortest.bartlett(R, mm)

## 구형성 검정 예시
set.seed(42)
x <- matrix(rnorm(1000),ncol=10)
r <- cor(x)
cortest.bartlett(r)

