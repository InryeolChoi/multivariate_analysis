# <다변량분석 (5) - 요인분석(2)>
rm(list=ls())
# 자료 만들기 ------------------------------------------------------------------
## 위샤트분포로 공분산행렬 제작
library(MASS)
pp = 8
set.seed(2022)
Sigma = matrix(rWishart(1, pp, diag(pp)), pp)

## 표본상관행렬
## Sigma를 표준편차로 나눈 것으로 해석
R = diag(1 / sqrt(diag(Sigma))) %*% Sigma %*% diag(1/sqrt(diag(Sigma)))

## 공분산행렬 Sigma를 확인
round(Sigma, 2)

## 양정치행렬 확인
if (det(Sigma) > 0) paste("positive-definite") else paste("not positive-definite")

## 상관행렬을 확인
round(R, 2)

## 자료 만들기
n = 10000
mu = rep(0, pp)
set.seed(2022)
d1 = mvrnorm(n, mu, Sigma)
d1 = data.frame(d1)
names(d1) = paste('X', 1:pp, sep='')


# 요인분석 예시 (1) -----------------------------------------------------------------
## 공통요인 3개 : x7이 설명이 잘 안된다.
mm = 3
d1.fa = factanal(scale(d1), mm); d1.fa

## 공통요인 4개 : x7이 설명이 잘 안된다.
mm = 4
d1.fa = factanal(scale(d1), mm); d1.fa

## 공통요인 5개 : 에러가 발생한다. (특정분산행렬의 구조가 무너짐)
mm = 5
d1.fa = factanal(scale(d1), mm) 

# 요인분석 예시 (2) -----------------------------------------------------------------
## 공통요인 3개 + 6, 7번째 변수 빼기
mm = 3
d1.fa = factanal(d1[, -c(6,7)], mm); d1.fa

## 공통요인 3개 + 7번째 변수 빼기 => x6가 잘 설명이 안된다.
mm = 3
d1.fa = factanal(d1[, -7], mm); d1.fa

## 공통요인 3개 + 7번째 변수 빼기 => 에러 발생
mm = 4
d1.fa = factanal(d1[, -7], mm); d1.fa

# uniqueness 구하기 ----------------------------------------------------------------------------------
mm = 3
d1.fa = factanal(scale(d1), mm)

## 패키지의 uniqueness
unique1 = round(d1.fa$uniquenesses, 3)

## 직접 구한 uniqueness
d1.fa$loadings
round(d1.fa$loadings[1:pp], 3)
unique2 = round(1 - apply(d1.fa$loadings^2, 1, sum), 3)

## 둘은 서로 같은가?
if (all(unique1 == unique2)) paste("같다") else paste("다르다")

# Propotion Var 구하기----------------------------------------------------------------------------------

## 패키지의 Propotion Var
d1.fa$loadings

## 직접 구한  Propotion Var
ssl = apply(d1.fa$loadings^2, 2, sum)
ssl = round(ssl, 3)
round(ssl / (sum(ssl) + sum(d1.fa$uniquenesses)), 3)

## 공통인자가 늘어나면? Propotion Var가 늘어난다.
## 그렇다면, 공통인자를 늘리는게 좋을까?
mm = 4
d1.fa = factanal(scale(d1), mm)
d1.fa$loadings
d1.fa$uniquenesses

## tip : 생활의 지혜
d1.fa$loadings # 행렬 2개 도출
d1.fa$loadings[1:3, ] # 헹렬 1개만 도출. 수치화 가능

# 잔차행렬 ---------------------------------------------------------
## 원래의 표본상관행렬
mat1 = round(cor(d1), 2)

## 공통인자로 추정한 표본상관행렬
pp = 8; mm = 3
D1 = d1.fa$loadings[1:pp, 1:mm]
P1 = diag(d1.fa$uniquenesses)
mat2 = round(D1 %*% t(D1) + P1, 2)

## 둘의 차이 보기 : 약간의 차이 존재
mat1;mat2

## 둘의 차를 '잔차행렬' 이라고 함.
mat1 = cor(d1)
residual_matrix = round((mat1 - mat2), 2)
residual_matrix

round((diag(t(D1) %*% D1) / sum(diag(D1 %*% t(D1) + P1))), 3)

# 인자적재행렬을 이용한 biplot  ---------------------------------------------------------
round(D1, 2)
par(mfrow=c(1, 1))
biplot(x = D1, y = D1)
