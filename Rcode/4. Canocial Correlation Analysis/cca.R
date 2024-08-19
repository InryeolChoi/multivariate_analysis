# 정준분석
rm(list=ls())

# 1. intro ----
library(MASS)
library(Matrix)
pp = 3
set.seed(2022)

## 상관행렬 R1, R2
Sigma = matrix(rWishart(1, pp, diag(pp)), pp)
R1 = diag(1 / sqrt(diag(Sigma))) %*% Sigma %*% diag(1 / sqrt(diag(Sigma)))
Sigma = matrix(rWishart(1, pp, diag(pp)), pp)
R2 = diag(1 / sqrt(diag(Sigma))) %*% Sigma %*% diag(1 / sqrt(diag(Sigma)))
round(R1, 2)
round(R2, 2)

## 자료 뽑기 & 상관성 확인
nn = 10000
ss = bdiag(R1, R2) # 자료 합치기
pp2 = dim(ss)[1]
mu = rep(0, pp2)
d1 = mvrnorm(nn, mu, ss)
round(cor(d1), 2) # x와 y의 상관성이 없다.

## 행과 열을 교체한 새 자료
d1 = d1[, as.numeric(matrix(1:pp2, 2, byrow = T))]
d1 = data.frame(d1) # 인위적인 변수 조정
names(d1) = paste('X', 1 : pp2, sep='')
round(cor(d1), 2)

# update.packages(ask=F)
library(CCA)

# 행렬 간 상관관계
X = d1[, 1:3]
Y = d1[, -c(1:3)]
print(matcor(X, Y), digits=2)

# 정준상관분석 : cc
d2 = cc(X, Y)
str(d2)

# X에 대한 정준변수점수
score = d2$scores$xscores
round(apply(score, 2, mean), 2)
round(apply(score, 2, sd), 2)
round(cov(score), 2)

# 2. 응용하기----
## 적정한 정준변수쌍 적정 수 산정
# install.packages("CCP")
library(CCP)
p.asym(d2$cor, dim(X)[1], dim(X)[2], dim(Y)[2], tstat='Wilks')

## 공헌도
w = (d2$cor)^2; w
## 근사도
cumsum(w) / sum(w) * 100


# 3. 원리 소개 -----
# 손으로 구하기
# 재작성
ss = cov(d1)
Sxx = ss[1:3, 1:3]
Sxy = ss[1:3, 4:6]
Syy = ss[4:6, 4:6]

# Sigma의 -1/2승 구하기
tmp = eigen(Sxx)
Sxx2 = tmp$vectors %*% diag(sqrt(tmp$values)) %*% t(tmp$vectors)
tmp = eigen(Syy)
Syy2 = tmp$vectors %*% diag(sqrt(tmp$values)) %*% t(tmp$vectors)

# 연산1
tmp = solve(Sxx2) %*% Sxy %*% solve(Syy) %*% t(Sxy) %*% solve(Sxx2); tmp
tmp = eigen(tmp)
P = tmp$vectors
u = solve(Sxx2) %*% P

# 연산2
tmp = solve(Syy2) %*% t(Sxy) %*% solve(Sxx) %*% Sxy %*% solve(Syy2); tmp
tmp = eigen(tmp)
Q = tmp$vectors
v = solve(Syy2) %*% Q

# 라이브러리와 비교
round(d2$xcoef, 4); round(u, 4)
round(d2$ycoef, 4); round(v, 4)

