# <다변량분석 (3) - 주성분분석(1)>
rm(list=ls())
# 데이터 제작 ------------------------------------------------------------
library(MASS)
D = diag(c(1, 2, 0.4))
data = c(1, 0.6, 0.1, 0.6, 1, 0.2, 0.1, 0.2, 1)
rr = matrix(data, nrow=3, byrow=T)
Sigma = D^{1/2} %*% rr %*% D^{1/2}
mu = rep(0, 3)
n = 100
d1 = mvrnorm(n, mu, Sigma)
d2 = scale(d1)
## 자료확인
cov(d1); cor(d1)
## 표본상관행렬 (= 자료의 표준화)
cov(d2)
eeR = eigen(cov(d2))
str(eeR)

# 성분 구하기 ------------------------------------------------------------
## 성분의 분산 (=고유값 추출)
l1 = eeR$values; l1s
## 각 성분의 점수 계산
l1/sum(l1) * 100
## 각 성분의 누적 점수 계산
cumsum(l1/sum(l1) * 100)

## eeR 분석하기
U = eeR$vectors       # 고유벡터
L = diag(eeR$values)  # 고유값의 모임
### 원래의 공분산행렬과 같은지 비교
U %*% L %*% t(U)
cov(d2)
### 고유벡터가 정규직교기저를 만드는지 확인
round(U %*% t(U), 2)

# 주성분 점수 계산 ------------------------------------------------------------
## 주성분의 점수 (1)
Y = as.matrix(d2)
head(Y)
Y11 = matrix(Y[1, ], ncol = 1)
p11 = t(U[, 1]) %*% Y11; p11
## 또 다른 계산법
p11_b = t(U[, 1]) %*% Y[1, ]; p11_b

## 주성분의 점수 (2)
p1 = Y %*% U[, 1]
p1[1:3]
mean(p1); sd(p1)


# 주성분 점수의 시각화 ------------------------------------------------------------
# p1의 시각화
par(mfrow = c(1, 2))
hist(p1)
qqplot(qnorm(ppoints(length(p1)), sd = sqrt(L[1, 1])), p1,
       xlab = 'Normal dist', ylab = 'emphirical dist')
qqline(p1, distribution = function(p) qnorm(p, sd = sqrt(L[1, 1])))

## p1과 p2의 시각화 : 3d 그래프
p1 = Y %*% U[, 1]
p2 = Y %*% U[, 2]
require(KernSmooth)
par(mfrow = c(1, 2))
z1 = bkde2D(cbind(p1, p2), 1)
persp(z1$fhat, theta = 0, phi = 45)
z2 = bkde2D(Y[, 1:2], 1)
persp(z2$fhat, theta = 0, phi = 45)

## p1과 p2의 시각화 : biplot
par(mfrow = c(1, 1))
plot(p1, p2, cex = .5, pch = 19, col = "gray", 
     ylim = c(-4, 4), xlim = c(4, -4), bty = 'l')
abline(h = 0, v = 0, lty = 3)

# 시각화 B ------------------------------------------------------------
## 4x4 데이터 제작
D <- diag(c(1, 2, 0.4, 0.5))
data <- c(1, 0.6, 0.1, 0.3,
          0.6, 1, 0.2, 0.4,
          0.1, 0.2, 1, 0.5,
          0.3, 0.4, 0.5, 1)
rr <- matrix(data, nrow=4, byrow=TRUE)
Sigma = D^{1/2} %*% rr %*% D^{1/2}
mu = rep(0, 4)
n = 100
d3 = mvrnorm(n, mu, Sigma)
d4 = scale(d3)
eeR = eigen(cov(d4))

# 모든 주성분 점수 간 biplot
P = d3 %*% eeR$vectors[, 1:2]
P = data.frame(P)

display = function(d, x, y)
{
  t1 = paste(x, "번째 var")
  t2 = paste(y, "번째 var")
  plot(d[, x], d[, y], pch = 19, col='grey', 
       cex = 1, xlab = t1, ylab = t2, bty = 'l')
}

par(mfrow = c(2, 2))
display(d3, 1, 2)
display(d3, 1, 3)
display(d3, 3, 4)
plot(P$X1, P$X2, pch = 19, col = "grey", 
     cex = 1, xlab = "1st pc",
     ylab = "2nd pc", bty = 'l')

par(mfrow = c(2, 2))
plot(d3[, 1], d3[, 2], pch = 19, col = "grey", cex = 1, xlab = "1st var", ylab = "2nd var", bty = 'l')
plot(d3[, 1], d3[, 3], pch = 19, col = "grey", cex = 1, xlab = "1st var", ylab = "3rd var", bty = 'l')
plot(d3[, 3], d3[, 4], pch = 19, col = "grey", cex = 1, xlab = "3rd var", ylab = "4th var", bty = 'l')
plot(P$X1, P$X2, pch = 19, col = "grey", cex = 1, xlab = "1st pc", ylab = "2nd pc", bty = 'l')

# 3차원 시각화 : plotly 이용
library(plotly)
P = d3 %*% eeR$vectors[, 1:3]
P = data.frame(P)
plot_ly(x = P$X1, y = P$X2, z = P$X3, type = "scatter3d",
        width = 8, height = 9, mode = 'markers', 
        marker = list(size = 5))
for (i in 1:22) print("")

# 고유값의 합 = 4인지 확인 -------------------------------------------------
p = 4
sum(eeR$values > 0) == p
eeR$values
