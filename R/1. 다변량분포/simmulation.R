# <다변량분석 (2) - 시뮬레이션>
# 시뮬레이션 : 패키지 없이
## 이변량 데이터 : 패키지 없이
x = rnorm(100) * 2 + 5
y = rnorm(100) * 2 * sqrt(0.91) + 6 + 0.3 * (x-5)
w = cbind(x, y)
hist(w)

# 시뮬레이션 : 패키지 이용 (1)
## t분포 기반 : 자료 제작
n = 1000
set.seed(23)
x = rt(n, df=10)

## t분포 확인작업
qqplot(x, qnorm(ppoints(n), mean=0, sd=5), pch=19) # qnorm() = 정규분포의 cdf
qqline(x, distribution=function(p) qnorm(p, sd=5, mean=0), col='yellow', lwd=3)

## 정규분포 확인작업
qqplot(x, qt(ppoints(n), df=10), pch=19)
qqline(x, distribution=function(p) qt(p, df=10), col='yellow', lwd=3)


# 시뮬레이션 : 패키지 이용 (2)
## 중심행렬 제작
library(MASS)
D = diag(c(1, 2, 0.4))
data = c(1, 0.6, 0.1, 0.6, 1, 0.2, 0.1, 0.2, 1)
rr = matrix(data, nrow=3, byrow=T)
Sigma = D^{1/2} %*% rr %*% D^{1/2}
mu = rep(0, 3)
n = 1000

## 다변량자료 제작
d1 = mvrnorm(n, mu, Sigma)
class(d1); head(d1); dim(d1)

## 출력용 함수 제작
qq_total = function(x){
  mu = mean(x); sig = sd(x);
  qqplot(x, qnorm(ppoints(n), mean = mu, sd = sig), pch = 19);
  qqline(x, distribution = function(p) qnorm(p, sd = sig, mean = mu), col='yellow')
}

## 그림 출력
par(mfrow = c(1,3))
for (i in 1:3)
  qq_total(d1[,i])

## 표준화하기
d2 = scale(d1)
class(d1)
class(d2)

df1 = data.frame(d1)
names(df1) = c('X1', 'X2', 'X3')
df2 = data.frame(d2)
names(df2) = c('X1', 'X2', 'X3')

head(d1)
head(d2)
