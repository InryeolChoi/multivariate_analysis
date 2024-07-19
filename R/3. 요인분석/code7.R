# <다변량분석 (6) - 요인분석(3)>
rm(list=ls())
# 자료 만들기 ------------------------------------------------------------------
## 위샤트분포
library(MASS)
pp = set.seed(2022)
Sigma = matrix(rWishart(1, pp, diag(pp)), pp)
R = diag(1/sqrt(diag(Sigma))) %*% Sigma %*% diag(1/sqrt(diag(Sigma)))
round(Sigma, 2); det(Sigma); round(R, 2)

n = 10000
mu = rep(0, pp)
set.seed(2022)
d1 = mvrnorm(n, mu, Sigma)
d1 = data.frame(d1)
names(d1) = paste('X', 1:pp, sep='')

# 인자분석 : 회전 (1) ------------------------------------------------------------------
# install.packages("GPArotation")
library("GPArotation")
mm = 3
d1.fa.1 = factanal(d1, mm, rotation = 'none')
d1.fa.2 = factanal(d1, mm, rotation = 'varimax')
d1.fa.3 = factanal(d1, mm, rotation = 'promax')
d1.fa.1
d1.fa.2
d1.fa.3

# function
visual_2 = function(dd){
  dd = dd$loadings[1:8, ]
  mat = matrix(as.character(round(as.numeric(dd), 3)), 8)
  mat[abs(dd) < 0.6] = ''
  mat = data.frame(paste('X', 1:8, sep=''), mat)
  names(mat) = c('Var', 'Fact1', 'Fact2', 'Fact3')
  mat
}

## rotation = 'none'
dd = d1.fa.1
visual_2(dd)

## rotation = 'varimax'
dd = d1.fa.2
visual_2(dd)

## rotation = 'varimax'
dd = d1.fa.3
visual_2(dd)

## rotation = 'quartimax'
d1.fa.4 = factanal(d1, mm, rotation='quartimax')
visual_2(d1.fa.4)

## rotation = 'oblimin'
d1.fa.5 = factanal(d1, mm, rotation='oblimin')
visual_2(d1.fa.5)


# 인자분석 : 회전 (2) ---------------------------------------------------------------
## rotation의 효과 분석 - 열 단위
round(apply(loadings(d1.fa.1)^2, 2, var), 2)
round(apply(loadings(d1.fa.2)^2, 2, var), 2)
round(apply(loadings(d1.fa.3)^2, 2, var), 2)
round(apply(loadings(d1.fa.4)^2, 2, var), 2)
round(apply(loadings(d1.fa.5)^2, 2, var), 2)

## rotation의 효과 분석 - 행 단위
round(apply(loadings(d1.fa.1)^2, 1, var), 2)
round(apply(loadings(d1.fa.2)^2, 1, var), 2)
round(apply(loadings(d1.fa.3)^2, 1, var), 2)
round(apply(loadings(d1.fa.4)^2, 1, var), 2)
round(apply(loadings(d1.fa.5)^2, 1, var), 2)

## biplot 그리기
par(mfrow = c(2, 3))
D1 = d1.fa.1$loadings[1:pp, 1:2]
biplot(x = D1, y = D1)
D1 = d1.fa.2$loadings[1:pp, 1:2]
biplot(x = D1, y = D1)
D1 = d1.fa.3$loadings[1:pp, 1:2]
biplot(x = D1, y = D1)
plot(1, 1, type="n", bty="n", axes=F, xlab="", ylab="")
D1 = d1.fa.4$loadings[1:pp, 1:2]
biplot(x = D1, y = D1)
D1 = d1.fa.5$loadings[1:pp, 1:2]
biplot(x = D1, y = D1)

## varimax 적용
ro = varimax(loadings(d1.fa.1))
round(ro$rotmat %*% t(ro$rotmat), 3)
loadings(d1.fa.1) %*% ro$rotmat
ß
## 전체 자료의 variance 대비 i번째 공통인자의 설명력 보기ßß
ratio = function(d){
  x1 = apply(loadings(d)^2, 2, sum)
  x2 = sum(x1 + sum(d$uniquenesses))
  x1/x2
}

d1.fa.2
ratio(d1.fa.2)
d1.fa.3
ratio(d1.fa.3)


# 자료 만들기 : 시계열구조 -----------------------------------------------------------
## 위샤트분포에 AR(1) 과정이 들어간 형태
library(MASS)
library(Matrix)
pp = 3
set.seed(2022)
AR_cov = function(r, p){
  rrr = (1:p)
  ttt = matrix(rrr, nrow = p, ncol = p, byrow = T)
  return(r^abs(ttt - rrr))
}

m1 = matrix(rWishart(1, pp, AR_cov(0.9, pp)), pp)
bdiag(m1)
m2 = matrix(rWishart(1, pp, AR_cov(0.1, pp)), pp)
bdiag(m1, m2)

## 2개의 집단 : 일반 + 시계열 -> 겹친형태의 행렬
bdiag(diag(3), AR_cov(0.1, 3))


# 인접 집단 ------------------------------------------------------------------------
nn = 1000
mu = rep(0, pp)
set.seed(2022)
d1 = mvrnorm(nn, mu, Sigma)
d1 = data.frame(scale(d1))
names(d1) = paste('X', 1:pp, sep='')

d1.fa = factanal(d1[, -4], 3, rotation = 'varimax')
print(d1.fa, digits = 2, cutoff = 0.65, sort = T)

