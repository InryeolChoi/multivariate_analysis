# 군집분석
rm(list=ls())
getwd()
setwd("C:/Users/dlsfu/OneDrive/바탕 화면/multivariate/practice2")
d1 = read.csv("./protein.csv")
head(data)

# 계보적 군집분석 ----
d1.dist = dist(scale(d1[, -1]))
d1.hi = hclust(d1.dist)
## 시각화
plot(d1.hi, labels = d1$country)
rect.hclust(d1.hi, k=5, border='red')
## 
d2 = data.frame(country = d1$country, d1[,-1], cluster = cutree(d1.hi, k=5))
d2
## 
table(d2$cluster)
d3 = aggregate(. ~ cluster, d2[,-1], mean)
d3

# 평균을 군집별로 묶고 시각화
colcol = c('white', 'yellow', 'green', 'cyan', 'black', 'grey')
kkk = 5
vv = names(d3)[-1]
tmp = barplot(as.numeric((as.matrix(d3[,-1]))), col=colcol[1:kkk])
text(matrix(tmp, kkk)[1, ], rep(-2, length(vv)), vv, xpd=T, cex=.5)
text(tmp, rep(-4, length(tmp)), rep(c(' ', 2:kkk), length(vv)), xpd=T, cex=.5)

# kmeans ----
set.seed(4)
library(cluster)
d1.k = kmeans(scale(d1[,-1]), centers = 5)
d4 = data.frame(d1, cluster = d1.k$cluster)
d4

par(mfcol = c(1,2))
clusplot(scale(d1[,-1]), d1.k$cluster)
tmp = princomp(scale(d1[, -1]))
plot(tmp$scores[, 1], tmp$scores[, 2], type='n')
text(tmp$scores[, 1], tmp$scores[, 2], d1.k$cluster)

# pam ----
par(mfcol=c(1,2))
d1.p = pam(scale(d1[,-1]), 5)
plot(d1.p)

# 군집갯수 선정
## factoextra로 시각화
library(factoextra)
fviz_nbclust(scale(d1[,-1]), kmeans)
fviz_nbclust(scale(d1[,-1]), pam)

## nbclust로 시각화
install.packages('NbClust')
library(NbClust)
NbClust(scale(d1[,-1]), method = 'kmeans')
tmp = NbClust(scale(d1[,-1]), method = 'kmeans', index = 'ccc')
plot(names(tmp$All.index), tmp$All.index, type='b')

# 모형 기반 군집분석 ----
library(mclust)
result = Mclust(scale(d1[, -1]))
summary(result)
plot(result) # 종료하려면 0, 아니면 1

