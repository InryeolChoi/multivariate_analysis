# 판별분석
rm(list=ls())
getwd()

## 데이터 읽기
finance = read.csv("./finance.csv", header=TRUE)
head(finance)
dim(finance)

## 다변량 정규성 검사
library(MVN)
test_result = mvn(data = finance[, 2:5], mvnTest = "mardia")
test_result

## box's M-test
## h0 : 모든 그룹의 공분산 행렬이 동일하다
library(biotools)
finance.boxM = boxM(finance[, 2:5], finance$y)
finance.boxM

## 선형판별분석
library(MASS)
finance.lda = lda(y ~ x1 + x2 + x3 + x4, data = finance)
print(finance.lda)

## 선형판별점수
pred1 = predict(finance.lda, finance)
pred1

## 판별 점수 계산 및 요약
tmp = finance.lda$means
tmp4 = as.matrix(finance[,2:5]) %*% matrix(finance.lda[[4]], ncol=1)
tapply(tmp4,finance$y, function(xxx) round(c(mean(xxx),sd(xxx)),2))

## 판별 점수의 시각화
par(mfcol = c(1,2))
boxplot(pred1$x ~ finance$y); abline(h = 0, col = 'grey')
boxplot(tmp4 ~ finance$y); abline(h = 0, col = 'grey')

## 판별 점수의 비교
cbind(finance$y, pred1$x, tmp4, pred1$x - tmp4)

## 예측 결과 저장
finance.pred1 = cbind(finance, pred1$x, pred1$posterior, pred1$class)
print(finance.pred1, digits=3)

## 혼동행렬 생성
table(finance$y,pred1$class)

## 정오분류표
## 실제 클래스와 예측된 클래스의 교차표
finance.ctbl1 = table(finance$y,pred1$class)
library(DescTools)
Desc(finance.ctbl1,digits=2)

## box's m-test (모공분산행렬의 동일성 검정)
library(biotools)
finance.boxM = boxM(finance[,2:5],finance$y)
print(finance.boxM)
print(finance.boxM$cov)
print(finance.boxM$pooled)

## 이차 판별분석
finance.qda = qda(y ~ x1 + x2 + x3 + x4, data = finance)
pred2 = predict(finance.qda, finance)
finance.ctbl2 = table(finance$y, pred2$class)
library(DescTools)
Desc(finance.ctbl2, digits = 2)





