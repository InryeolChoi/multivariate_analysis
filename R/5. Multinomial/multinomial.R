---
title: "다항분포 (Multinomial Distribution)"
output: html_notebook
---

## 다항분포란?
다항분포는 여러 범주의 사건들에 대한 일련의 독립적인 시행에서 각 범주가 발생할 확률을 나타내는 확률분포다. 이는 이산 확률분포 중 하나로, 한 번의 시행에서 여러 가지 결과가 나올 수 있는 경우에 사용된다.

다항분포는 이항분포의 일반화된 형태로 볼 수 있다. 이항분포가 두 가지 범주(성공과 실패)에 대한 분포를 다룬다면, 다항분포는 세 가지 이상 범주에 대한 분포를 다룬다.

## 다항분포의 사용 사례
다항분포는 다음과 같은 경우에 사용된다:
1. **선거에서의 득표율**: 여러 후보가 있는 선거에서 각 후보가 얻는 득표 수.
2. **여론조사**: 여러 선택지가 있는 여론조사에서 각 선택지가 선택될 확률.
3. **유전자형 빈도**: 유전자형이 여러 가지인 경우, 각 유전자형의 발생 빈도.

## 다항분포의 수식
다항분포는 다음과 같은 확률 질량 함수(pmf)를 가진다:
$$
P(X_1 = x_1, X_2 = x_2, \ldots, X_k = x_k) = \frac{n!}{x_1! x_2! \ldots x_k!} p_1^{x_1} p_2^{x_2} \ldots p_k^{x_k}
$$
여기서:
- $n$: 총 시행 횟수
- $k$: 범주의 수
- $x_i$: 범주 $i$가 발생한 횟수
- $p_i$: 범주 $i$가 발생할 확률
- $\sum_{i=1}^k x_i = n$
- $\sum_{i=1}^k p_i = 1$

## 다항분포의 평균과 분산
다항분포에서 각 범주 $i$의 발생 횟수 $X_i$에 대한 평균과 분산은 다음과 같다:

- 평균: $E[X_i] = n p_i$
- 분산: $Var[X_i] = n p_i (1 - p_i)$
- 공분산: $Cov[X_i, X_j] = -n p_i p_j \quad (i \ne j)$

## 모멘트 생성 함수 (MGF)
다항분포의 모멘트 생성 함수는 다음과 같다:
$$
M(t_1, t_2, \ldots, t_k) = \left( \sum_{i=1}^k p_i e^{t_i} \right)^n
$$

## R을 이용한 다항분포 예시

```{r}
# 다항분포 샘플 생성
n <- 10  # 시행 횟수
p <- c(0.2, 0.5, 0.3)  # 각 범주의 확률

# 다항분포로부터 샘플 생성
set.seed(42)
samples <- rmultinom(1000, n, p)

# 샘플의 평균 계산
mean_samples <- colMeans(samples)
print(mean_samples)

# 샘플의 분산 계산
var_samples <- apply(samples, 1, var)
print(var_samples)

# 다항분포의 pmf 시각화
categories <- c('Category 1', 'Category 2', 'Category 3')
sample_counts <- rowSums(samples)

barplot(sample_counts, names.arg=categories, xlab='Category', ylab='Frequency', main='Frequencies of Categories in Multinomial Distribution')

# 모멘트 생성 함수 (MGF) 계산
mgf <- function(t, p, n) {
  return((sum(p * exp(t))) ^ n)
}

t <- c(1, 1, 1)  # t1, t2, t3 값
mgf_value <- mgf(t, p, n)
print(mgf_value)
```
