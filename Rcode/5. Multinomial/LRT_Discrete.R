---
title: "이산형 분포에서의 우도비 검정 (Likelihood Ratio Test)"
output: html_notebook
---

## 우도비 검정이란?
우도비 검정(Likelihood Ratio Test, LRT)은 두 개의 통계 모델을 비교하는 데 사용되는 검정 방법이다. 이 검정은 주로 복잡한 모델과 단순한 모델을 비교하여, 단순한 모델이 데이터에 잘 맞는지 여부를 평가하는 데 사용된다. 우도비 검정의 검정 통계량은 다음과 같이 정의된다:
$$
D = -2 \log \left( \frac{L_0}{L_1} \right)
$$
여기서 $L_0$는 귀무가설 하에서의 최대 우도, $L_1$는 대립가설 하에서의 최대 우도이다. 이 통계량 $D$는 카이제곱 분포를 따르며, 자유도는 두 모델 간의 파라미터 수 차이와 같다.

## 이산형 분포에서의 우도비 검정
이산형 분포에서는 주로 포아송 분포와 다항 분포와 같은 분포를 사용하여 우도비 검정을 수행한다. 이 예제에서는 포아송 분포를 사용하여 우도비 검정을 수행하는 방법을 살펴본다.

## 우도비 검정의 단계
1. **모델 설정**: 귀무가설과 대립가설을 설정한다.
2. **최대 우도 추정**: 각각의 모델에 대해 최대 우도를 추정한다.
3. **검정 통계량 계산**: 우도비 검정 통계량 $D$를 계산한다.
4. **p-값 계산**: 검정 통계량의 p-값을 계산하여 귀무가설을 기각할지 여부를 결정한다.

## 평균과 분산
포아송 분포의 평균과 분산은 다음과 같다:
- 평균: $E[X] = \lambda$
- 분산: $Var[X] = \lambda$

## R을 이용한 우도비 검정 예시

```{r}
# 데이터 생성
set.seed(42)
data <- rpois(100, lambda = 3)

# 최대 우도 추정 함수
log_likelihood_poisson <- function(lambda, data) {
  sum(dpois(data, lambda, log = TRUE))
}

# 귀무가설 하에서의 최대 우도 추정
lambda_0 <- mean(data)  # MLE under null hypothesis
L0 <- log_likelihood_poisson(lambda_0, data)

# 대립가설 하에서의 최대 우도 추정
lambda_1 <- 3.5  # MLE under alternative hypothesis (example value)
L1 <- log_likelihood_poisson(lambda_1, data)

# 우도비 검정 통계량 계산
D <- -2 * (L0 - L1)
cat("우도비 검정 통계량:", D, "\n")

# p-값 계산
p_value <- pchisq(D, df = 1, lower.tail = FALSE)
cat("p-값:", p_value, "\n")
```

```{r}
# 우도비 검정 결과 시각화
x <- seq(0, 10, length.out = 1000)
y <- dchisq(x, df = 1)

plot(x, y, type = "l", lwd = 2, main = "Likelihood Ratio Test Visualization", xlab = "Test Statistic", ylab = "Density")
abline(v = D, col = "red", lwd = 2, lty = 2)
polygon(c(D, x[x >= D], max(x)), c(0, y[x >= D], 0), col = rgb(1, 0, 0, 0.5), border = NA)
legend("topright", legend = c(paste("Test Statistic (D =", round(D, 2), ")"), paste("p-value =", round(p_value, 4))), col = c("red"), lwd = 2, lty = 2, bty = "n")
```

```{R}
library(MASS)
library(stats4)

# 데이터 준비
data <- rpois(100, lambda = 3)

# 귀무가설 모델 (포아송 회귀)
null_model <- glm(data ~ 1, family = poisson)

# 대립가설 모델 (포아송 회귀)
data_df <- data.frame(count = data)
data_df$group <- ifelse(data_df$count > mean(data_df$count), 1, 0)
alt_model <- glm(count ~ group, family = poisson, data = data_df)

# 우도비 검정 수행
lr_stat <- 2 * (logLik(alt_model) - logLik(null_model))
p_value <- pchisq(lr_stat, df = 1, lower.tail = FALSE)

cat("우도비 검정 통계량:", lr_stat, "\n")
cat("p-값:", p_value, "\n")
```