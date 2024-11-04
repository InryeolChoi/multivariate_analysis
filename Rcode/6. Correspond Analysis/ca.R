# 대응분석
rm(list=ls())
library(ca)

# 데이터 불러오기
brand = read.csv('./brand.csv', head=T)
head(brand)

# 교차분석 실시
brand.tbl = xtabs(count ~ age + brand, data = brand)
brand.tbl

# 대응분석 실시
brand.ca = ca(brand.tbl)
plot(brand.ca, map='symmetric')
