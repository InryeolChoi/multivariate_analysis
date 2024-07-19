# <다변량분석 (1) - R언어 기초>
# 기초적인 R
x = rnorm(100)
hist(x)

# plotly 패키지
library(plotly)
x = rnorm(1000)
y = rnorm(1000)
z = dnorm(x) * dnorm(y) # dnorm = 정규분포
g = 'red'

d1 = data.frame(x,y,z,g)
d2 = d1
d2$x = d1$x + 0.9
d2$y = d1$y + 0.9
d2$g = 'blue'

d2 = rbind(d1, d2)
ddd = 1.2

plot_ly(x=d1$x, y=d1$y, z=d1$z, type="scatter3d",
        width=7, height=9, mode='markers',
        marker=list(size=1))%>%
  layout(scene = list(camera=list(eye = list(x=0/ddd, y=-.01/ddd, z=1.5/ddd))))

plot_ly(x=d2$x, y=d2$y, z=d2$z, type="scatter3d",
        width=7, height=9, mode='markers',
        marker=list(size=1))%>%
  layout(scene = list(camera=list(eye = list(x=1/ddd, y=1.1/ddd, z=.6/ddd))))

plot_ly(x=d2$x, y=d2$y ,z=d2$z ,type="scatter3d",
        width=7, height=9, mode='markers',
        marker=list(size=1,color=d2$g))%>%
  layout(scene = list(camera=list(eye = list(x=1/ddd, y=-1.1/ddd, z=.6/ddd))))
