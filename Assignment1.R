setwd("C:/Users/user/Desktop/2018- NJIT Life/2019 fall/Math644 Regression Analysis Method/assignment")

data=read.table("GPA.txt")

colnames(data)=c("GPA","ACT")
attach(data)
model1=lm(GPA~ACT)
model1
summary(model1)
plot(ACT,GPA,main="Scattler plot for data")
abline(model1)

new.X=data.frame(ACT=c(30))
predict(model1,new.X)
e=model1$residuals

plot(ACT,e,main="residual plot for GPA data")
abline(0,0)
sum(e^2)
