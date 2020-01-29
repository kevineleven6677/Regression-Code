setwd("C:/Users/user/Desktop/2018- NJIT Life/2019 fall/Math644 Regression Analysis Method/assignment")

###for problem3
data=read.table("CH06PR18.txt")
head(data)
colnames(data)=c("Y","X1","X2","X3","X4")
plot(data$X1,data$Y)
attach(data)
X5=X1^2
model1=lm(Y~X1+X2+X4+X5)
plot(Y,model1$fitted.values,xlab="Y",ylab = "Fitted Y",main="Fitted Y v.s. Y")
abline(0,1)
n=length(data)
summary(model1)

new_point=data.frame(X1=8,X2=16,X4=250000,X5=64)
predict(model1,new_point,interval="confidence")

###for problem5
data2=read.table("APPENC02.txt")
head(data2)
colnames(data2)=c("id","country","state","land","population","percent1834","percent65","physicians","bed","crimes","percent_eduhigh","percent_edubache","percent_low","percent_unemp","income_capita","income","region_id")
attach(data2)

model2_data=data.frame(Y=physicians,X1=population,X2=income,X3=(region_id==1),X4=(region_id==2),X5=(region_id==3))
model2=lm(Y~X1+X2+X3+X4+X5,data=model2_data)
summary(model2)
anova(model2)

model3=lm(Y~X1+X2,data=model2_data)
anova(model3)
((sum(model3$residuals^2)-sum(model2$residuals^2))/3)/(sum(model2$residuals^2)/model2$df.residual)
qf(0.9,3,434)
