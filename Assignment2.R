###problem1
setwd("C:/Users/user/Desktop/2018- NJIT Life/2019 fall/Math644 Regression Analysis Method/assignment")

data1=read.table("GPA.txt")
GPA=data1[,1]
ACT=data1[,2]
#attach(data1)

reg_GPA=lm(GPA~ACT)
n=length(GPA)


summary(reg_GPA)
anova(reg_GPA)
MSE_GPA=sum(reg_GPA$residuals^2)/(n-2)
SS_ACT=sum((ACT-mean(ACT))^2)
alpha=0.01

T_GPA=qt(1-alpha/2,df = n-2)

reg_GPA$coefficients[2]-(MSE_GPA/SS_ACT)^0.5*T_GPA
reg_GPA$coefficients[2]+(MSE_GPA/SS_ACT)^0.5*T_GPA

(1-pt(reg_GPA$coefficients[2]/(MSE_GPA/SS_ACT)^0.5,df = n-2))*2


newX=data.frame(ACT=c(28))
predY1=predict(reg_GPA,newX,interval = "confidence", level = 0.95)
predY2=predict(reg_GPA,newX,interval = "prediction", level = 0.95)



#####problem4

data2=read.table("Airfreight.txt")
Y=data2[,1]
X=data2[,2]

reg_air=lm(Y~X)

plot(X,Y,main="Scalter plot X and Y of Airfreight data")
abline(reg_air)
summary(reg_air)
n=length(X)
MSE_air=sum(reg_air$residuals^2)/(n-2)
SXX=sum((X-mean(X))^2)
alpha=0.05
T_air=qt(1-alpha/2,df = n-2)

reg_air$coefficients[2]+(MSE_air/SXX)^0.5*T_air
sd_beta1=(MSE_air/SXX)^0.5
(1-pt(reg_air$coefficients[2]/sd_beta1,df=n-2))*2



sd_beta0=(MSE_air*(1/n+mean(X)^2/SXX))^0.5

reg_air$coefficients[1]-T_air*sd_beta0
