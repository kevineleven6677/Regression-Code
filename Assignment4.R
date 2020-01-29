setwd("C:/Users/user/Desktop/2018- NJIT Life/2019 fall/Math644 Regression Analysis Method/assignment")

#for problem2
data1=data.frame(read.table("data020101.txt"))
head(data1)
colnames(data1)=c("Y","X1","X2","X3")
model1=lm(Y~X1+X2+X3,data=data1)
anova(model1)
qf(0.975,1,42)
SSE_F=sum(model1$residuals^2)

model1_reduce=lm((Y+X1)~X3,data=data1)
anova(model1_reduce)
SSE_R=sum(model1_reduce$residuals^2)

((SSE_R-SSE_F)/2)/(SSE_F/42)
qf(0.975,2,42)

#for problem4
data2=data.frame(read.table("CH08PR06.txt"))
colnames(data2)=c("Y","X")
head(data2)
X2=data2$X^2
model2=lm(Y~X+X2, data=data2)
summary(model2)
plot(data2$X,data2$Y, main = "Scalter plot for X,Y",xlab="X",ylab="Y")

x=seq(5,25,by = 0.01)
x2=x^2
new=data.frame(X=x,X2=x2)
y=predict(model2,new)
points(x,y,type = "l")

anova(model2)
(793.28+252.99)/9.94
qf(0.99,2,24)

new2=data.frame(X=15,X2=225)
predict(model2,new2,interval = "confidence",level=0.99)
