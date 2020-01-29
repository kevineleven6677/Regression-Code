xi1=c(7,4,16,3,21,8)
xi2=c(33,41,7,49,5,31)
yi=c(42,33,75,28,91,55)

n=length(xi1)

Xdesign=cbind(rep(1,n),xi1,xi2)


betahat=solve(t(Xdesign)%*%Xdesign)%*%t(Xdesign)%*%yi
H=Xdesign%*%solve(t(Xdesign)%*%Xdesign)%*%t(Xdesign)
ei=(diag(n)-H)%*%yi
J=matrix(rep(1,n*n),nrow = n)
SSR=yi%*%(H-1/n*J)%*%yi

SSE=sum(ei^2)
MSE=SSE/(n-2)
sigmahat=MSE*solve(t(Xdesign)%*%Xdesign)

newX=c(1,10,30)
Yhat=newX%*%betahat
sigma_Yhat=newX%*%sigmahat%*%newX