library(Ecdat)
?CPSch3
data(CPSch3)
dimnames(CPSch3)[[2]]
male.earnings=CPSch3[CPSch3[,3]=='male',2]

library(MASS)
par(mfrow=c(1,1))
bc=boxcox(male.earnings~1,lambda=seq(0.3,0.45,1/100),interp=FALSE)
ind=(bc$y==max(bc$y))
ind2=(bc$y>max(bc$y)-qchisq(0.99,df=1)/2) #log-likelihood test
bc$x[ind]# alpha of MLE
bc$x[ind2]#alpha in confidence interval
max(bc$y) #MLE of Lambda

#skewed t fit
library(fGarch)
fit=sstdFit(male.earnings,hessian=TRUE)
fit$estimate

dev.new()
par(mfrow=c(1,1))
plot(density(male.earnings),lty=1)
x_data=seq(min(male.earnings),max(male.earnings),0.5)
lines(x=x_data,y=dsstd(x=x_data,mean=fit$estimate[1],sd=fit$estimate[2]
                            ,nu=fit$estimate[3],xi=fit$estimate[4]),lty=2)
legend('topright',legend=c('density plot', 'est skewed T'),lty=1:2)

#skewed GED fit

fit_ged=sgedFit(male.earnings,hessian=TRUE)
dev.new()
par(mfrow=c(1,1))
plot(density(male.earnings),lty=1)
x_data=seq(min(male.earnings),max(male.earnings),0.5)
lines(x=x_data,y=dsged(x=x_data,mean=fit_ged$par[1],sd=fit_ged$par[2]
                       ,nu=fit_ged$par[3]), xi=fit_ged$par[4],lty=2)
legend('topright',legend=c('density plot', 'est skewed GED'),lty=1:2)