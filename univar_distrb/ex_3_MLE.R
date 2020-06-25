data(Garch,package='Ecdat')
library(fGarch)
data("EuStockMarkets")
Y=diff(log(EuStockMarkets[,1]))


#standard t parameter estimation using mle

loglik_std=function(x){f=-sum(dsstd(Y,x[1],x[2],x[3],log=TRUE))
return(f)}
start=c(mean(Y), sd(Y),4)
fit_std=optim(start,loglik_std,method="L-BFGS-B",
              lower=c(-0.1,0.001,2.1),
              upper=c(0.1,1,20),hessian = TRUE)
cat("MLE=",round(fit_std$par,digits=5))
minus_log_lik=fit_std$value
AIC_std=2*minus_log_lik+2*length(fit_std$par)
print(AIC_std)