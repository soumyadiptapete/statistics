data=read.csv('ford.csv')
head(data)
returns=data$FORD
s_mean=mean(returns)
s_sd=sd(returns)
s_median=median(returns)
hist(returns)

dev.new()
qqnorm(returns,datax=T)
qqline(returns,datax=T)
print(shapiro.test(returns))

dev.new()
n=dim(data)[1]
q_grid=(1:n)/(n+1)
f_grid=c(2,4,5,10,15,20)
par(mfrow=c(3,2))
for (df in f_grid){
  
  qqplot(x=returns,y=qt(q_grid,df),xlab='actual',
         ylab='theoretical-t',main=paste("df=",df))
  abline(lm(qt(c(0.25,0.75),df)~quantile(returns,c(0.25,0.75))))
  
}

