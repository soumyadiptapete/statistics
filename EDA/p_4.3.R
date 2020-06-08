library("Ecdat")
head(Garch)
dev.new()
plot(density(Garch$dy))
data=Garch$dy
x=seq(0,0.01,by=0.0001)
lines(x,dnorm(x,mean=mean(data),sd=sd(data)),lty=3,col='red')
lines(x,dnorm(x,mean=median(data),sd=mad(data)),lty=2,col='blue')

legend("topright",c("KDE","normal_sd","normal_mad"),lty=c(1,3,2),col=c("black","red","blue"))