library(Ecdat)
?CPSch3
data(CPSch3)
dimnames(CPSch3)[[2]]
male.earnings=CPSch3[CPSch3[,3]=='male',2]
sqrt.male.earnings=sqrt(male.earnings)
log.male.earnings=log(male.earnings)

dev.new()
par(mfrow=c(2,2))
qqnorm(male.earnings,datax=TRUE,main='untransformed')
qqnorm(sqrt.male.earnings,datax=TRUE,main='sqrt transformed')
qqnorm(log.male.earnings,datax=TRUE,main='log tansformed')

