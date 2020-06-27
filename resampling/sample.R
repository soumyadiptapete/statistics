library(bootstrap)
x=seq(1,100,0.5)
y=seq(3,102,0.5)
samplecor=cor(x,y)
n=length(x)
nboot=5000
resamplecor=rep(0,nboot)
for (b in (1:nboot))
{ind=sample(1:n,replace=TRUE)
resamplecor[b]=cor(x[ind], y[ind])
}
samplecor
mean(resamplecor)
sd(resamplecor)

#bias of samplecor
bias=mean(resamplecor)-samplecor

#MSE of samplecor 
MSE=sum((resamplecor-samplecor)^2)

MSE-bias^2
bias^2/MSE