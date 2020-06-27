library(bootstrap)
x=seq(1,100,0.5)
y=seq(3,102,0.5)
data=cbind(x,y)
samplecor= function(p,d){cor(d[p,1],d[p,2])}
n=length(x)
nboot=5000
#multivariate bootstrap
results=bootstrap(1:n,nboot,samplecor,data)
mean(results$thetastar)
#multivariate bootstrap confidence accelerated
bcconf=bcanon(1:n,nboot,samplecor,data)
bcconf$confpoints