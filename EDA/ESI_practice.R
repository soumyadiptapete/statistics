data("EuStockMarkets")
mode(EuStockMarkets)
class(EuStockMarkets)
#plot(EuStockMarkets)
#pdf('EuStocks.pdf',width=6,height=5)
logr=diff(log(EuStockMarkets))#log(P_i+1)-log(p_i)
#plot(logr)

plot(as.data.frame(logr))# scatter plots of all pairs

par(mfrow=c(2,2))
for (i in colnames(logr)){
  qqnorm(logr[,i],datax = T,main=i)
  qqline(logr[,i],datax = T)
  print(shapiro.test(logr[,i]))
}



