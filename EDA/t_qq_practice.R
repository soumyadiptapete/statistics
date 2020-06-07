logR=diff(log(EuStockMarkets))
#qq plot with t distributions for various df
n=dim(logR)[1]# n is the number of different indices
q_grid=(1:n)/(n+1)
df_grid=c(1,4,6,10,20,30)
index_names=colnames(logR)

for (i in 1:4)
{dev.new()
  par(mfrow=c(3,2))
  for (df in df_grid){
    qqplot(logR[,i],qt(q_grid,df),
    main=paste(index_names[i],"df=",df))
    y_vals=qt(c(0.25,0.75),df=df)
    x_vals=quantile(logR,c(0.25,0.75))
    abline(lm(y_vals~x_vals))
    
  }}
