### Name: bars
### Title: Bayesian Adaptive Regression Splines
### Aliases: bars
### Keywords: regression

### ** Examples

x<-c(1:110)
mu<-20*abs(exp(-x/100)*cos(x/5))
y<-rnorm(110,mu,1)
xo<-order(x)
out = bars(x[xo],y[xo],prior="uniform",priorparam=c(1,15),fits=TRUE)

upper=apply(out$sampfits,2,quantile,0.975)
lower=apply(out$sampfits,2,quantile,0.025)

matplot(x[xo],cbind(upper,out$postmeans,lower),lty=c(2,1,2),type='l')
points(x,y)



