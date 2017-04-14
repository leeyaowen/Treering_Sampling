#Confidence interval computation
y<-c(1,50,21,98,2,36,4,29,7,15,86,10,21,5,4)
n<-length(y)
n
N<-286
ybar<-mean(y)
ybar
var_hat_y_bar<-(1-n/N)*var(y)/n
var_hat_y_bar
tau_hat<-N*ybar
tau_hat
var_hat_tau_hat<-N^2*var_hat_y_bar
var_hat_tau_hat
se_tau_hat<-sqrt(var_hat_tau_hat)
se_tau_hat
qt(0.95,df=14)
LowerLimit<-tau_hat-qt(0.95,df=14)*se_tau_hat
LowerLimit
UpperLimit<-tau_hat+qt(0.95,df=14)*se_tau_hat
UpperLimit
qt(c(0.05,0.95),df=14)
CI<-tau_hat+qt(c(0.05,0.95),df=14)*se_tau_hat
CI


#Simulations illustrating the approximate normality of a sampling distribution with small n and N
y<-trees$Volume
N<-31
png(filename = "hist_y.png",width = 800, height = 645, units = "px")
par(mar=c(5,5,5,3))
hist(y,cex.axis=1,cex.lab=1.5,main="hist_y")
dev.off()
hist(y,xlim = c(10,80))

ybar<-numeric(0)
b=10000
n=1
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n1.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn1")
dev.off()
n<-2
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n2.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn2")
dev.off()
n<-5
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n5.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn5")
dev.off()
n<-15
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n15.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn15")
dev.off()
n<-25
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n25.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn25")
dev.off()
n<-30
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n30.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn30")
dev.off()

ybar<-numeric(0)
b=10000
n=1
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n1c.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn1c",xlim = c(10,80))
dev.off()
n<-2
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n2c.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn2c",xlim = c(10,80))
dev.off()
n<-5
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n5c.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn5c",xlim = c(10,80))
dev.off()
n<-15
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n15c.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn15c",xlim = c(10,80))
dev.off()
n<-25
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n25c.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn25c",xlim = c(10,80))
dev.off()
n<-30
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean((y[s]))}
png(filename = "hist_ybar_n30c.png",width = 800)
par(mar=c(5,5,5,3))
hist(ybar,cex.axis=1,cex.lab=1.5,main="hist_ybarn30c",xlim = c(10,80))
dev.off()
