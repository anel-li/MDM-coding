library(MASS)

###generate one sample

n<-1000			##sample size
mu<-c(0,0)
Sigma<-matrix(c(1,0.5,0.5,1),byrow=TRUE,ncol=2)
Y<-mvrnorm(n=n,mu=mu,Sigma=Sigma)

set.seed(12345)

###generate missings

##MCAR

M<-0.5
mis.ind<-sample(n,size=M*n,replace=FALSE)
Y.MCAR<-Y
Y.MCAR[mis.ind,1]<-NA

###unconditional mean imputation
Y.imp.uncond.<-mean(Y.MCAR[,1],na.rm=TRUE)
plot(Y.MCAR,main="Unconditional mean imputation",xlab="Y1",ylab="Y2") 
points(cbind(Y.imp.uncond,Y.MCAR[is.na(Y.MCAR[,1]),2]),col=2)

###regression imputation
windows()
reg<-lm(Y.MCAR[,1]~Y.MCAR[,2])
Y.imp.reg<-coef(reg)[1]+coef(reg)[2]*Y.MCAR[is.na(Y.MCAR[,1]),2]
plot(Y.MCAR,main="Regression imputation",xlab="Y1",ylab="Y2") 
points(cbind(Y.imp.reg,Y.MCAR[is.na(Y.MCAR[,1]),2]),col=2)

###stochastic regression imputation
windows()
reg<-lm(Y.MCAR[,1]~Y.MCAR[,2])
mu<-coef(reg)[1]+coef(reg)[2]*Y.MCAR[is.na(Y.MCAR[,1]),2]
resid<-summary(reg)$sigma
n.imp<-length(mu)
Y.imp.st.reg<-rnorm(n.imp,mean=mu,sd=resid)
plot(Y.MCAR,main="Stochastic Regression imputation",xlab="Y1",ylab="Y2") 
points(cbind(Y.imp.st.reg,Y.MCAR[is.na(Y.MCAR[,1]),2]),col=2)

