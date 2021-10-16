library(MASS)
n<-10000

X<-rbinom(n,prob=0.5,size=1)
Y<-0.2+0.7*X+rnorm(n,mean=0,sd=0.2)
hist(Y,breaks=1000)

#assume estimate of interest is \mu(Y)
#
#True mean is E(Y)=E(0.2+0.7*X+epsilon)=0.2+0.7*E(X)+0=0.2+0.7*0.5=0.55

meanY.true<-0.55

###run simulation

sim<-1000  ##number of simulation runs
m<-5       ##number of imputations

meanY.org<-rep(NA,sim)
var.meanY.org<-rep(NA,sim)
meanY.cc<-rep(NA,sim)
var.meanY.cc<-rep(NA,sim)
meanY.imp<-rep(NA,sim)
var.meanY.imp<-rep(NA,sim)
df.mean.imp<-rep(NA,sim)

for(i in 1:sim){

###generate data
  X<-rbinom(n,prob=0.5,size=1)
  Y<-0.2+0.7*X+rnorm(n,mean=0,sd=0.2)

##store estimate of interest
  meanY.org[i]<-mean(Y)
  var.meanY.org[i]<-var(Y)/n
###introduce missings

###MAR

  M<-rep(0.5,n)
  M[X==0]<-0.3
  rand<-runif(n)
  Y.mis<-Y
  Y.mis[M>rand]<-NA

##store estimate of interest using complete case analysis
  meanY.cc[i]<-mean(Y.mis,na.rm=TRUE)
  var.meanY.cc[i]<-var(Y.mis,na.rm=TRUE)/sum(!is.na(Y.mis))

###multiply impute missing values
	
####Get parameters from regression

  reg<-lm(Y.mis~X,x=T)
  regxtx<-solve(t(reg$x)%*%reg$x)
  regcoef<-reg$coef
  regvar<-summary(reg)$sigma^2

  Y.imp<-matrix(rep(Y.mis,m),ncol=m)

####Start multiple imputations
	
  for (j in 1:m){
	newsigma<-(sum(!is.na(Y.mis))-1)*regvar/rchisq(n=1, sum(!is.na(Y.mis))-1)
	newbeta<-mvrnorm(n=1,regcoef, regxtx*newsigma)
	imp<-rnorm(n,0,sqrt(newsigma))+c(newbeta%*%t(cbind(1,X)))
	Y.imp[is.na(Y.mis),j]<-imp[is.na(Y.mis)]
	}

###estimate mean based on imputed data

  q.i<-apply(Y.imp,2,mean)
  u.bar<-mean(apply(Y.imp,2,var)/n)
  b.m<-var(q.i)
  
  T.m<-(1+1/m)*b.m+u.bar
  df.mean.imp[i]<-(m-1)*((1+1/m)*b.m/T.m)^{-2}

  meanY.imp[i]<-mean(q.i)
  var.meanY.imp[i]<-T.m

  }

###compute coverage rates

###org
l95.org<-meanY.org-1.96*sqrt(var.meanY.org)
u95.org<-meanY.org+1.96*sqrt(var.meanY.org)	
cover.org<-(l95.org<=meanY.true&u95.org>=meanY.true)

###complete case
l95.cc<-meanY.cc-1.96*sqrt(var.meanY.cc)
u95.cc<-meanY.cc+1.96*sqrt(var.meanY.cc)	
cover.cc<-(l95.cc<=meanY.true&u95.cc>=meanY.true)

###MI
l95.imp<-meanY.imp-qt(df=df.mean.imp,p=0.975)*sqrt(var.meanY.imp)
u95.imp<-meanY.imp+qt(df=df.mean.imp,p=0.975)*sqrt(var.meanY.imp)
cover.imp<-(l95.imp<=meanY.true&u95.imp>=meanY.true)

res<-cbind(c(mean(meanY.org),mean(meanY.cc),mean(meanY.imp)),
	c(mean(cover.org)*100,mean(cover.cc)*100,mean(cover.imp)*100))

colnames(res)<-c("point estimate", "coverage rate")
rownames(res)<-c("no missings","complete case analysis", "multiple imputation")

print(round(res,3))

###Y does not need to be normal to get valid inferences with MI
###estimate of the parameter of interest in the analysis model 
###needs to be normally distributed for the fully observed data 

hist(meanY.org,breaks=100)

