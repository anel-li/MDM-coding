########################################################
#########Illustrations for Multiple Imputation##########
########################################################


#######fucntion for SRS#########
sampling<-function(n){
samp.ind<-sample(N,n,replace=FALSE)
y.org<-Y[samp.ind,]

return(y.org)
}

####function to generate missings under different designs

mis.gen=function(Y=y.org,n){
##MCAR

M<-0.5
mis.ind<-sample(n,size=M*n,replace=FALSE)
Y.MCAR<-Y
Y.MCAR[mis.ind,1]<-NA

###MAR

M<-exp(3*Y[,2])/(1+exp(3*Y[,2]))
rand<-runif(n)
Y.MAR<-Y
Y.MAR[M>rand,1]<-NA

###NMAR

M<-exp(3*Y[,1])/(1+exp(3*Y[,1]))
rand<-runif(n)
Y.NMAR<-Y
Y.NMAR[M>rand,1]<-NA

####store datasets in a list
data<-list(Y,Y.MCAR,Y.MAR,Y.NMAR)

return(data)
}



#########function to generate multiple imputations 

imputations=function(data=datasets,m=10){

###fill in original values to be replaced later
imputed<-array(data[[1]][,1],dim=c(nrow(data[[1]]),m,(length(data)-1)))
#matrix(rep(data[[1]][,1],3),ncol=3)


####start imputations

for (i in 2:length(data)){	###starting with 2 since 1 has no missings

	data1<-data[[i]]
	mis.ind<-is.na(data1[,1])

####Get parameters from regression

	reg<-lm(data1[,1]~data1[,2],x=T)
	regxtx<-solve(t(reg$x)%*%reg$x)
	regcoef<-reg$coef
	regvar<-summary(reg)$sigma^2

####Start multiple imputations
	
	for (j in 1:m){
		newsigma<-(table(mis.ind)[2]-1)*regvar/rchisq(n=1, (table(mis.ind)[2]-1))
		newbeta<-mvrnorm(n=1,regcoef, regxtx*newsigma)
		imp<-rnorm(n,0,sqrt(newsigma))+c(newbeta%*%t(cbind(1,data1[,2])))
		imputed[mis.ind,j,(i-1)]<-imp[mis.ind]
		}

	}


return(imputed)
}


###########function to estimate quantitites of interest

results=function(org=y.org,imp=data.imp,m=10){


###estimate results for complete case and imputed data

###mean of Y1

###run for each missing design for each imputation 

q.mean<-rep(NA,dim(imp)[3])
u.mean<-rep(NA,dim(imp)[3])
B.mean<-rep(NA,dim(imp)[3])

q.beta1<-rep(NA,dim(imp)[3])
u.beta1<-rep(NA,dim(imp)[3])
B.beta1<-rep(NA,dim(imp)[3])

q.beta2<-rep(NA,dim(imp)[3])
u.beta2<-rep(NA,dim(imp)[3])
B.beta2<-rep(NA,dim(imp)[3])

	for (i in 1:(dim(imp)[3])){

		q.mean[i]<-mean(apply(imp[,,i],2,mean))
		u.mean[i]<-mean(apply(imp[,,i],2,function (x) var(x)/length(x)))
		B.mean[i]<-var(apply(imp[,,i],2,mean))

		beta1<-rep(0,m)
		var.beta1<-rep(0,m)

		beta2<-rep(0,m)
		var.beta2<-rep(0,m)

		for (j in 1:m){
#regression coeff of Y1 in a regression of Y2 on Y1
			reg<-lm(org[,2]~imp[,j,i])
			beta1[j]<-summary(reg)$coef[2,1]
			var.beta1[j]<-summary(reg)$coef[2,2]^2

#regression coeff of Y2 in a regression of Y1 on Y2
			reg2<-lm(imp[,j,i]~org[,2])
			beta2[j]<-summary(reg2)$coef[2,1]
			var.beta2[j]<-summary(reg2)$coef[2,2]^2
			}
		q.beta1[i]<-mean(beta1)
		u.beta1[i]<-mean(var.beta1)
		B.beta1[i]<-var(beta1)

		q.beta2[i]<-mean(beta2)
		u.beta2[i]<-mean(var.beta2)
		B.beta2[i]<-var(beta2)
	}

res<-cbind(q.mean,u.mean,B.mean,q.beta1,u.beta1,B.beta1,q.beta2,u.beta2,B.beta2)


return(res)
}


############function to evaluate estimators

evaluation=function(m=10){

###evaluate  data

avrg<-apply(mi.results[,c(1,4,7),],1:2,mean)
true.var.org<-apply(mi.results[,c(1,4,7),],1:2,var)
T.mean<-mi.results[,2,]+(1+1/m)*mi.results[,3,]
T.beta1<-mi.results[,5,]+(1+1/m)*mi.results[,6,]
T.beta2<-mi.results[,8,]+(1+1/m)*mi.results[,9,]
var.ratio.mi<-cbind(apply(T.mean,1,mean)/true.var.org[,1],apply(T.beta1,1,mean)/true.var.org[,2],apply(T.beta2,1,mean)/true.var.org[,3])

df.mean<-(m-1)*((1+1/m)*mi.results[,3,]/T.mean)^{-2}
l.95.mean<-mi.results[,1,]-qt(df=df.mean,p=0.975)*sqrt(T.mean)
u.95.mean<-mi.results[,1,]+qt(df=df.mean,p=0.975)*sqrt(T.mean)
length.95.mean.mi<-apply(u.95.mean-l.95.mean,1,mean)
cover.mi.mean<-apply(ifelse(mean.pop>=l.95.mean&mean.pop<=u.95.mean,1,0),1,mean)

df.beta1<-(m-1)*((1+1/m)*mi.results[,6,]/T.beta1)^{-2}
l.95.beta1<-mi.results[,4,]-qt(df=df.beta1,p=0.975)*sqrt(T.beta1)
u.95.beta1<-mi.results[,4,]+qt(df=df.beta1,p=0.975)*sqrt(T.beta1)
length.95.beta1.mi<-apply(u.95.beta1-l.95.beta1,1,mean)
cover.mi.beta1<-apply(ifelse(beta1.pop>=l.95.beta1&beta1.pop<=u.95.beta1,1,0),1,mean)

df.beta2<-(m-1)*((1+1/m)*mi.results[,9,]/T.beta2)^{-2}
l.95.beta2<-mi.results[,7,]-qt(df=df.beta2,p=0.975)*sqrt(T.beta2)
u.95.beta2<-mi.results[,7,]+qt(df=df.beta2,p=0.975)*sqrt(T.beta2)
length.95.beta2.mi<-apply(u.95.beta2-l.95.beta2,1,mean)
cover.mi.beta2<-apply(ifelse(beta2.pop>=l.95.beta2&beta2.pop<=u.95.beta2,1,0),1,mean)

res.mi<-cbind(c(t(avrg)),c(t(var.ratio.mi)),c(rbind(length.95.mean.mi,length.95.beta1.mi,length.95.beta2.mi)),c(rbind(cover.mi.mean*100,cover.mi.beta1*100,cover.mi.beta2*100)))

print(paste("Results for multiple imputation"))
print(xtable(res.mi,digits=c(1,3,2,2,2)))

return(res.mi)


}


######global skript
print(Sys.time())
library (xtable)
library (MASS)
N<-1000000 			##population size
n<-1000			##sample size
sim<-5000			##number of simulation runs
m<-10				##number of imputations

######generate vectors to store results

mi.results<-array(NA,dim=c(3,9,sim))

######Generate population

mu<-c(0,0)
Sigma<-matrix(c(1,0.5,0.5,1),byrow=TRUE,ncol=2)
Y<-mvrnorm(n=N,mu=mu,Sigma=Sigma)
mean.pop<-mean(Y[,1])
beta1.pop<-(lm(Y[,2]~Y[,1]))$coef[2]
beta2.pop<-(lm(Y[,1]~Y[,2]))$coef[2]

###start sampling and imputation replications
for (k in 1:sim){
	print(k)
	y.org<-sampling(n=n)		###draw a sample of size n
	datasets<-mis.gen(Y=y.org,n)	###generate missings under different designs
	data.imp<-imputations(m=10)		###impute data under different models 
	mi.results[,,k]<-results(m=10)			###compute quantites of interest

	}

res.mi<-evaluation(m=10)
print(Sys.time())
