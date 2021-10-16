########################################################
#########Illustrations for Single Imputation############
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
Y.MCAR<-Y[,1]
Y.MCAR[mis.ind]<-NA

###MAR

M<-exp(3*Y[,2])/(1+exp(3*Y[,2]))
rand<-runif(n)
Y.MAR<-Y[,1]
Y.MAR[M>rand]<-NA

###NMAR

M<-exp(3*Y[,1])/(1+exp(3*Y[,1]))
rand<-runif(n)
Y.NMAR<-Y[,1]
Y.NMAR[M>rand]<-NA

####store original data and different Y1 in a matrix
data<-cbind(Y,Y.MCAR,Y.MAR,Y.NMAR)

return(data)
}



#########function for different imputation models

imputations=function(data=datasets){

###fill in original values to be replaced later
uncon.mean<-matrix(rep(data[,1],3),ncol=3)
con.mean<-uncon.mean
reg.mean<-uncon.mean
reg.stoch<-uncon.mean
pmm<-uncon.mean

###define 10 quantiles of Y2 for conditional imputation
quant<-quantile(data[,2],prob=seq(0,1,by=0.1))  ###Beschäftigtenzahl in Quantile aufteilen
classes<-cut(data[,2],breaks=quant,label=FALSE,include.lowest=TRUE,right=FALSE)


####store complete cases

cc<-cbind(data[,3],data[,4],data[,5])

####start imputations

for (i in 3:ncol(data)){	###starting with 2 since 1 has no missings

	data1<-data[,i]
	mis.ind<-is.na(data1)
#####Unconditional Mean Imputation
	uncon.mean[mis.ind,(i-2)]<-mean(data1,na.rm=TRUE)

########Conditional Mean Imputation
	means<-by(data1,classes,mean,na.rm=TRUE)
	for (j in 1:length(means)){
		con.mean[classes==j&mis.ind,(i-2)]<-means[j]
		}

#########Regression Imputation

	reg<-lm(data1~data[,2])
	imputed<-reg$coef%*%t(cbind(1,data[,2]))
	reg.mean[mis.ind,(i-2)]<-imputed[mis.ind]

########Predictive Mean Matching
	predicted.imp<-c(imputed)[mis.ind==1]
	donors<-c(imputed)[mis.ind==0]
	which.Y<-apply(t(predicted.imp),2,function(x) order((x-donors)^2)[1:nn][sample(1:nn,1)])
	pmm[mis.ind,(i-2)]<-data1[mis.ind==0][which.Y]
#########Stochastic Regression Imputation

	sigma<-summary(reg)$sigma^2
	e<-rnorm(length(data1),mean=0,sd=sqrt(sigma))
	reg.stoch[mis.ind,(i-2)]<-imputed[mis.ind]+e[mis.ind]

	}

data.imp<-list(cc,uncon.mean,con.mean,reg.mean,pmm,reg.stoch)

return(data.imp)
}


###########function to estimate quantitites of interest

results=function(org=datasets[,1:2],imp=data.imp){

###generate vector to store results

res<-vector("list",7)

###estimate results for original sample

###mean of Y1
mean.org<-mean(org[,1])
var.mean.org<-var(org[,1])/nrow(org)

#regression coeff of Y1 in a regression of Y2 on Y1
reg<-lm(org[,2]~org[,1])
beta.org<-summary(reg)$coef[2,1]
var.beta.org<-summary(reg)$coef[2,2]^2

reg2<-lm(org[,1]~org[,2])
beta2.org<-summary(reg2)$coef[2,1]
var.beta2.org<-summary(reg2)$coef[2,2]^2

res[[1]]<-c(mean.org,var.mean.org,beta.org,var.beta.org,beta2.org,var.beta2.org)	
###estimate results for complete case and imputed data

for (i in 1:length(imp)){

###mean of Y1
	mean<-apply(imp[[i]],2,mean,na.rm=TRUE)
	var.mean<-apply(imp[[i]],2,function (x) var(x,na.rm=TRUE)/length(na.omit(x)))

	beta<-rep(0,ncol(imp[[i]]))
	var.beta<-rep(0,ncol(imp[[i]]))


	beta2<-rep(0,ncol(imp[[i]]))
	var.beta2<-rep(0,ncol(imp[[i]]))

###run for each missing design for each imputation method

	for (j in 1:ncol(imp[[i]])){

#regression coeff of Y1 in a regression of Y2 on Y1

		reg<-lm(org[,2]~imp[[i]][,j])
		beta[j]<-summary(reg)$coef[2,1]
		var.beta[j]<-summary(reg)$coef[2,2]^2

#regression coeff of Y1 in a regression of Y2 on Y1
		reg2<-lm(imp[[i]][,j]~org[,2])
		beta2[j]<-summary(reg2)$coef[2,1]
		var.beta2[j]<-summary(reg2)$coef[2,2]^2
		}

	res[[i+1]]<-cbind(mean,var.mean,beta,var.beta,beta2,var.beta2)

	}

return(res)
}


############function to evaluate estimators

evaluation=function(){

all.results<-vector("list",7)

###evaluate original data

avrg.org<-apply(org.results,2,mean)
true.var.org<-apply(org.results,2,var)
means.org<-c(avrg.org[1],avrg.org[3],avrg.org[5])
var.ratio.org<-c(avrg.org[2]/true.var.org[1],avrg.org[4]/true.var.org[3],avrg.org[6]/true.var.org[5])
l.95.mean<-org.results[,1]-qt(df=(n-1),p=0.975)*sqrt(org.results[,2])
u.95.mean<-org.results[,1]+qt(df=(n-1),p=0.975)*sqrt(org.results[,2])
length.95.mean.org<-mean(u.95.mean-l.95.mean)
cover.org.mean<-mean(ifelse(mean.pop>=l.95.mean&mean.pop<=u.95.mean,1,0))


l.95.beta<-org.results[,3]-qt(df=(n-2),p=0.975)*sqrt(org.results[,4])
u.95.beta<-org.results[,3]+qt(df=(n-2),p=0.975)*sqrt(org.results[,4])
length.95.beta.org<-mean(u.95.beta-l.95.beta)
cover.org.beta<-mean(ifelse(beta.pop>=l.95.beta&beta.pop<=u.95.beta,1,0))

l.95.beta2<-org.results[,5]-qt(df=(n-2),p=0.975)*sqrt(org.results[,6])
u.95.beta2<-org.results[,5]+qt(df=(n-2),p=0.975)*sqrt(org.results[,6])
length.95.beta2.org<-mean(u.95.beta2-l.95.beta2)
cover.org.beta2<-mean(ifelse(beta2.pop>=l.95.beta2&beta2.pop<=u.95.beta2,1,0))

res.org<-cbind(means.org,var.ratio.org,c(length.95.mean.org,length.95.beta.org,length.95.beta2.org),c(cover.org.mean*100,cover.org.beta*100,cover.org.beta2*100))
all.results[[1]]<-res.org


###evaluate imputed data

for (i in 1:length(imp.results)){
	res.temp<-array(NA,dim=c(9,4),dimnames=list(c("mean.MCAR","beta.MCAR","beta2.MCAR","mean.MAR","beta.MAR","beta2.MAR","mean.NMAR","beta.NMAR","beta2.NMAR"),c("mean","var.ratio","CI length","coverage rate")))
	avrg<-apply(imp.results[[i]],1:2,mean)
	true.var<-apply(imp.results[[i]],1:2,var)
	res.temp[,1]<-t(cbind(avrg[,1],avrg[,3],avrg[,5])) ###reorder to have mean and beta for each design together
	res.temp[,2]<-t(cbind(avrg[,2]/true.var[,1],avrg[,4]/true.var[,3],avrg[,6]/true.var[,5])) ###reorder to have mean and beta for each design together

	l.95.mean<-imp.results[[i]][,1,]-qt(df=(n-1),p=0.975)*sqrt(imp.results[[i]][,2,])
	u.95.mean<-imp.results[[i]][,1,]+qt(df=(n-1),p=0.975)*sqrt(imp.results[[i]][,2,])
	length.95.mean<-apply(u.95.mean-l.95.mean,1,mean)
	cover.mean<-apply(ifelse(mean.pop>=l.95.mean&mean.pop<=u.95.mean,1,0),1,mean)

	l.95.beta<-imp.results[[i]][,3,]-qt(df=(n-2),p=0.975)*sqrt(imp.results[[i]][,4,])
	u.95.beta<-imp.results[[i]][,3,]+qt(df=(n-2),p=0.975)*sqrt(imp.results[[i]][,4,])
	length.95.beta<-apply(u.95.beta-l.95.beta,1,mean)
	cover.beta<-apply(ifelse(beta.pop>=l.95.beta&beta.pop<=u.95.beta,1,0),1,mean)

	l.95.beta2<-imp.results[[i]][,5,]-qt(df=(n-2),p=0.975)*sqrt(imp.results[[i]][,6,])
	u.95.beta2<-imp.results[[i]][,5,]+qt(df=(n-2),p=0.975)*sqrt(imp.results[[i]][,6,])
	length.95.beta2<-apply(u.95.beta2-l.95.beta2,1,mean)
	cover.beta2<-apply(ifelse(beta2.pop>=l.95.beta2&beta2.pop<=u.95.beta2,1,0),1,mean)

	res.temp[,3]<-t(cbind(length.95.mean,length.95.beta,length.95.beta2))
	res.temp[,4]<-t(cbind(cover.mean*100,cover.beta*100,cover.beta2*100))

	all.results[[i+1]]<-res.temp
	}


return(all.results)


}


######global skript
print(Sys.time())
#library (xtable)
library (MASS)
N<-1000000 			##population size
n<-1000			##sample size
sim<-5000			##number of simulation runs
nn<-1			##number of nearest neighbors to be considered for pmm

######generate vectors to store results

org.results<-array(NA,dim=c(sim,6))
imp.results.temp<-array(NA,dim=c(3,6,sim))
imp.results<-vector("list",6)
for(i in 1:length(imp.results)){
	imp.results[[i]]<-imp.results.temp
	}

######Generate population

N<-1000000
mu<-c(0,0)
Sigma<-matrix(c(1,0.5,0.5,1),byrow=TRUE,ncol=2)
set.seed(12345)
Y<-mvrnorm(n=N,mu=mu,Sigma=Sigma)
mean.pop<-mean(Y[,1])
beta.pop<-(lm(Y[,2]~Y[,1]))$coef[2]
beta2.pop<-(lm(Y[,1]~Y[,2]))$coef[2]

###start sampling and imputation replications
for (k in 1:sim){
	print(k)
	y.org<-sampling(n=n)		###draw a sample of size n
	datasets<-mis.gen(Y=y.org,n)	###generate missings under different designs
	data.imp<-imputations()		###impute data under different models 
	res<-results()			###compute quantites of interest

#####store original results

	org.results[k,]<-res[[1]]

####store imputed results

	for(i in 1: length(imp.results)){

		imp.results[[i]][,,k]<-res[[i+1]]
		}
	}

all<-evaluation()

print(paste("Results Original Sample"))
print(round(all[[1]],3))

print(paste("Results Complete Case"))
print(round(all[[2]],3))

print(paste("Results Unconditional Imputation"))
print(round(all[[3]],3))

print(paste("Results Conditional Imputation"))
print(round(all[[4]],3))

print(paste("Results Regression Imputation"))
print(round(all[[5]],3))

print(paste("Results Predictive Mean Matching"))
print(round(all[[6]],3))

print(paste("Results Stochastic Regression Imputation"))
print(round(all[[7]],3))


