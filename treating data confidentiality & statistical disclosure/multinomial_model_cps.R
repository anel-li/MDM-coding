############Example for categorical imputation
library(nnet)
library(MASS)
n<-nrow(cps)
m<-10

###load original data

###combine last two categories of race into "others"
cps$race[cps$race==4]<-3
cps$race<-factor(cps$race)

nm<-multinom(race~.-tax+I(tax),data=cps,maxit=5000,Hess=TRUE)

###obtain parameters from logit regression
beta.star<-summary(nm)$coeff			
hesse<-(nm)$Hessian		
var.star<-(solve(hesse))		

###problem not enough blacks with high education
table(cps$race,cps$educ)

cps$educ[cps$educ==46]<-45
cps$educ<-factor(cps$educ)


nm<-multinom(race~.-tax+I(tax),data=cps,maxit=5000,Hess=TRUE)

###obtain parameters from logit regression
beta.star<-summary(nm)$coeff			
hesse<-(nm)$Hessian		
var.star<-(solve(hesse))		

X<-model.matrix(nm)

###generate synthetic data 

syn.data<-list(rep(cps),m)

for (i in 1:m){

###store original dataset to be replaced later
	syn.data[[i]]<-cps

###draw new parameters (not necessary for partially synthetic data)
	beta<-mvrnorm(mu=c(beta.star[1,],beta.star[2,]),Sigma=var.star)	
	beta<-matrix(beta,ncol=(ncol(X)),byrow=T)			
	exp<-exp(X%*%t(beta.star))
	pi<-exp/(1+apply(exp,1,sum))
	base.cat<-(1-apply(pi,1,sum))
	R<-apply(cbind(base.cat,pi),1,cumsum)
	u<-runif(n)
	ind<-apply((u-t(R)),1,function(x) x>0)
	race.syn<-apply(ind,2,sum)+1
	syn.data[[i]]$race<-race.syn
	}

