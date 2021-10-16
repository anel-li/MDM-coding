#################################
########CPS DATA EXAMPLE#########
#################################
library(MASS)
library (arm)
library(truncnorm)
####load data "cps5000.RData"

#####variables to be protected: income, age, sex
m<-10
n<-nrow(cps)
set.seed(1234)

###transform data by taking the cubic root

org.data<-cps
org.data$tax<-org.data$tax^(1/3)
org.data$income<-org.data$income^(1/3)
org.data$csp<-org.data$csp^(1/3)
org.data$ss<-org.data$ss^(1/3)

#########
#obtain all regression parameters from the original data
#and evaluate models
#########

####parameters for sex

###obtain parameters from logit regression
nm<-glm(sex~.-age-income+I(tax>0),data=org.data,family=binomial(link="logit"))
###evaluate model
binnedplot(x=nm$fitted.values,y=nm$y-nm$fitted.values)

beta.hat<-summary(nm)$coef[,1]

####parameters for age

####obtain parameters from linear regression
reg1<-lm(age~.-income+I(tax>0),data=org.data)

###evaluate model
plot(reg1)

regcoef1<-summary(reg1)$coef[,1]
regvar1<-summary(reg1)$sigma^2

####parameters for income 

####obtain parameters from linear regression
reg2<-lm(income~.+I(tax>0),data=org.data)
###evaluate model
plot(reg2)

regcoef2<-summary(reg2)$coef[,1]
regvar2<-summary(reg2)$sigma^2

###generate empty list to be filled with synthetic data 
syn.data<-vector("list",m)

for (i in 1:m){

###store original dataset to be replaced later
	syn.data[[i]]<-cps



###sex


###generate new Y	
	X<-model.matrix(nm)
	P<-exp(c(X%*%beta.hat))/(1+exp(c(X%*%beta.hat)))
	u<-runif(n)

	sex.syn<-ifelse(P>=u,1,0)
	syn.data[[i]]$sex<-as.factor(sex.syn+1)

###age

###generate new Y	
	X<-model.matrix(reg1)

###adjust X
	X[,"sex2"]<-sex.syn

	age.syn<-rnorm(n,c(X%*%regcoef1),sqrt(regvar1))

###redraw for implausible values
lower.bound<-14
upper.bound<-91

	ind<-which(age.syn<lower.bound)
	while(length(ind)>0){

		age.syn[ind]<-rnorm(length(ind),c(X[ind,]%*%regcoef1),sqrt(regvar1))
		ind<-which(age.syn<lower.bound)
		
		}

	ind<-which(age.syn>upper.bound)
	while(length(ind)>0){
		age.syn[ind]<-rnorm(length(ind),c(X[ind,]%*%regcoef1),sqrt(regvar1))
		ind<-which(age.syn>upper.bound)

		}

###more convinient
	age.syn<-rtruncnorm(n,a=14,b=91,mean=c(X%*%regcoef1),sd=sqrt(regvar1))

	syn.data[[i]]$age<-as.integer(round(age.syn))

	
###income

###generate new Y	
	X<-model.matrix(reg2)
	X[,"sex2"]<-sex.syn
	X[,"age"]<-age.syn

###income should be >1
	
	income.syn<-rtruncnorm(n,a=1,mean=c(X%*%regcoef2),sd=sqrt(regvar2))


	syn.data[[i]]$income<-as.integer(round(income.syn^3))
	}

###check whether truncation is problematic
###age
X<-model.matrix(reg1)
X[,"sex2"]<-sex.syn
summary(pnorm(14,mean=c(X%*%regcoef1),sd=sqrt(regvar1)))
sort(pnorm(14,mean=c(X%*%regcoef1),sd=sqrt(regvar1)),decreasing=T)[1:100]

summary(1-pnorm(91,mean=c(X%*%regcoef1),sd=sqrt(regvar1)))
sort(1-pnorm(91,mean=c(X%*%regcoef1),sd=sqrt(regvar1)),decreasing=T)[1:100]

###income
X<-model.matrix(reg2)
X[,"sex2"]<-sex.syn
X[,"age"]<-age.syn
summary(pnorm(1,mean=c(X%*%regcoef2),sd=sqrt(regvar2)))
sort(pnorm(1,mean=c(X%*%regcoef2),sd=sqrt(regvar2)),decreasing=T)[1:100]


