#####general parameters
n<-1000 #number of observations in the original data
m<-5    #number of synthetic datasets

############
#generate data
#############

###draw n values from a standard normal distribution
y.org<-rnorm(n)

############
#start synthesis
###########

y.syn.post<-array(NA,dim=c(n,m))
y.syn<-array(NA,dim=c(n,m))

for (i in 1:m){

####draw a new value for the parameters mu and sigma
####!not necessary for partially synthetic data!

###sigma

	chisq<-rchisq(1,(n-1)) #one random draw from a chi-squared distribution with n-1 df
	sigma2<-(n-1)*var(y.org)/chisq

###mu
	mu<-rnorm(1,mean=mean(y.org),sd=sqrt(sigma2/n)) #posterior of the mean is normal distributed
					   

####draw synthetic values

	y.syn.post[,i]<-rnorm(n,mean=mu,sd=sqrt(sigma2))
	y.syn[,i]<-rnorm(n,mean=mean(y.org),sd=sqrt(var(y.org)))

	}

#################
#analyize synthetic data
#################

###analyst is interested in the mean of y

####results using posterior draws
q.i.post<-apply(y.syn.post,2,mean)
u.i.post<-apply(y.syn.post,2,var)/n

q.bar.post<-mean(q.i.post)
u.bar.post<-mean(u.i.post)
b.m.post<-var(q.i.post)

T.p.post<-u.bar.post+b.m.post/m

###compute the degrees of freedom
dfs.post<-(m-1)*(1+u.bar.post/(b.m.post/m))^2

l.95.post<-q.bar.post-qt(0.975,df=dfs.post)*sqrt(T.p.post)
u.95.post<-q.bar.post+qt(0.975,df=dfs.post)*sqrt(T.p.post)
ci.post<-c(l.95.post,u.95.post)

####results using ML-estimate
q.i<-apply(y.syn,2,mean)
u.i<-apply(y.syn,2,var)/n

q.bar<-mean(q.i)
u.bar<-mean(u.i)
b.m<-var(q.i)

T.p<-u.bar+b.m/m

###compute the degrees of freedom
dfs<-(m-1)*(1+u.bar/(b.m/m))^2

l.95<-q.bar-qt(0.975,df=dfs)*sqrt(T.p)
u.95<-q.bar+qt(0.975,df=dfs)*sqrt(T.p)
ci<-c(l.95,u.95)

print(paste("the means of the synthetic sample are:"))
print(c(q.bar.post,q.bar))
print(paste("the confidence intervals for the mean are given by:"))
print(rbind(ci.post,ci))
