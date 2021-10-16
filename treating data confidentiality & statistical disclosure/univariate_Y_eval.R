#####synthesis function
synthesizer<-function(m,n,y.org){
y.syn.post<-array(NA,dim=c(n,m))
y.syn<-array(NA,dim=c(n,m))

for (i in 1:m){

####draw a new value for the parameters mu and sigma

###sigma

	chisq<-rchisq(1,(n-1)) #one random draw from a chi-squared distribution with n-1 df
	sigma2<-(n-1)*var(y.org)/chisq

###mu
	mu<-rnorm(1,mean=mean(y.org),sd=sqrt(sigma2/n)) #posterior of the mean is normal distributed
					   

####draw synthetic values

	y.syn.post[,i]<-rnorm(n,mean=mu,sd=sqrt(sigma2))
	y.syn[,i]<-rnorm(n,mean=mean(y.org),sd=sqrt(var(y.org)))

	}
return(list(y.syn.post,y.syn))
}

####analysis function
analysis<-function(y.syn,n,m){
q.i<-apply(y.syn,2,mean)
u.i<-apply(y.syn,2,var)/n

q.bar<-mean(q.i)
u.bar<-mean(u.i)
b.m<-var(q.i)

T.p<-u.bar+b.m/m

###compute the degrees of freedom
dfs<-(m-1)*(1+u.bar/(b.m/m))^2

return(c(q.bar,T.p,dfs))
}

###############################
###repeated simulation#########
###############################

nsim<-5000 	#number of simulation runs
n<-1000	#number of records in the dataset
m<-5		#number of imputations

###generate vectors for the results
res.org.all<-array(NA,dim=c(nsim,2))
res.syn.post.all<-array(NA,dim=c(nsim,3))
res.syn.all<-array(NA,dim=c(nsim,3))

###start simulation
for (i in 1:nsim){
	print(i)
############
#generate data
#############

###draw n values from a standard normal distribution
	y.org<-rnorm(n)

###compute quantitiy of interest (mean)
	res.org.all[i,1]<-mean(y.org)
	res.org.all[i,2]<-var(y.org)/n

###synthesize data
	y.syn.res<-synthesizer(m,n,y.org)

###analyize synthetic data
	res.syn.post.all[i,]<-analysis(y.syn.res[[1]],n,m)
	res.syn.all[i,]<-analysis(y.syn.res[[2]],n,m)
	}


#################
####evaluate results
#################

####compute confidence intervals

l.95.org<-res.org.all[,1]-1.96*sqrt(res.org.all[,2])
u.95.org<-res.org.all[,1]+1.96*sqrt(res.org.all[,2])
cover.org<-ifelse(l.95.org<=0 & u.95.org>=0,1,0)

#compute confidence intervals for synthetic data based on posterior draws
dfs.post<-res.syn.post.all[,3]
l.95.syn.post<-res.syn.post.all[,1]-qt(0.975,df=dfs.post)*sqrt(res.syn.post.all[,2])
u.95.syn.post<-res.syn.post.all[,1]+qt(0.975,df=dfs.post)*sqrt(res.syn.post.all[,2])
cover.syn.post<-ifelse(l.95.syn.post<=0 & u.95.syn.post>=0,1,0)

#compute confidence intervals for synthetic data based on ML estimate
dfs<-res.syn.all[,3]
l.95.syn<-res.syn.all[,1]-qt(0.975,df=dfs)*sqrt(res.syn.all[,2])
u.95.syn<-res.syn.all[,1]+qt(0.975,df=dfs)*sqrt(res.syn.all[,2])
cover.syn<-ifelse(l.95.syn<=0 & u.95.syn>=0,1,0)

###print results
print(paste("mean of the original samples:",mean(res.org.all[,1])))
print(paste("coverage rate of the original samples:",mean(cover.org)))

print(paste("mean of the synthetic samples (posterior):",mean(res.syn.post.all[,1])))
print(paste("coverage rate of the synthetic samples (posterior):",mean(cover.syn.post)))

print(paste("mean of the synthetic samples (ML based):",mean(res.syn.all[,1])))
print(paste("coverage rate of the synthetic samples (ML based):",mean(cover.syn)))

