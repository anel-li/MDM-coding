#########################################
#####    FULLY SYNTHETIC DATA      ######
#####simulation with multivariate Y######
#########################################

####we assume that data consist of five variables Y1 to Y5
####Y1 and Y2 are available for the entire population
####Y3 to Y5 will be synthesized

###synthesis function

synthesizer<-function(data.org,pop,m,n,nsyn){

###get regression coefficients

	reg1<-lm(data.org[,3]~data.org[,1]+data.org[,2], x=T)
	reg2<-lm(data.org[,4]~data.org[,1]+data.org[,2]+data.org[,3], x=T)
	reg3<-lm(data.org[,5]~data.org[,1]+data.org[,2]+data.org[,3]+data.org[,4], x=T)
	regcoef1<-summary(reg1)$coef[,1]
	regcoef2<-summary(reg2)$coef[,1]
	regcoef3<-summary(reg3)$coef[,1]
	regvar1<-summary(reg1)$sigma^2
	regvar2<-summary(reg2)$sigma^2
	regvar3<-summary(reg3)$sigma^2
	regxtx1<-solve(t(reg1$x) %*% reg1$x)
	regxtx2<-solve(t(reg2$x) %*% reg2$x)
	regxtx3<-solve(t(reg3$x) %*% reg3$x)

	syn.data<-array(NA,dim=c(nsyn,5,m))
	for (j in 1:m){				#repeat until number of samples = samplenum

###draw new values for beta and sigma

		newsigma1<-(n-3)*regvar1/rchisq(n=1, (n-3))
		newbeta1<-mvrnorm(n=1,regcoef1, regxtx1*newsigma1)

		newsigma2<-(n-4)*regvar2/rchisq(n=1, (n-4))
		newbeta2<-mvrnorm(n=1,regcoef2, regxtx2*newsigma2)

		newsigma3<-(n-5)*regvar3/rchisq(n=1, (n-5))
		newbeta3<-mvrnorm(n=1, regcoef3, regxtx3*newsigma3)

####draw a new sample of Y1 and Y2
		sample.new<-pop[sample(1:N,nsyn,replace=FALSE),]

####draw new values for Y3, Y4 and Y5

		newY3<-rnorm(nsyn, mean=cbind(1,sample.new)%*% newbeta1, sd=sqrt(newsigma1))
		newY4<-rnorm(nsyn, mean=cbind(1,sample.new,newY3)%*% newbeta2, sd=sqrt(newsigma2))
		newY5<-rnorm(nsyn, mean=cbind(1,sample.new,newY3,newY4)%*% newbeta3, sd=sqrt(newsigma3))
	
		syn.data[,,j]<-cbind(sample.new,newY3,newY4,newY5)
		}

	return(syn.data)
}


###analysis function
analysis<-function(data){

###quantities of interest: 	mean of Y3, Y4, and Y5
###					regression coefficients
	n<-nrow(data)
	Y3.mean<-mean(data[,3])
	Y4.mean<-mean(data[,4])
	Y5.mean<-mean(data[,5])
	reg1<-lm(data[,2]~data[,1]+data[,3]+data[,4]+data[,5])
	reg2<-lm(data[,5]~data[,1]+data[,2]+data[,3]+data[,4])
	q<-c(Y3.mean,Y4.mean,Y5.mean,summary(reg1)$coef[,1],summary(reg2)$coef[,1])
	u<-c(var(data[,3])/n,var(data[,4])/n,var(data[,5])/n,
         summary(reg1)$coef[,2]^2,summary(reg2)$coef[,2]^2)
	return(cbind(q,u))
	}



library(MASS)

##########################
#repeated simulation######
##########################
m<-50
n<-1000
nsyn<-2000
nsim<-1000

###################
#generate population
###################

###define parameters
mu<-c(0,0,0,0,0)
Sigma<-matrix(c(1,0.2,0.8,0.5,0.5,0.2,1,0.6,0.6,0.5,0.8,0.6,1,0.4,0.6,0.5,
       0.6,0.4,1,0.4,0.5,0.5,0.6,0.4,1),byrow=TRUE,ncol=5)

###generate population
N<-100000
pop<-mvrnorm(N,mu=mu,Sigma=Sigma)
q.pop<-analysis(pop)[,1]

res.org.all<-array(NA,dim=c(nsim,13,2))
res.syn.all<-array(NA,dim=c(nsim,13,4))

for(i in 1:nsim){
print(i)
####draw a new sample
	sample.org<-pop[sample(1:N,n,replace=FALSE),]
	res.org.all[i,,]<-analysis(sample.org)

####generate synthetic datasets




	syn.data<-synthesizer(sample.org,pop[,1:2],m,n,nsyn)

###obtain analysis results

	syn.res<-array(NA,dim=c(13,2,m))
	for(j in 1:m){
		syn.res[,,j]<-analysis(data=syn.data[,,j])
		}
	q.bar<-apply(syn.res[,1,],1,mean)
	u.bar<-apply(syn.res[,2,],1,mean)
	b.m<-apply(syn.res[,1,],1,var)

	T.f<-(1+1/m)*b.m-u.bar
	T.f.star<-T.f
	T.f.star[T.f<0]<-nsyn/n*u.bar[T.f<0]
	

###compute the degrees of freedom
	dfs<-(m-1)*(1-u.bar/((1+1/m)*b.m))^2

	res.syn.all[i,,]<-cbind(q.bar,T.f,T.f.star,dfs)
	}

#################
####evaluate results
#################

####compute confidence intervals

l.95.org<-res.org.all[,,1]-1.96*sqrt(res.org.all[,,2])
u.95.org<-res.org.all[,,1]+1.96*sqrt(res.org.all[,,2])

###calculate coverage for each quantity of interest

cover.org<-rep(NA,length(q.pop))
for(i in 1:length(q.pop)){
	cover.org[i]<-mean(ifelse(l.95.org[,i]<=q.pop[i] & u.95.org[,i]>=q.pop[i],1,0))
	}


#compute confidence intervals for synthetic data
dfs<-res.syn.all[,,4]
l.95.syn<-res.syn.all[,,1]-qt(0.975,df=dfs)*sqrt(res.syn.all[,,3])
u.95.syn<-res.syn.all[,,1]+qt(0.975,df=dfs)*sqrt(res.syn.all[,,3])

cover.syn<-rep(NA,length(q.pop))
for(i in 1:length(q.pop)){
	cover.syn[i]<-mean(ifelse(l.95.syn[,i]<=q.pop[i] & u.95.syn[,i]>=q.pop[i],1,0))
	}

####compute percentage negative variance estimates
neg<-apply(res.syn.all[,,2],2,function(x) mean(x<0))

###print results

###combine results
results<-cbind(q.pop,apply(res.org.all[,,1],2,mean),
         apply(res.syn.all[,,1],2,mean),cover.org,cover.syn,neg*100)
dimnames(results)=list(c("meanY3","meanY4","meanY5",rep("reg1",5),
         rep("reg2",5)),c("q.pop","q.org","q.syn","cover.org","cover.syn",
          "% negative T.f"))
print(round(results,3))
