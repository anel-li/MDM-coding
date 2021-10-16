#######problems with joint modeling

library(norm2)

###We use the dataset marijuana provided with this package
data(marijuana)

##Contains results of an experiment to investigate the effects of marijuana.
##Nine male subjects received three treatments each (placebo, low dose,
##and high dose). Under each treatment, the change in heart rate (beats per 
##minute above baseline) was recorded 15 minutes after smoking and 90 minutes 
##after smoking

marijuana

##what happens if we use all data?
em.res2<-emNorm(cbind(Plac.15,High.15,Plac.90,High.90)~Low.15+Low.90,
	data=marijuana)

###final Covariance matrix is (almost) singular
###Problem arises because of uniform prior
###sets nu=-(r+1) and S_p^(-1)=0, with r = # of response vars
###posterior is always W(n+\nu-p,...), p = # of predictors
###not defined if n is small (Wishart distribution only exists
###if nu>r

summary(em.res2)

###EM still converged. Maybe we can impute anyway?

imp2<-mcmcNorm(em.res2,iter=5000,impute.every=100,seeds=c(123,456))


###maybe we can use Jeffreys prior instead?
###sets nu=0 and S_p^{-1)=0

em.res3<-emNorm(cbind(Plac.15,High.15,Plac.90,High.90)~Low.15+Low.90,
	prior="jeffreys",data=marijuana)

imp3<-mcmcNorm(em.res3,iter=5000,impute.every=100,seeds=c(123,456))

###Problem arises since covariance still almost singular
###we need to use ridge prior
###variances are estimated based on fully observed data
###correlations are smoothed towards zero
###degrees of freedom govern the degree of smoothing
###small values are better since prior is still relatively uninformative

em.res4<-emNorm(cbind(Plac.15,High.15,Plac.90,High.90)~Low.15+Low.90,
	prior="ridge", prior.df=0.5,data=marijuana)

summary(em.res4)

imp4<-mcmcNorm(em.res4,iter=5000,impute.every=100,seeds=c(123,456))
plot(imp4$series.worst)
acf(imp4$series.worst,lag=100)


####what if we want to use Gelman-Rubin convergence diagnostic?

####implemented in the coda package

library(coda)

###need to run m independent chains
###parameters for each chain need to be stored in a separate list

m<-50
burn.in<-100

##get starting values using EM
start.val<-emNorm(cbind(Plac.15,High.15,Plac.90,High.90)~Low.15+Low.90,
	prior="ridge", prior.df=0.5,data=marijuana)

###generate empty vector to store mcmc chains
mcmc.results<-vector("list",m)

###generate list of datasets containing the original data
###variables with missings will be replaced by imputations later

imp.data<-vector("list",m)
imp.data<-lapply(imp.data,function(x) marijuana)

set.seed(1234)

for (i in 1:m){

	imp<-mcmcNorm(em.res4,iter=burn.in,impute.every=burn.in)
	all.pars<-cbind(imp$series.beta,imp$series.sigma,imp$series.worst)
	mcmc.results[[i]]<-as.mcmc(all.pars)
	imp.data[[i]][,c(1,3,4,6)]<-imp$imp.list[[1]]
	}	

###compute Gelman-Rubin diagnostic

R.hat<-gelman.diag(mcmc.results,autoburnin=FALSE,multivariate=FALSE)
R.hat



