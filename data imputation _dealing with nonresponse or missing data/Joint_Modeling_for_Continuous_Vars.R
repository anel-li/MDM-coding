library(norm2)

###need to load data
load("C:\\Users\\BA_Standalone\\Documents\\IPSM\\Multiple_Imputation\\R\\Fifth_Dutch_Growth_Study.RData")

ls()

##fdgs = Fifth Dutch Growth Rate Study 2009

summary(fdgs)

###hgt.z and wgt.z are the z-scores of the variables
###we will only use vars on the original scale

###core function to generate imputations is mcmcNorm

?mcmcNorm 

###we need to specify starting values
###best to use starting values from EM algorithm
###EM algorithm can be run using the function emNorm

?emNorm

em.res<-emNorm(cbind(hgt,wgt)~reg+age+sex,data=fdgs)

summary(em.res)
attributes(em.res)

em.res$miss.patt
em.res$miss.patt.freq
em.res$em.worst.ok
em.res$worst.frac
em.res$worst.linear.coef
em.res$msg

##now we are ready to run the imputations

imp<-mcmcNorm(em.res,iter=5000,impute.every=100,seeds=c(123,456))

summary(imp)
attributes(imp)

###imputed datasets are stored in imp.list

length(imp$imp.list)

###first imputed dataset
head(imp$imp.list[[1]])


###monitor convergence

###need to check the dimension of the parameters
dim(imp$series.beta)
dim(imp$series.sigma)


#trace plots
plot(imp$series.beta[,1:7])
plot(imp$series.beta[,8:14])
plot(imp$series.sigma)
plot(imp$series.worst)

###autocorrelation plots for regression coefficients
par(mfrow=c(1,7))
for(i in 1:7){
	acf(imp$series.beta[,i],
		main=colnames(imp$series.beta)[i])
	}

for(i in 8:14){
	acf(imp$series.beta[,i],
		main=colnames(imp$series.beta)[i])
	}

###autocorrelation plots for covariance matrix
par(mfrow=c(1,3))
for(i in 1:3){
	acf(imp$series.sigma[,i],
		main=colnames(imp$series.sigma)[i])
	}

###same for worst linear function
par(mfrow=c(1,1))
acf(imp$series.worst)
