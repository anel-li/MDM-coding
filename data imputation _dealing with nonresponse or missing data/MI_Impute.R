###Multiple Imputation using mice

library(mice)

##load data
load("C:\\Users\\BA_Standalone\\Documents\\IPSM\\Item_NR_and_Imputation\\R\\cpsdata_missing.RData")


##inspect missing data pattern

md.pattern(cps.mis) ###only useful for small datasets
md.pairs(cps.mis)


###preprocess data 
###don't want skewed variables
hist(cps$income,breaks=100)
hist(log(cps$income),breaks=100)
hist(cps$income^(1/3),breaks=100)

cps.mis$tax<-cps.mis$tax^(1/3)
cps.mis$income<-cps.mis$income^(1/3)
cps.mis$csp<-cps.mis$csp^(1/3)
cps.mis$ss<-cps.mis$ss^(1/3)
cps.mis$educ<-as.factor(cps.mis$educ)
cps.mis$race<-as.factor(cps.mis$race)

###run imputation
##get default predictor matrix
ini<-mice(cps.mis,method=c("","norm","","norm","","","polyreg","","norm"),maxit=0,print=FALSE)

ini
attributes(ini)

###possible options for method:

#pmm 			Predictive mean matching (any)
#norm 		Bayesian linear regression (numeric)
#norm.nob 		Linear regression ignoring model error (numeric)
#norm.boot 		Linear regression using bootstrap (numeric)
#norm.predict	Linear regression, predicted values (numeric)
#mean 		Unconditional mean imputation (numeric)
#2l.norm 		Two-level normal imputation (numeric)
#2l.pan 		Two-level normal imputation using pan (numeric)
#2lonly.mean 	Imputation at level-2 of the class mean (numeric)
#2lonly.norm 	Imputation at level-2 by Bayesian linear regression (numeric)
#2lonly.pmm		Imputation at level-2 by Predictive mean matching (any)
#quadratic 		Imputation of quadratic terms (numeric)
#logreg 		Logistic regression (factor, 2 levels)
#logreg.boot 	Logistic regression with bootstrap
#polyreg 		Polytomous logistic regression (factor, >= 2 levels)
#polr			Proportional odds model (ordered, >=2 levels)
#lda 			Linear discriminant analysis (factor, >= 2 categories)
#cart			Classification and regression trees (any)
#rf 			Random forest imputations (any)
#ri 			Random indicator method for nonignorable data (numeric)
#sample 		Random sample from the observed values (any)
#fastpmm 		Experimental: Fast predictive mean matching using C++ (any)

###form allows to specify individual formulas for different imputation models


###pad is padded dataset, i.e. dataset with all generated dummies etc.

#ini$loggedEvents ###documents if steps were necessary to keep the programm
				##running, e.g. removing collinear or constant 
				###variables, changing from ordered logit to ployreg  


###adjust predictor matrix

pred<-ini$pred
pred
###adjust predictor matrix
pred["race",]<-c(1,1,1,1,1,0,0,1,1)
pred

###specify imputation model
form<-ini$form
form[2]<-"~tax+csp+age+I(age^2)+educ+marital+race+sex+ss"


cps.imp<-mice(cps.mis,method=ini$meth,form=form,pred=pred,seed=1234)


####compare results

###univariate distribution

stacked.data<-complete(cps.imp,"long",inc=F)
summary(cps$income)
summary(cps.mis$income^3)
summary(stacked.data$income^3)


###some problems

###problems with implausible values
summary(stacked.data$age)

###problems recovering the distribution
first.imp<-complete(cps.imp,1)
par(mfrow=c(1,2))
hist(cps$ss,breaks=100)
hist(first.imp$ss[is.na(cps.mis$ss)]^3,breaks=100)