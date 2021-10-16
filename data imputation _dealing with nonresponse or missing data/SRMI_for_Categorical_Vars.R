###Multiple Imputation using mice

library(mice)
library(rpart)

##load data
load("C:\\Users\\BA_Standalone\\Documents\\IPSM\\Multiple_Imputation\\R\\cpsdata_missing.RData")

##inspect missing data pattern

md.pattern(cps.mis) ###only useful for small datasets
md.pairs(cps.mis)



###make sure categorical variables are coded as factors
mis.data<-cps.mis
str(mis.data)
mis.data$educ<-as.factor(mis.data$educ)
mis.data$marital<-as.factor(mis.data$marital)
mis.data$race<-as.factor(mis.data$race)

###run imputation
##get default predictor matrix
ini<-mice(mis.data,method=c("","norm","","norm","","","polyreg","","norm"),maxit=0,print=FALSE)

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
###adjust predictor matrix (exclude csp in imputation model for age)
pred["age","csp"]<-0
pred


cps.imp<-mice(mis.data,method=ini$meth,pred=pred,seed=1234,m=5)

###any problems?
cps.imp$loggedEvents

###monitor convergence

plot(cps.imp,c("income","age","race"))
plot(cps.imp,c("ss"))

###specify imputation model
form<-ini$form
form
form[7]<-"~.+marital:educ"
form

cps.imp2<-mice(mis.data,method=ini$meth,pred=pred,form=form,seed=1234,m=3,maxit=2)
cps.imp2$loggedEvents
###use cart instead
meth.new<-ini$method
meth.new
meth.new[7]<-"cart2"

##Note: We need to use our own modified function "cart2" here, because of a 
##bug in the original mice code. Hopefully, this will be fixed soon and you
##can simply use "cart" instead.

cps.imp3<-mice(mis.data,method=meth.new,pred=pred,form=form,seed=1234,m=5,cp=0.0001)
