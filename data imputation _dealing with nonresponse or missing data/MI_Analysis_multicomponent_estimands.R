#########################
####testing constraints##
#########################

library(mice)


###load imputed data
load(file="C:\\Users\\BA_Standalone\\Documents\\IPSM\\Multiple_Imputation\\R\\cpsdata_imputed.RData")

###need to turn imputed data into mids object again
imp<-as.mids(imp.data,.imp=1, .id=2)


###command: pool.compare(fit1,fit0,data=NULL,method="Wald")

##############
###standard test: joint significance of the full model
##############

###use returns to schooling model again
res<-with(data=imp,lm(log(income)~age+I(age^2)+educ+race))

###estimate empty model
mean.inc<-(with(imp,lm(income~1)))


model.fit<-pool.compare(res,mean.inc)

attributes(model.fit)

##rm relativ increase due to nonresponse
model.fit$pvalue

###coefficients are significantly different from zero

###but Wald-test assumes equal fraction of missing information
###is this realistic?
pool(res)$fmi

###not realistic; can't use likelihood ratio test either since it
###only works for logistic regression in mice

###need to use the mitml package
library(mitml)

##need to transform mids obejct into mitml.list object and fit models again
imp.mitml<-mids2mitml.list(imp)

###use returns to schooling model again
res.mitml<-with(data=imp.mitml,lm(log(income)~age+I(age^2)+educ+race))

###estimate empty model
mean.inc.mitml<-(with(imp.mitml,lm(income~1)))


model.fit.lr<-testModels(res.mitml,mean.inc.mitml,method="D3")
attributes(model.fit.lr)
model.fit.lr$test

###could also do Wald test similar to mice
model.fit.wald<-testModels(res.mitml,mean.inc.mitml,method="D1")
model.fit.wald$test

###compare with mice
model.fit$Dm
model.fit$df1
model.fit$df2
model.fit$rm

###mitml also allows Wald test for small sample case
###(based on F distribution instead of Chi-Square distribution)

###requires the denominator degrees of freedom for the complete data, 
###i.e n-p, where p is the number of parameters in the unrestricted model

n<-nrow(imp$data)
p<-length(model.fit$qbar1)

model.fit.wald.adj<-testModels(res.mitml,mean.inc.mitml,method="D1",df.com=(n-p))
model.fit.wald.adj$test


#########
###other tests
#########

###assume we want to test whether age and age^2 are jointly different
###from zero 

###important requirement: variables that appear in both models need
###to be included first -> age and age^2 need to be the last variables

model1<-with(data=imp,lm(log(income)~educ+race+age+I(age^2)))
model0<-with(data=imp,lm(log(income)~educ+race))

pool.compare(model1,model0)$pvalue

pool.compare(model1,model0)$df1


#####testing other restrictions
###(only works with mitml using Wald test)

#asume we want to test whether the coefficient for race=3 and race=4 are equal
cons<-c("race3-race4")
testConstraints(res.mitml,constraints=cons)


##########################
##miscellaneous comands###
##########################

###computing a pooled R^2 statistic
###(based on Fisher's z-transformation)
pool.r.squared(model1) 

####################
###updating and changing variables
###################

###not straightforward
###if variable is imputed, we would need to update imp$data and imp$imp
###easier to extract data in long format, update, and define as.mids()
###again

###example: change educ to numeric

imp.long<-complete(imp,action="long",include=TRUE)
imp.long$educ<-as.numeric(levels(imp.long$educ))[imp.long$educ]

imp.new<-as.mids(imp.long,.imp=1, .id=2)


###run returns to schooling model again
res.new<-with(data=imp.new,lm(log(income)~educ+race+age+I(age^2)))
summary(pool(res.new))

###alternative: include statement in the expression passed to 'with'
expr<-expression(educ.cont<-as.numeric(levels(educ))[educ],
		lm(log(income)~educ.cont+race+age+I(age^2)))

res.new2<-with(data=imp,expr)
summary(pool(res.new2))

