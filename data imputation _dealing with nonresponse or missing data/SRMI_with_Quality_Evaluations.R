library(mice)
library(lattice)

##load data
load("C:\\Users\\BA_Standalone\\Documents\\IPSM\\Multiple_Imputation\\R\\cpsdata_missing.RData")

mis.data<-cps.mis
mis.data$educ<-as.factor(mis.data$educ)
mis.data$marital<-as.factor(mis.data$marital)
mis.data$race<-as.factor(mis.data$race)


###use imputation methods from previous example
cps.imp<-mice(mis.data,method=c("","norm","","norm","","","polyreg","","norm"))
bwplot(cps.imp)
densityplot(cps.imp)

summary(cps$income)
summary(cps.imp$imp$income)

summary(cps$age)
summary(cps.imp$imp$age)

summary(cps$ss)
table(cps$ss==0)

summary(cps.imp$imp$ss)
table(cps.imp$imp$ss==0)

###check models

##income
reg.income<-lm(income~.,data=mis.data)
plot(reg.income)
hist(mis.data$income,breaks=100)

###better to transform the data before imputation
hist(log(mis.data$income),breaks=100)
hist(mis.data$income^(1/3),breaks=100)

mis.data$tax<-mis.data$tax^(1/3)
mis.data$income<-mis.data$income^(1/3)
mis.data$csp<-mis.data$csp^(1/3)
mis.data$ss<-mis.data$ss^(1/3)

reg.income<-lm(income~.,data=mis.data)
plot(reg.income)

##age
reg.age<-lm(age~.,data=cps.mis)
plot(reg.age)

reg.age<-lm(age~.,data=mis.data)
plot(reg.age)

###ss
reg.ss<-lm(ss~.,data=mis.data)
plot(reg.ss)

###transformation alone does not help
###better to use semicontinuous imputation
###needs some tweaking in mice

###add two variables for ss to address semi-continuity

ss.ind<-ifelse(mis.data$ss>0,1,0)
ss.ind[is.na(mis.data$ss)]<-NA
ss.cont<-ifelse(mis.data$ss>0,mis.data$ss,NA)

mis.data<-cbind(mis.data,ss.ind=as.factor(ss.ind),ss.cont)

ini<-mice(mis.data,method=c("","norm","","norm","","","polyreg","","~I(as.numeric(levels(ss.ind))[ss.ind]*ss.cont)","logreg","norm"),maxit=0,print=FALSE)


###adjust predictor matrix
pred<-ini$pred
pred
pred[,"ss.ind"]<-0
pred["ss.ind","ss"]<-0
pred["ss.cont",]<-c(1,1,1,1,1,1,1,1,0,0,0)
pred

###adjust visiting sequence
visit<-ini$visit
visit
visit[4]<-10
visit[5]<-11
visit<-c(visit,ss=9)
visit
names(visit)[4:5]<-c("ss.ind","ss.cont")
visit

###now impute the data again

cps.imp2<-mice(mis.data,method=ini$method,pred=pred,visit=visit)


###check data quality again
bwplot(cps.imp2)
densityplot(cps.imp2)

summary(cps$income[is.na(cps.mis$income)])
summary(cps.imp2$imp$income^3)

summary(cps$age[is.na(cps.mis$age)])
summary(cps.imp2$imp$age)

table(cps$ss[is.na(cps.mis$ss)]==0)
table(cps.imp2$imp$ss==0)/5

summary(cps$ss[is.na(cps.mis$ss)&cps$ss!=0])
summary(cps.imp2$imp$ss[cps.imp2$imp$ss!=0]^3)

###ad-hoc fix for low age values and negative income: post command
post<-ini$post
post["age"]<-"imp[[j]][,i]<-squeeze(imp[[j]][,i],c(13,100))"
post["income"]<-"imp[[j]][,i]<-squeeze(imp[[j]][,i],c(0,Inf))"

cps.imp3<-mice(mis.data,method=ini$method,pred=pred,visit=visit,post=post)

summary(cps.imp3$imp$age)

###caveat: easy fix, but does not address problem of model mis-specification
