load("F:/sic_asus/IPSDS/SURV725_Item Nonresponse and Imputation_fall 2018/assignments/cps_files.RData")

#FIRST ATTEMPT TO DETECT ANOMALITIES
plot(cps$educ,cps$income)
plot(cps.mis$educ,cps.mis$income)
plot(cps.imp1$educ,cps.imp1$income)
plot(cps.imp2$educ,cps.imp2$income)
plot(cps.imp3$educ,cps.imp3$income)
plot(cps.imp4$educ,cps.imp4$income)

hist(cps$income, breaks=100)
hist(cps.mis$income, breaks=100)
hist(cps.imp1$income, breaks=100)
hist(cps.imp2$income, breaks=100)
hist(cps.imp3$income, breaks=100)
hist(cps.imp4$income, breaks=100)

#DESCRIPTIVE STATS
summary(cps$income)
summary(cps.mis$income)
summary(cps.imp1$income)
summary(cps.imp2$income)
summary(cps.imp3$income)
summary(cps.imp4$income)

table(cps$educ, useNA = "always")
table(cps.mis$educ, useNA = "always")

table(cps.mis$income)
summary(cps.imp1$income)
summary(cps.imp2$income)
summary(cps.imp3$income)
summary(cps.imp4$income)

table(cps$educ)

#FIRST PREDICTIVE STATS, ANOVA
summary(aov(income ~ educ, data=cps ))

#STARTING A TABLE IN ORDER TO COMPARE THE DATASETS (ALL DATASETS)
# Original data: 
#mean and variance of the mean
mean.org<-mean(cps[,1])
mean.org
var.mean.org<-var(cps[,1])/nrow(cps)
var.mean.org
#regression coeff 
reg<-lm(cps[,1]~cps[,2])
beta.org<-summary(reg)$coef[2,1]
var.beta.org<-summary(reg)$coef[2,2]^2
beta.org
var.beta.org

# missing data: 
#mean and variance of the mean
mean.mis<-mean(cps.mis[,1],na.rm = "T")
mean.mis
var.mean.mis<-var(cps.mis[,1],na.rm = "T")/sum(is.na((cps.mis)))
var.mean.mis
#regression coeff 
reg<-lm(cps.mis[,1]~cps.mis[,2])
beta.mis<-summary(reg)$coef[2,1]
var.beta.mis<-summary(reg)$coef[2,2]^2
beta.mis
var.beta.mis

# imp1: 
#mean and variance of the mean
mean.imp1<-mean(cps.imp1[,1])
mean.imp1
var.mean.imp1<-var(cps.imp1[,1])/nrow(cps.imp1)
var.mean.imp1
#regression coeff 
reg<-lm(cps.imp1[,1]~cps.imp1[,2])
beta.imp1<-summary(reg)$coef[2,1]
var.beta.imp1<-summary(reg)$coef[2,2]^2
beta.imp1
var.beta.imp1

# imp2: 
#mean and variance of the mean
mean.imp2<-mean(cps.imp2[,1])
mean.imp2
var.mean.imp2<-var(cps.imp2[,1])/nrow(cps.imp2)
var.mean.imp2
#regression coeff 
reg<-lm(cps.imp2[,1]~cps.imp2[,2])
beta.imp2<-summary(reg)$coef[2,1]
var.beta.imp2<-summary(reg)$coef[2,2]^2
beta.imp2
var.beta.imp2

# imp3: 
#mean and variance of the mean
mean.imp3<-mean(cps.imp3[,1])
mean.imp3
var.mean.imp3<-var(cps.imp3[,1])/nrow(cps.imp3)
var.mean.imp3
#regression coeff 
reg<-lm(cps.imp3[,1]~cps.imp3[,2])
beta.imp3<-summary(reg)$coef[2,1]
var.beta.imp3<-summary(reg)$coef[2,2]^2
beta.imp3
var.beta.imp3

# imp4: 
#mean and variance of the mean
mean.imp4<-mean(cps.imp4[,1])
mean.imp4
var.mean.imp4<-var(cps.imp4[,1])/nrow(cps.imp4)
var.mean.imp4
#regression coeff 
reg<-lm(cps.imp4[,1]~cps.imp4[,2])
beta.imp4<-summary(reg)$coef[2,1]
var.beta.imp4<-summary(reg)$coef[2,2]^2
beta.imp4
var.beta.imp4

summary(reg)

test <- lm(cps.imp4$income~cps.imp4$educ)
summary(test)


## DETECTING MISSING CASES IN ORDER TO FOLLOW THESE CASES AND DETECT PATTERNS
m <- is.na(cps.mis$income)
# I add A flag-variable to the imputed datasets
#imp1
cps.imp1m <- cps.imp1
cps.imp1m$m <- m
head(cps.imp1m)
#imp2
cps.imp2m <- cps.imp2
cps.imp2m$m <- m
#imp3
cps.imp3m <- cps.imp3
cps.imp3m$m <- m
#imp4
cps.imp4m <- cps.imp4
cps.imp4m$m <- m

# subset of imputed datasets containing ONLY imputed values
imp1m <- subset(cps.imp1m, m==T)
imp2m <- subset(cps.imp2m, m==T)
imp3m <- subset(cps.imp3m, m==T)
imp4m <- subset(cps.imp4m, m==T)

# frequency tables of imputed values
table(imp1m$income)
table(imp2m$income)
table(imp3m$income)
table(imp4m$income)

