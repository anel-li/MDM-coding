##############################################
#######Analyzing Synthetic Datasets###########
##############################################

###load "syndata.RData"

###contains two datasets
###cps 	original data (sample of 5,000 records from the public use microdata
###		of the current population survey 2000
###syn.data list of 10 synthetic datasets (income, age, and sex are synthesized)


###inspect original data
summary(cps)

###csp 	child support payment
###educ	education with 16 categories ranging from 31 to 46
###ss		social security payments
###sex	2=female
###race     1=white
###         2=black
###         3=American Indian
###         4=Asian
###marital	marital status with 7 categories
###		1 Married, spouse present 
###		2 Married, spouse absent
###		3 Separated
###		4 Divorced
###		5 Widowed
###		6 Never married/single
###		7 Unknown

###compare to one of the synthetic datasets

summary(syn.data[[1]])

#############
###analysis of the synthetic data
#############

########simple analysis###########

####quantity of interest: average income

###get point and variance estimate from each synthetic dataset

m<-length(syn.data)
n<-nrow(cps)

q.i<-rep(NA,m)
u.i<-rep(NA,m)

for(i in 1:m){
	q.i[i]<-mean(syn.data[[i]]$income)
	u.i[i]<-var(syn.data[[i]]$income)/n
	} 

###more efficient
q.i<-sapply(syn.data,function(x) mean(x$income))
u.i<-sapply(syn.data,function(x) var(x$income)/nrow(x))

###get necessary quantities to compute final variance
u.bar<-mean(u.i)
b.m<-var(q.i)

###get final estimates
q.bar<-mean(q.i)
T.p<-u.bar+b.m/m

###compute the degrees of freedom
dfs<-(m-1)*(1+u.bar/(b.m/m))^2

###compute 95% confidence intervals
l.95.syn<-q.bar-qt(0.975,df=dfs)*sqrt(T.p)
u.95.syn<-q.bar+qt(0.975,df=dfs)*sqrt(T.p)

###compare results to original data
q.org<-mean(cps$income)
T.org<-var(cps$income)/nrow(cps)
l.95.org<-q.org-1.96*sqrt(T.org)
u.95.org<-q.org+1.96*sqrt(T.org)

syn.res<-cbind(l.95.syn,q.bar,u.95.syn)
org.res<-cbind(l.95.org,q.org,u.95.org)

res<-rbind(org.res,syn.res)
dimnames(res)<-list(c("org","syn"),c("lower95","mean.inc","upper95"))
print(res)


######more complex analysis##########

###quantities of interest:
###  - average income by gender
###  - average age by gender
###  - returns to schooling

###write analysis function that stores all point and variance estimates
###(to be applied to all datasets)
  
func<-function(data){
 
###descriptive statistics
inc.mean<-as.numeric(by(data$income,data$sex,mean))
inc.var<-as.numeric(by(data$income,data$sex,var)/table(data$sex))
inc.results<-cbind(inc.mean,inc.var)

age.mean<-as.numeric(by(data$age,data$sex,mean))
age.var<-as.numeric(by(data$age,data$sex,var)/table(data$sex))
age.results<-cbind(age.mean,age.var)

###regression
###turn education variable into continuous variable
###(not strictly valid)

data$educ<-as.numeric(levels(data$educ))[data$educ]

reg<-lm(log(income)~educ+race+as.factor(sex)+age,data=data)
reg.results<-cbind(summary(reg)$coef[,1],summary(reg)$coef[,2]^2)

results<-rbind(inc.results,age.results,reg.results)
dimnames(results)<-list(c("avrg.inc.m","avrg.inc.f","avrg.age.m","avrg.age.f",
	"Intercept","education","black","Am. Indian","Asian","female","age"),c("est","var"))
return(results)

}

###get estimates from synthetic data
###(generates a list of length m containing q_i and u_i)

syn.res<-sapply(syn.data,func,simplify="array")

###compute final estimates
q.bar<-apply(syn.res[,1,],1,mean)
u.bar<-apply(syn.res[,2,],1,mean)
b.m<-apply(syn.res[,1,],1,var)

T.p<-u.bar+b.m/m

###compute the degrees of freedom
dfs<-(m-1)*(1+u.bar/(b.m/m))^2

l.95.syn<-q.bar-qt(0.975,df=dfs)*sqrt(T.p)
u.95.syn<-q.bar+qt(0.975,df=dfs)*sqrt(T.p)
ci.length.syn<-u.95.syn-l.95.syn


###get estimates from original data

org.res<-func(cps)
l.95.org<-org.res[,1]-1.96*sqrt(org.res[,2])
u.95.org<-org.res[,1]+1.96*sqrt(org.res[,2])
ci.length.org<-u.95.org-l.95.org

###compare results
print(cbind(org.res[,1],q.bar),digits=3)
print(cbind(ci.length.org,ci.length.syn),digits=3)

###why are synthetic confidence intervals shorter for income?
qqplot(cps$income,syn.data[[1]]$income,
     ylim=c(min(cps$income,syn.data[[1]]$income),
      max(cps$income,syn.data[[1]]$income)))

qqplot(cps$income,syn.data[[1]]$income,
     ylim=c(min(cps$income,syn.data[[1]]$income),200000),
     xlim=c(min(cps$income,syn.data[[1]]$income),200000))
length(cps$income[cps$income>150000])
