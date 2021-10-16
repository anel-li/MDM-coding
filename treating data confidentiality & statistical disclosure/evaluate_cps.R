#####################################
#####Analytical Validity Evaluations#
#####################################

eval<-function(data){
 
####quantities of interest
####mean income by gender
####mean age by gender
####returns to schooling

###descriptive statistics
income<-cbind(as.numeric(by(data$income,data$sex,mean)),
        as.numeric(by(data$income,data$sex,var)/table(data$sex)))
age<-cbind(as.numeric(by(data$age,data$sex,mean)),
     as.numeric(by(data$age,data$sex,var)/table(data$sex)))

###regression
if(is.factor(data$educ)){
	data$educ<-as.numeric(levels(data$educ))[data$educ]
	}
reg1<-lm(log(income)~educ+race+as.factor(sex)+age+I(tax>0),data=data)
res<-cbind(summary(reg1)$coef[,1],summary(reg1)$coef[,2]^2)

results<-rbind(income,age,res)
return(results)

}

###load original and synthetic data ("syndata.RData")



###get estimates from original data

org.res<-eval(cps)
l.95.org<-org.res[,1]-1.96*sqrt(org.res[,2])
u.95.org<-org.res[,1]+1.96*sqrt(org.res[,2])
ci.length.org<-u.95.org-l.95.org

###get estimates from synthetic data
m<-length(syn.data)
syn.temp<-array(NA,dim=c(nrow(org.res),ncol(org.res),length(syn.data)))
for (i in 1:length(syn.data)){
	syn.temp[,,i]<-eval(syn.data[[i]])
	}
q.bar<-apply(syn.temp[,1,],1,mean)
u.bar<-apply(syn.temp[,2,],1,mean)
b.m<-apply(syn.temp[,1,],1,var)

T.p<-u.bar+b.m/m

###compute the degrees of freedom
dfs<-(m-1)*(1+u.bar/(b.m/m))^2

l.95.syn<-q.bar-qt(0.975,df=dfs)*sqrt(T.p)
u.95.syn<-q.bar+qt(0.975,df=dfs)*sqrt(T.p)
ci.length.syn<-u.95.syn-l.95.syn

########compare the results

##compute the overlap
U.max.beta<-apply(cbind(u.95.syn,u.95.org),1,max)
L.max.beta<-apply(cbind(l.95.syn,l.95.org),1,max)
U.min.beta<-apply(cbind(u.95.syn,u.95.org),1,min)
L.min.beta<-apply(cbind(l.95.syn,l.95.org),1,min)
overlap.beta<-apply(cbind((U.max.beta-L.min.beta)-(U.max.beta-U.min.beta)-
              (L.max.beta-L.min.beta),0),1,max)

J.k.beta<-1/2*(overlap.beta/ci.length.org+overlap.beta/ci.length.syn)

beta.coef<-cbind(org.res[,1],q.bar,J.k.beta,ci.length.syn/ci.length.org)

print(beta.coef)



	