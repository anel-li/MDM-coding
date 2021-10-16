library(synthpop)

###load data ("cps5000.RData")

###synthesize data using default values
test<-syn(cps,m=3)

###inspect resulting object
names(test)

test$method

###run parametric synthesis with synpop
m<-5

org.data<-cps
org.data$income<-org.data$income^(1/3)
org.data$tax<-org.data$tax^(1/3)
org.data$csp<-org.data$csp^(1/3)
org.data$ss<-org.data$ss^(1/3)


method.ini<-c("","norm","","norm","","","","logreg","")
visit.ini<-c(1,3,5,6,7,9,8,4,2)
syn.data1<-syn(org.data,m=m,method=method.ini,visit.sequence=visit.ini,
  drop.pred.only=FALSE)

#constrained imputation not implemented yet -> will contain some negative values
summary(syn.data1$syn[[1]])

###nonparametric
method.ini2<-c("","ctree","","ctree","","","","ctree","")
smoothing.ini2<-list("density")
names(smoothing.ini2)<-"income"
syn.data2<-syn(org.data,m=m,method=method.ini2,visit.sequence=visit.ini,
  smoothing=smoothing.ini2,drop.pred.only=FALSE)

###evaluate quality (only based on first replicate)
compare.synds(syn.data1,org.data,vars=c("income","age","sex"))

###run regression model 

##change educ to continuous
syn1.adj<-syn.data1
syn2.adj<-syn.data2

for(i in 1:m){
	syn1.adj$syn[[i]]$educ<-
         as.numeric(levels(syn1.adj$syn[[i]]$educ))[syn1.adj$syn[[i]]$educ]
	syn2.adj$syn[[i]]$educ<-
         as.numeric(levels(syn2.adj$syn[[i]]$educ))[syn2.adj$syn[[i]]$educ]
	}

cps.adj<-org.data
cps.adj$educ<-as.numeric(levels(cps.adj$educ))[cps.adj$educ]


reg.syn1<-lm.synds(log(income^3)~educ
	+race+as.factor(sex)+age+I(tax>0),data=syn1.adj)

compare.fit.synds(reg.syn1,cps.adj)


reg.syn2<-lm.synds(log(income^3)~educ
	+race+as.factor(sex)+age+I(tax>0),data=syn2.adj)

compare.fit.synds(reg.syn2,cps.adj)
