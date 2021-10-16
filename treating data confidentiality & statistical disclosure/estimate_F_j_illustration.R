
########################################
########disclosure risk evaluations#####
########################################

###estimate the number of unique combinations in the population


##########	match on Age,Race,Sex

#load population and sample data



###estimate the number of unique combinations in the population
attach(cps)

###crosstabulate age x rage x sex in the sample
freq<-xtabs(~age+race+sex)

####crosstabulate age x rage x sex in the population for comparison
pop.freq<-xtabs(~cpspop$age+cpspop$race+cpspop$sex)

###fit all-two-way log-linear model to the crosstabulation in the sample
fit<-loglin(freq,list(c(1,2),c(1,3),c(2,3)),param=T,fit=T)

###calculate theta (the inverse of the the number of records for each cell)
sampling.prob<-nrow(cps)/nrow(cpspop)
theta<-1/((1-sampling.prob)/sampling.prob*fit$fit)*
      (1-exp(-(1-sampling.prob)/sampling.prob*fit$fit))
F.j.est<-NULL
F.j.true<-NULL
for (i in 1:nrow(cps)){
	F.j.est[i]<-1/theta[age=(age[i]-14),race=race[i],sex=sex[i]]
	F.j.true[i]<-pop.freq[age=(age[i]-14),race=race[i],sex=sex[i]]
	}	
print(summary(F.j.est))
print(summary(F.j.true))
plot(F.j.est,F.j.true)
detach(cps)
