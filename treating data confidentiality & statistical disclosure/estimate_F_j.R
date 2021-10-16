F.j.estimator<-function(syn.data){


sampling.prob<-nrow(syn.data[[1]])/nrow(cpspop)


F.j.est<-array(NA,dim=c(nrow(syn.data[[1]]),length(syn.data)))
for(i in 1:length(syn.data)){



###estimate the number of unique combinations in the population
##########	match on Age,Race,Sex

###estimate the number of unique combinations in the population
	attach(syn.data[[i]])

###crosstabulate age x rage x sex in the sample
	freq<-xtabs(~age+race+sex)

###fit all-two-way log-linear model to the crosstabulation in the sample
	fit<-loglin(freq,list(c(1,2),c(1,3),c(2,3)),param=T,fit=T)

###calculate theta (the inverse of the the number of records for each cell
	theta<-1/((1-sampling.prob)/sampling.prob*fit$fit)*
           (1-exp(-(1-sampling.prob)/sampling.prob*fit$fit))
	for (j in 1:nrow(syn.data[[i]])){
		F.j.est[j,i]<-1/theta[which(attributes(theta)$dimnames$age==age[j]),
                 which(attributes(theta)$dimnames$race==race[j]),
                 which(attributes(theta)$dimnames$sex==sex[j])]
		}	
	detach(syn.data[[i]])
	}
return(F.j.est)
}
