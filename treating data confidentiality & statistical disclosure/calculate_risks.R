########disclosure risk

###assume intruder has information who participated in the survey
###and knows the sex, race, and age for the first 100 records

###load synthetic and original data

c.j<-rep(NA,100)
I.j<-rep(NA,100)
m<-length(syn.data)

for(i in 1:100){

	print(i)

	match.prob<-array(NA,dim=c(nrow(syn.data[[1]]),m))
	for (j in 1:m){

###identify all matching records
		match<-(cps$sex[i]==(syn.data[[j]]$sex)&
               cps$race[i]==syn.data[[j]]$race&cps$age[i]==syn.data[[j]]$age)
		

###if more than one match, intruder would pick one at random
		match.prob[,j]<-ifelse(match,1/sum(match),0)
		}

####calculate P(J=j)
	pr.J<-apply(match.prob,1,mean)

###calculate c.j etc

	c.j[i]<-length(pr.J[pr.J==max(pr.J)])
	I.j[i]<-(pr.J[i]==max(pr.J))
	}
K<-(c.j*I.j==1)
F<-(c.j*(1-I.j)==1)
s<-length(c.j[c.j==1&is.na(c.j)==FALSE])

####expected match risk
sum(1/c.j*I.j)

###true match rate

sum(na.omit(K))/s

###false match rate

sum(na.omit(F))/s





		