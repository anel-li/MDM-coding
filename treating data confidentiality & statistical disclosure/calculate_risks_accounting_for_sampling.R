########disclosure risk accounting for the sampling uncertainty

###load population and synthetic data

###load F.j.hat.estimator

###draw new sample from cpspop and treat this as the information available 
###to the intruder

set.seed(1234)
intruder.data<-cpspop[sample(1:nrow(cpspop),5000,replace=FALSE),c(4,7,8)]

####how many records are in both files?

sum(duplicated(c(row.names(intruder.data),row.names(cps))))

######estimate the number of matches in the population for all synthetic datasets

F.j.hat<-F.j.estimator(syn.data)

m<-length(syn.data)
c.j<-rep(NA,nrow(intruder.data))
I.j<-rep(NA,nrow(intruder.data))
j.outside<-rep(NA,nrow(intruder.data))
outside.highest<-rep(NA,nrow(intruder.data))

for(i in 1:nrow(intruder.data)){

	print(i)

	match.prob<-array(NA,dim=c(nrow(syn.data[[1]]),m))
	for (j in 1:m){

###identify all matching records
		match<-(intruder.data$sex[i]==syn.data[[j]]$sex&
                intruder.data$race[i]==syn.data[[j]]$race&
                intruder.data$age[i]==round(syn.data[[j]]$age,0))
		
###get F.j
		F.j<-F.j.hat[match,j][1]

###if more than one match, intruder would pick one at random
		match.prob[,j]<-ifelse(match,min(1/F.j,1/sum(match)),0)
		}

####calculate P(J=j)
	pr.J<-apply(match.prob,1,mean)

###calculate c.j etc

	j.outside[i]<-1-sum(pr.J)
	outside.highest[i]<-(j.outside[i]>max(pr.J))
	c.j[i]<-length(pr.J[pr.J==max(pr.J)])
	I.j[i]<-any(row.names(cps[pr.J==max(pr.J),])==row.names(intruder.data[i,]))
	}

###match only if pr(outside) not highest
K<-(c.j[outside.highest==FALSE]*I.j[outside.highest==FALSE]==1)
F<-(c.j[outside.highest==FALSE]*(1-I.j[outside.highest==FALSE])==1)
s<-length(c.j[c.j==1&is.na(c.j)==FALSE&outside.highest==FALSE])

####expected match risk
sum(1/c.j[outside.highest==FALSE]*I.j[outside.highest==FALSE])

###true match rate

sum(na.omit(K))/s

###false match rate

sum(na.omit(F))/s

###match if pr(outside) is below a given threshold gamma
gamma<-0.9

K<-(c.j[j.outside<gamma]*I.j[j.outside<gamma]==1)
F<-(c.j[j.outside<gamma]*(1-I.j[j.outside<gamma])==1)
s<-length(c.j[c.j==1&is.na(c.j)==FALSE&j.outside<gamma])

####expected match risk
sum(1/c.j[j.outside<gamma]*I.j[j.outside<gamma])

###true match rate

sum(na.omit(K))/s

###false match rate

sum(na.omit(F))/s





		