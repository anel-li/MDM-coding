library(mice)


###load imputed data
load(file="C:\\Users\\BA_Standalone\\Documents\\IPSM\\Item_NR_and_Imputation\\R\\cpsdata_imputed.RData")
ls()

###inspect data
head(imp.data)

###turn data into "mids" object so that mice can be used
imp<-as.mids(imp.data,.imp=1, .id=2)

class(imp)

###mids stands for multiply imputed datasets

##inspect imp object

attributes(imp)

imp$call 		#would contain the call to mice if data were imputed with mice
summary(imp$data) 	#data contains the original data
imp$m				#number of imputations
imp$nmis			#info on number of missing obs per variable
summary(imp$imp$income) #imp contains only the imputed values
head(imp$pad[[1]])	#contains the data with dummies for factor variables

###all other attributes are only relevant if mice is used for imputation
###values are automatically set to their default values, which does not
###necessarily mean that the data was imputed based on these settings!!

###extracting individual datasets
imp1<-complete(imp,1)

###extract original data
org<-complete(imp,0)

###extract all imputations in long format
imp.long<-complete(imp,action="long")
head(imp.long)

###extract in wide format and include original data
imp.wide<-complete(imp,action="broad",include=TRUE)
head(imp.wide)

###store imputed variables sequentially
imp.wide2<-complete(imp,action="repeated",include=TRUE)
head(imp.wide2)

###analyzing the imputed data
res<-with(data=imp,lm(log(income)~age+I(age^2)+educ+race))

class(res)

##"mira"->multiple imputation repeated analysis

attributes(res)
res$call
res$call1
res$nmis
res$analyses

summary(res)

###to get the pooled results (using the MI combining rules) we need

final.res<-pool(res)
class(final.res)

##"mipo" ->multiple imputation pooled outcome
attributes(final.res)
final.res$call
final.res$call1
final.res$call2

#t is the final variance (not to be confused with the t in summary(final.res))
#r is the relative increase in variance due to nonresponse
#fmi is the fraction of missing information
#lambda is proportion of variation due to nonresponse ( (B+B/m)/T )

summary(final.res)

###pool() works for any command that returns coefficients
###and allows the vcov() command for obtaining the variance
###matrix for the coefficients
###with() works with any regular R expression

###example

race.tab<-with(imp,table(race))
race.tab$analyses

###but pooling doesn't work
pool(race.tab)

###pooling also doesn't work directly for computing the means

###workaround:
mean.inc<-(with(imp,lm(income~1)))
summary(pool(mean.inc))

###compare with original data
mean(org$income,na.rm=TRUE)

###missingness was not MCAR
