library(mice)

###load data
data(fdgs) ###data comes with mice package
names(fdgs)

###need to exclude "id", "hgt.z", and "wgt.z" as predictors
###core function in mice is "mice"
fdgs.red<-fdgs[,2:6]
names(fdgs.red)
imp.data<-mice(fdgs[,2:6],m=50,maxit=10,method=c("","","","norm","norm"),
	seed=1234)

attributes(imp.data)

dim(imp.data$imp$hgt)
###imp is a named list containing only the imputed values for each variable

dim(imp.data$chainMean)

plot(imp.data)


