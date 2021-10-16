############################################
########CPS DATA EXAMPLE USING CART#########
############################################

#####imputation function based on regression trees

tree.impute<-function (thetree,imputed.data,cat) 
{
###get all values of the dependent variable
fakeydata <- thetree$y

###find out in which leave each record ends in the synthetic data
newdatawhere <- predict(thetree, imputed.data, "where")

###count the number of records in each leaf
newtreetable <- table(newdatawhere)

###get leave names from the original data and the synthetic data
treenodes <- names(newtreetable)
oldtreenodes <- names(table(thetree$where))


###loop over all leaves
for(i in 1:length(treenodes)) {

###candidate values for imputation are all values that are observed
###in this leave in the original tree

###check whether terminal leaf in synthetic data is also terminal leaf
###in original data.
###If yes, draw from that leaf
###if not, snip the original tree to make it terminal leaf
 
	if(is.element(treenodes[i], oldtreenodes))
      	eligibles <- thetree$y[thetree$where == treenodes[i]]
	else {

###get number for the terminal leaf in the synthetic data
      	leaves <- row.names(thetree$frame)[as.numeric(treenodes[i])]

#snip the original tree to ensure that this leaf is also a terminal leaf
#based on the original data
            temptree <- snip.tree(thetree, leaves)

#get original values that end up that leaf as eligibles
            eligibles <- temptree$y[temptree$where ==which(row.names(temptree$frame)==leaves)]
		}

###if variable is categorical, take Bayesian Bootstrap in each leaf
	if (cat){
		fakeydata[newdatawhere == treenodes[i]] <- bayesianboot(eligibles, newtreetable[i])
		}

	else{

###if variance is very small, take Bayesian Bootstrap
###(data already protected by k-anonymity)

		tempo <- bayesianboot(eligibles, length(eligibles))
      	if(var(tempo) < 0.001){
			fakeydata[newdatawhere == treenodes[i]] <- bayesianboot(eligibles, newtreetable[i])
			}
		else{
###use a kernel density estimator on a Bayesian Bootstrap sample
###and sample new Ys from it
			xdens <- density(tempo,n= 10000, from = min(eligibles), to = max(eligibles))
			xdens$y <- xdens$y/sum(xdens$y)
	      	fakeydata[newdatawhere == treenodes[i]] <- sample(xdens$x,
      			size = newtreetable[i], replace = T, prob = xdens$y)
			}
		}
	}
return(fakeydata)
}


####Bayesian Bootstrap Function
bayesianboot<-function(eligibles, n)
{
         a <- sort(runif(length(eligibles) - 1))
         values <- sample(eligibles, n, T, c(a, 1) - c(0, a))
         values
}

##################
#synthesis code###
##################

library(tree)

###load data

library(tree)
#####variables to be protected: income, age, sex
m<-10
n<-nrow(cps)

###transform data by taking the cubic root

data.org<-cps
data.org$income<-data.org$income^(1/3)

#########
#obtain trees from the original data
#########

####tree for sex
sex.org<-tree(sex~.-age-income+I(tax>0),data=data.org,
     control=tree.control(nrow(data.org),mincut=5,minsize=10,mindev=0.001))

####tree for age
age.org<-tree(age~.-income+I(tax>0),data=data.org,
     control=tree.control(nrow(data.org),mincut=5,minsize=10,mindev=0.001))

####tree for income
income.org<-tree(income~.+I(tax>0),data=data.org,
     control=tree.control(nrow(data.org),mincut=5,minsize=10,mindev=0.001))

###generate synthetic data 

syn.data<-list(rep(cps),m)

for (i in 1:m){

###store original dataset to be replaced later
	syn.data[[i]]<-data.org



###sex

###generate new data
	sex.syn<-tree.impute(thetree=sex.org,imputed.data=syn.data[[i]],cat=TRUE)
	syn.data[[i]]$sex<-sex.syn

###age

	age.syn<-tree.impute(thetree=age.org,imputed.data=syn.data[[i]],cat=FALSE)
	syn.data[[i]]$age<-age.syn


###income

	income.syn<-tree.impute(thetree=income.org,imputed.data=syn.data[[i]],cat=FALSE)
	syn.data[[i]]$income<-income.syn^3
	}


