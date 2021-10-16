#######################################################################################
# Lab Exercise A: Comparing Simple Plots
######################################################################################

# Let's Create Fake Polling Data 
# The numbers are percentages of respondents who see a party 
# as being competent in three policy areas. In addition, we create a 
# variable for which party respondents support.

party <- c("Conservative", "Liberal", "Green", "Socialist")
economy <- c(65, 40, 52, 28)
social <- c(36, 38, 8, 73)
foreign <- c(40, 35, 10, 5)
support <- c(38, 35, 12, 15)

poll <- cbind(economy, social, foreign, support)
rownames(poll) <- party


# Using this polling data, compare the perceived policy
# competence of the four political parties:
#
# 1. Start out by comparing economic and social policy.
# 2. Compare different versions of barcharts and dotplots.
#    Summarize and discuss what you see in each plot type. Which is better and why?
# 3. Now plot the same data using line charts. 
#    Again, summarize and discuss: which plot type is the best and why?
# 4. Add foreign policy and repeat.
# 5. Optional: You may also change the distributions of the polling data and repeat.
#
#  If you are new to R, have a look at the following video
#  on the course site: 
#  "surv752-code-03-Adding_a_Second_Variable"
#
# You may also find the following functions helpful:
#
# barplot()
# dotchart()
# 
#######################################################################################
# Lab Exercise B: Graph Redesign
######################################################################################

# Use the following freedom house disaggregated dataset 2003-2014 which provides
# "political rights ratings" (pr_20XX) and "civil liberties ratings" (cl_20XX) for 
# all countries across the globe. Note that the dataset is in "wide" format.

library(foreign)
data <- read.csv("YOURPATH/FH_disaggregated_03_14.csv", sep=";")


# 1. Using these data, mimic four of the six (more or less) famous graphic designs 
#    you find on the handout. Use the political rights variable to do this.
#
#   I am providing code to help you with the first steps further below – 
#   but use it only when you are stuck yourself!
#
# 2. Now, come up with your own re-design according to the principles we 
#    have discussed in the course.
#
#  If you are new to R, have a look at the following two videos
#  on the course site: 
#  "surv752-code-04-Changing_Elements_of_a_Graphic"
#  "surv752-code-05-Adding_Elements_to_an_Exisiting_Plot"
#
#####################################################################################
# First, some data preparation
# Turn variables into numeric variable type
for(i in 2:25){
  data[,i] <- as.numeric(data[,i])  
}

# Define some paramaters used for the plots later 
startYear <- 2003
endYear <- 2014

maxVal <- max(data[,2:13], na.rm=T) + 1
minVal <- min(data[,2:13], na.rm=T) - 1


# Optional: just sample 30 countries 
data <- data[sample(1:197, 30),]

# Now Plot a Simple Line Chart of Political Rights over Time
par(mar=c(3,3,2,2), yaxs="r")

plot(0, 0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), pch="",  ylab="Political Rights Score", xlab="Year")

for(i in 1:197){
  lines(c(startYear:endYear), data[i,2:13])
}

lines(c(startYear:endYear), data[which(data$Country=="United Kingdom"),2:13], col="red", lwd=3)
lines(c(startYear:endYear), data[which(data$Country=="Germany"),2:13], col="yellow", lwd=3)
lines(c(startYear:endYear), data[which(data$Country=="Mexico"),2:13], col="green", lwd=3)

######################################################################################






