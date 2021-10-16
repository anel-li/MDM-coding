rm(list=ls())
library(haven)
library(tidyverse)
library(CGPfunctions)


load("ESS.RData")
long <- ESS$long
e <- ESS$e
clevel <- list()
for (i in seq(2002,2016,by=2)){
  # list of needed variables
  # I get the variable names like gdppc_2002 
  ## actual Corruption perception index c_ticpi_XXXX
  corrupt <- str_c("c_ticpi_",i)
  ## actual GDP per capita c_gdppc_XXX
  gdp <- str_c("c_gdppc_",i)
  ck$year <- i
  # vector with all needed variable names 
  neededvars <- c("cntry","year", corrupt, gdp)
  # database with only 7 contries + needed vars for actual year
  clevel0 <- ck[,neededvars]
  colnames(clevel0) <- c("cntry","year","corrupt","gdp")
  # I need a wave
  wave <- (i-2000)/2
  # list
  clevel[[wave]] <- clevel0
}

#f "adding" the data using rbind 
clevelDF <- NULL
for (i in seq_along(clevel)) {
  clevelDF <- rbind(clevelDF,clevel[[i]])
}
clevelDF

summary(clevelDF)

### A new table since I need now both: cntry and cntryName but no need now for region
vt3 <- long %>% group_by(cntry,cntryName,year) %>% 
  summarise(Proportion=mean(vote2n,na.rm=T)) %>% 
  mutate(Proportion = round(Proportion,3)) %>% 
  mutate(Proportion=Proportion*100) %>% 
  filter(cntry %in% c("BE","CZ","FR","IL","IT","PL","SI")) %>%
  select(cntry,cntryName,year,Proportion)
vt3

# I will try to merge vt3 and clevelDF now
clevelDATA <- inner_join(vt3,clevelDF, by=c("cntry","year"))
clevelDATA
clevelDATA <- as.data.frame(clevelDATA)
clevelDATA

##### scale change in 2011 at corruption!!!!!
# I separate the years before and after 2011
clevelDATA1 <- clevelDATA %>% filter(clevelDATA$year<2011)
clevelDATA1
clevelDATA2 <- clevelDATA %>% filter(clevelDATA$year>2011)
clevelDATA2
clevelDATA1$corrupt <- clevelDATA1$corrupt*10
clevelDATA <- rbind(clevelDATA1,clevelDATA2)
c <- ESS$c
evars <- ESS$evars

############ data description
setwd("C:/Panni/IPSDS/SURV665_RealWordDataAnalysis/assignments/final_project/results")

# new variable: year
long <- long %>% mutate(year = essround*2+2000)

# recoding age
long$ageg <- cut(long$agea,breaks=c(0,29.5,44.5,64.5,105),labels=c("under 30","30-44","45-65","65+"))
table(long$ageg,useNA="ifany")

# vote lables
long$vote <- factor(long$vote, levels = c(1,2,3), labels=c("Voted last national election","Not voted","Not eligible to vote"))
table(long$vote,useNA = "ifany")

# polintr labels
long$polintr <- factor(long$polintr, levels=c(1:4), labels=c("Very interested","Quite interested",
                                                             "Hardly interested","Not at all interested"))
#sclmeet labels
long$sclmeet <- factor(long$sclmeet, levels=c(1:7),
                       labels=c("Never","Less than one a month","Once a month","Several times a month",
                                "Once a week","Several times a week","Every day"))

#happy factor - labeled
long$happyf <- factor(long$happy,levels=c(0:10),labels=c("0-Extremely unhappy","1","2","3","4","5",
                                                         "6","7","8","9","10-Extremely happy"))

long$ppltrstf <- factor(long$ppltrst,levels=c(0:10),labels=c("0-You can't be too careful","1","2","3","4","5",
                                                            "6","7","8","9","10-Most people can be trusted"))

# I need a variable showing n-s by country + year
t1 <- table(long$cntryName,long$year,useNA = "ifany")
t1
write.csv2(t1,"t1.csv")

long$gndr <- factor(long$gndr,levels = c(1,2),labels = c("Male","Female")) 



### exploratory variables
####### just to take a look 
table(long$gndr,useNA="ifany")
table(long$agea,useNA="ifany")
table(long$happy,useNA="ifany")
table(long$ppltrst,useNA="ifany")
table(long$sclmeet,useNA="ifany")
table(long$polintr,useNA="ifany")
table(long$vote,useNA="ifany")

# list of not ID-variables (i drop name essround cntry and idno)
evars2 <- evars[-c(1:4)]
# I will use ageg (factor) not agea (continous) for age in the table
evars2[2] <- "ageg"
evars2

# BUILDING TABLE STEP BY STEP
tct <- NULL
tct0 <- NULL
tct0 <- long %>% count(gndr) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2)) %>%
  mutate(Varname="Gender") %>%
  rename(Category=gndr) %>%
  select(Varname,everything())
tct0
tct <- rbind(tct,tct0)
tct0 <- long %>% count(ageg) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2)) %>%
  mutate(Varname="Age") %>%
  rename(Category=ageg) %>%
  select(Varname,everything())
tct <- rbind(tct,tct0)
tct
tct0 <- long %>% count(vote) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2)) %>%
  mutate(Varname="Vote") %>%
  rename(Category=vote) %>%
  select(Varname,everything())
tct <- rbind(tct,tct0)
tct
tct0 <- long %>% count(polintr) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2)) %>%
  mutate(Varname="Interest in politics") %>%
  rename(Category=polintr) %>%
  select(Varname,everything())
tct <- rbind(tct,tct0)
tct
tct0 <- long %>% count(sclmeet) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2)) %>%
  mutate(Varname="Socially meet friends") %>%
  rename(Category=sclmeet) %>%
  select(Varname,everything())
tct <- rbind(tct,tct0)
tct0
tct
tct0 <- long %>% count(ppltrstf) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2)) %>%
  mutate(Varname="Trust") %>%
  rename(Category=ppltrstf) %>%
  select(Varname,everything())
tct <- rbind(tct,tct0)
tct0
tct0 <- long %>% count(happyf) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2)) %>%
  mutate(Varname="Happy") %>%
  rename(Category=happyf) %>%
  select(Varname,everything())
tct <- rbind(tct,tct0)
tct0

# renaming columns + %-values
colnames(tct) <- c("Variable name","Values","N","Proportion")
tct$Proportion <- str_c(as.character(tct$Proportion *100),"%")
tct

write.csv2(tct,"t2.csv")



###################### VOTING BEHAVIOUR
# recoding vote to two categorical factor
vote2key <- c("Voted last national election" = "Voted last national election", 
              "Not voted"= "Not voted", "Not eligible to vote" = NA)
long$vote2f <- recode_factor(long$vote, !!!vote2key)
table(long$vote2f,useNA = "ifany")

# recoding vote to two categorical numeric 

long$vote2n[long$vote2f=="Not voted"] <- 0
long$vote2n[long$vote2f=="Voted last national election"] <- 1
table(long$vote2n,useNA = "ifany")


############ ESS level 

### A table:  year / proportion of voters
vt1<- long %>% group_by(year) %>% summarize(Proportion=mean(vote2n,na.rm = T))
# adjusted table with nice %-values
vt1ADJ <- vt1 %>% mutate(Proportion=as.character(round(Proportion *100,1))) %>%
  mutate(Proportion=str_c(Proportion,"%"))
write.csv2(vt1ADJ, "appendixtable01.csv")

# figure
figure1 <- ggplot(vt1, aes(year,Proportion)) + 
  geom_smooth() +
  scale_x_continuous(breaks=seq(2002,2016,by=2)) +
  ylim(0.7,0.85) + 
  theme_minimal() +
  labs(x="Year",
       y="Proportion",
       title="Proportion of voters at the last national election (2002-2016)",
       caption="data source: ESS")
figure1
ggsave("figure1.jpg",figure1)
  

######### COUNTRY LEVEL
####### grouping of countries
### reading in the 



#### I want to drop countries where maximum 3 waves are available
# transforming it to dataframe
t1df <- data.frame(t1)
# I drop rows where n=0 (no survey)
t1df <- dplyr::filter(t1df,Freq!=0) 
names(t1df)
# a table with the countries in rows + number of waves
temp <- t1df %>% group_by(Var1) %>% count() 
# I take a look at this table showing how many countries in 1,2...8 participated
table(temp$n)
# I decided to drop countries with maximum 3 waves
maxmissingpercountry <- 3
droptrends <- temp[temp$n<=maxmissingpercountry,1]
droptrends <- droptrends$Var1

# A table for ALL countries with year and roportion of voters
vt2 <- long %>% group_by(cntryName,year) %>% summarise(Proportion=mean(vote2n,na.rm=T)) %>% 
  mutate(Proportion = round(Proportion,3)) %>% mutate(Proportion=Proportion*100)

# I check the maximum and minimum of voter rates: 55-95%
summary(vt2)


# a table for appendix
vt2ADJ <- vt2 %>% 
  mutate(Proportion=str_c(Proportion,"%")) %>%
  mutate(year=as.character(year))
class(vt2ADJ)
vt2ADJ
## I spread it in order to have the years in columns

vt2ADJ <- spread(vt2ADJ,key=year,value=Proportion)
colnames(vt2ADJ)[1] <- "Country name"
vt2ADJ
write.csv2(vt2ADJ,"appendixtable02.csv",na = "")


# a table for the GRAPH, grouped by region as well + dropping some contries
vt2 <- long %>% group_by(region,cntryName,year) %>% 
  summarise(Proportion=mean(vote2n,na.rm=T)) %>% 
  mutate(Proportion = round(Proportion,3)) %>% 
  mutate(Proportion=Proportion*100) %>% 
  filter(!cntryName %in% c("Israel","Albania","Croatia","Iceland","Latvia",
                           "Luxembourg","Romania","Turkey")) 


#vt2$year <- as.character(vt2$year)
vt2$region <- as.character(vt2$region)

# I separate regions
table(vt2$region)
vt2ee <- vt2 %>% filter(region=="Eastern-Europe")
vt2me <- vt2 %>% filter(region=="Middle-Europe")
vt2ne <- vt2 %>% filter(region=="Northern-Europe")
vt2see <- vt2 %>% filter(region=="South-East-Europe")
vt2se <- vt2 %>% filter(region=="South-Europe")
vt2we <- vt2 %>% filter(region=="West-Europe")

vt2

# old version, i decided to work with ggplot
#EE
#newggslopegraph(vt2ee, year, Proportion, cntryName ,DataTextSize = 2.5, YTextSize = 3) +
#  labs(title="Proportion of participants in the last national election ", 
#       subtitle="Eastern-Europe", 
#       caption="ESS")


#vt2ee$year<- as.numeric(vt2ee$year)  
#ggplot(vt2ee, aes(x=year,y=Proportion,color=cntryName)) + 
#  geom_line() +
#  scale_x_continuous(breaks=seq(2002,2016,by=2)) +
#  ylim(0.7,0.85) + 
#  theme_minimal() +
#  labs(x="Year",
#       y="Proportion",
#       title="Proportion of voters at the last national election (2002-2016)",
#       caption="data source: ESS")

vt2
vt2$Proportion <- vt2$Proportion/100
vt2$year <- as.numeric(vt2$year)
colnames(vt2)[2] <- "Country"
vt2$year<- as.numeric(vt2$year)
vt2
vt2ee <- vt2 %>% filter(region=="Eastern-Europe")
vt2me <- vt2 %>% filter(region=="Middle-Europe")
vt2ne <- vt2 %>% filter(region=="Northern-Europe")
vt2see <- vt2 %>% filter(region=="South-East-Europe")
vt2se <- vt2 %>% filter(region=="South-Europe")
vt2we <- vt2 %>% filter(region=="West-Europe")

vt2ne

#Eastern
plot <- ggplot(vt2ee,aes(x=year,y=Proportion,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(0.55,0.95) +
  labs(y="Proportion of voters",
       title="Proportion of voters at the last national election (2002-2016)",
       subtitle="Eastern-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("f2ee.jpg",plot)  

#Middle
plot <- ggplot(vt2me,aes(x=year,y=Proportion,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(0.55,0.95) +
  labs(y="Proportion of voters",
       title="Proportion of voters at the last national election (2002-2016)",
       subtitle="Middle-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("f2me.jpg",plot)  

#Northern
plot <- ggplot(vt2ne,aes(x=year,y=Proportion,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(0.55,0.95) +
  labs(y="Proportion of voters",
       title="Proportion of voters at the last national election (2002-2016)",
       subtitle="Northern-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("f2ne.jpg",plot)  

#South-east
plot <- ggplot(vt2see,aes(x=year,y=Proportion,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(0.55,0.95) +
  labs(y="Proportion of voters",
       title="Proportion of voters at the last national election (2002-2016)",
       subtitle="South-East-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("f2see.jpg",plot)  

#South
plot <- ggplot(vt2se,aes(x=year,y=Proportion,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(0.55,0.95) +
  labs(y="Proportion of voters",
       title="Proportion of voters at the last national election (2002-2016)",
       subtitle="South-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("f2se.jpg",plot)  

#West
plot <- ggplot(vt2we,aes(x=year,y=Proportion,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(0.55,0.95) +
  labs(y="Proportion of voters",
       title="Proportion of voters at the last national election (2002-2016)",
       subtitle="West-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("f2we.jpg",plot)  


############### CORRUPTION PERCEPTION index + GDP per capita

#### I want to select the contries with the highest difference 2002-2016
vt2b <- long %>% group_by(cntry,year) %>% summarise(Proportion=mean(vote2n,na.rm=T)) %>% 
  mutate(Proportion = round(Proportion,3)) %>% mutate(year=as.character(year))
class(vt2b)
vt2b
## I spread it in order to have the years in columns

##### I want to select the contries with the highest differences 2002-2016
vt2s <- spread(vt2b,key=year,value=Proportion)
colnames(vt2s) <- c("cntry","p2002","p2004","p2006","p2008","p2010","p2012","p2014","p2016")
colnames(vt2s)
# a variableshowing the absolute value of difference
vt2s$diff <- abs(vt2s$p2002 - vt2s$p2016)
nagydifi <- vt2s[vt2s$diff>0.064,]
nagydifi[!is.na(nagydifi$diff),]

#the list of needed contries
neededcountries <- c("BE","CZ","FR","IL","IT","PL","SI")

#COUNTRY LEVEL DATA
#### NO data for GDP 2016. I estimate this using GDP2015
c$c_gdppc_2016 <- c$c_gdppc_2015



### Ck is a nice temporal database having 7 contries with the highest differences + all variables
ck <- filter(c,c$cntry %in% c("BE","CZ","FR","IL","IT","PL","SI")) 

str(ck)
summary(ck)
ck

# I have to do build the dataset 
# I will collect my data in this list 
clevel <- list()
for (i in seq(2002,2016,by=2)){
  # list of needed variables
  # I get the variable names like gdppc_2002 
  ## actual Corruption perception index c_ticpi_XXXX
  corrupt <- str_c("c_ticpi_",i)
  ## actual GDP per capita c_gdppc_XXX
  gdp <- str_c("c_gdppc_",i)
    ck$year <- i
  # vector with all needed variable names 
  neededvars <- c("cntry","year",corrupt,gdp)
  # database with only 7 contries + needed vars for actual year
  clevel0 <- ck[,neededvars]
  colnames(clevel0) <- c("cntry","year","corrupt","gdp")
  # I need a wave
  wave <- (i-2000)/2
  # list
  clevel[[wave]] <- clevel0
}

#f "adding" the data using rbind 
clevelDF <- NULL
for (i in seq_along(clevel)) {
  clevelDF <- rbind(clevelDF,clevel[[i]])
}
clevelDF

summary(clevelDF)

### A new table since I need now both: cntry and cntryName but no need now for region
vt3 <- long %>% group_by(cntry,cntryName,year) %>% 
  summarise(Proportion=mean(vote2n,na.rm=T)) %>% 
  mutate(Proportion = round(Proportion,3)) %>% 
  mutate(Proportion=Proportion*100) %>% 
  filter(cntry %in% c("BE","CZ","FR","IL","IT","PL","SI")) %>%
  select(cntry,cntryName,year,Proportion)
vt3

# I will try to merge vt3 and clevelDF now
clevelDATA <- inner_join(vt3,clevelDF, by=c("cntry","year"))
clevelDATA
clevelDATA <- as.data.frame(clevelDATA)
clevelDATA

##### scale change in 2011 at corruption!!!!!
# I separate the years before and after 2011
clevelDATA1 <- clevelDATA %>% filter(clevelDATA$year<2011)
clevelDATA1
clevelDATA2 <- clevelDATA %>% filter(clevelDATA$year>2011)
clevelDATA2
clevelDATA1$corrupt <- clevelDATA1$corrupt*10
clevelDATA <- rbind(clevelDATA1,clevelDATA2)

        
###### DIMITRI's GRAPH : working but not really interpretabble version
ggplot(clevelDATAplus, aes(x=corrupt,y=Proportion,color=cntryName, alpha=year)) +
  geom_point(aes(size=0.3),show.legend=T) + 
  geom_line(aes(size=0.3),show.legend=F) +
  scale_x_continuous(name="Corruption") +
  theme_minimal() +
  labs(y="Proportion of voters",
       title="Voter behaviour across countries")

#### GRAPH for only 3 countries
clevelDATAplus <- clevelDATA %>% filter(cntryName %in% c("Poland","Israel","Belgium"))

dimitrigraph <- ggplot(clevelDATAplus, aes(x=gdp,y=Proportion,color=cntryName, alpha=year)) +
  geom_point(aes(),show.legend=T) + 
  geom_line(aes(size=1),show.legend=F) +
  scale_x_continuous(name="GDP per capita") +
  theme_classic() +
  labs(y="Proportion of voters",
       title="Voter behaviour across Poland, Belgium and Israel",
       caption="ESS")

ggsave("dimitrigraph.jpg",dimitrigraph)

dimitrigraph
############################## HAPPY part
table(long$happy)


############ ESS level 

### A TABLE:  year / proportion of voters
ht1<- long %>% group_by(year) %>% summarize(Happiness=mean(happy,na.rm = T))
# adjusted table with nice %-values
ht1
write.csv2(ht1, "appendixtablehappy03.csv")

# GRAPH
figure1 <- ggplot(ht1, aes(year,Happiness)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(2002,2016,by=2)) +
  ylim(5.2,8.5) + 
  theme_minimal() +
  labs(x="Year",
       y="Happiness",
       title="Happiness (mean values, based on 0-10 scale)",
       caption="data source: ESS")
figure1
ggsave("figurehappy1.jpg",figure1)



############### country level TABLE

# A table for ALL countries with year and roportion of voters
ht2 <- long %>% group_by(cntryName,year) %>% summarise(Happiness=mean(happy,na.rm=T)) 
# I check the maximum and minimum of voter rates: 55-95%
ht2ADJ <- ht2 %>% mutate(Happiness=round(Happiness,2)) %>% mutate(Happiness=as.character(Happiness))
summary(ht2$Happiness)
ht2ADJ
#min 5,2 max 8,5
ht2ADJ <- spread(ht2,key=year,value=Happiness)
colnames(ht2ADJ)[1] <- "Country name"
ht2ADJ
write.csv2(ht2ADJ,"appendixtable04.csv",na = "")


##### happy plots in regions

# a table for the GRAPH, grouped by region as well + dropping some contries
ht2 <- long %>% group_by(region,cntryName,year) %>% 
  summarise(Happiness=mean(happy,na.rm=T)) %>% 
  mutate(Happiness = round(Happiness,3)) %>% 
  filter(!cntryName %in% c("Israel","Albania","Croatia","Iceland","Latvia",
                           "Luxembourg","Romania","Turkey")) 

colnames(ht2)[2] <- "Country"

ht2$region <- as.character(ht2$region)
ht2


# I separate regions
table(ht2$region)
ht2ee <- ht2 %>% filter(region=="Eastern-Europe")
ht2me <- ht2 %>% filter(region=="Middle-Europe")
ht2ne <- ht2 %>% filter(region=="Northern-Europe")
ht2see <- ht2 %>% filter(region=="South-East-Europe")
ht2se <- ht2 %>% filter(region=="South-Europe")
ht2we <- ht2 %>% filter(region=="West-Europe")



#Eastern
plot <- ggplot(ht2ee,aes(x=year,y=Happiness,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(5.2,8.5) +
  labs(y="Happiness",
       title="Change in happiness (2002-2016)",
       subtitle="Eastern-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("h2ee.jpg",plot)  

#Middle
plot <- ggplot(ht2me,aes(x=year,y=Happiness,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(5.2,8.5) +
  labs(y="Happiness",
       title="Change in happiness (2002-2016)",
       subtitle="Middle-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("h2me.jpg",plot)  

#Northern
plot <- ggplot(ht2ne,aes(x=year,y=Happiness,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(5.2,8.5) +
  labs(y="Happiness",
       title="Change in happiness (2002-2016)",
       subtitle="Northern-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("h2ne.jpg",plot)  

#South-east
plot <- ggplot(ht2see,aes(x=year,y=Happiness,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(5.2,8.5) +
  labs(y="Happiness",
       title="Change in happiness (2002-2016)",
       subtitle="South-East-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("h2see.jpg",plot)  

#South
plot <- ggplot(ht2se,aes(x=year,y=Happiness,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(5.2,8.5) +
  labs(y="Happiness",
       title="Change in happiness (2002-2016)",
       subtitle="South-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("h2se.jpg",plot)  

#West
plot <- ggplot(ht2we,aes(x=year,y=Happiness,colour=Country )) + 
  geom_line(aes(group=Country),lwd=1.5) + 
  scale_x_continuous(breaks=seq(2002,2016,by=2) ) +
  ylim(5.2,8.5) +
  labs(y="Happiness",
       title="Change in happiness (2002-2016)",
       subtitle="West-Europe",
       caption="data source: ESS",
       colour="Country") +
  theme_minimal() +
  theme(legend.position="bottom")
ggsave("h2we.jpg",plot)  





################ demographic







################ GENDER - VOTING - GAP

hgvt2 <- long %>% filter(!is.na(gndr)) %>% group_by(cntryName,gndr) %>% summarise(vote=mean(vote2n,na.rm=T)) 
hgvt2
# I check the maximum and minimum of voter rates: 55-95%
hgvt2ADJ <- hgvt2 
hgvt2ADJ
#min 5,2 max 8,5
hgvt2ADJ <- spread(hgvt2ADJ,key=gndr,value=vote)
colnames(hgvt2ADJ)[1] <- "Country name"
hgvt2ADJ
write.csv2(hgvt2ADJ,"gendervotegap.csv",na = "")
colnames(hgvt2ADJ)[1] <- "cntry"

hgvt2adj_selected <- hgvt2ADJ %>% filter(cntry %in% c("Belgium","Finland","France","Germany","Hungary","Ireland","Netherlands","Norway","Poland","Portugal","Slovenia","Spain","Sweden","Switzerland","UnitedKingdom"))
write.csv2(hgvt2adj_selected,"gendervotegap_selected.csv",na = "")


################ AGE - VOTING - GAP

hgvt3 <- long %>% filter(!is.na(ageg)) %>% group_by(cntryName,ageg) %>% summarise(vote=mean(vote2n,na.rm=T)) 
hgvt3
# I check the maximum and minimum of voter rates: 55-95%
hgvt3ADJ <- hgvt3 
hgvt3ADJ
#min 5,2 max 8,5
hgvt3ADJ <- spread(hgvt3ADJ,key=ageg,value=vote)
colnames(hgvt3ADJ)[1] <- "Country name"
hgvt3ADJ
write.csv2(hgvt3ADJ,"agevotegap.csv",na = "")
colnames(hgvt3ADJ)[1] <- "cntry"

hgvt3adj_selected <- hgvt3ADJ %>% filter(cntry %in% c("Belgium","Finland","France","Germany","Hungary","Ireland","Netherlands","Norway","Poland","Portugal","Slovenia","Spain","Sweden","Switzerland","UnitedKingdom"))
write.csv2(hgvt3adj_selected,"agevotegap_selected.csv",na = "")







###### AGE - HAPPINESS - GAP

hat2 <- long %>% filter(!is.na(ageg)) %>% group_by(cntryName,ageg) %>% summarise(Happiness=mean(happy,na.rm=T)) 
hat2
# I check the maximum and minimum of voter rates: 55-95%
summary(hat2$Happiness)
hat2ADJ
#min 5,2 max 8,5
hat2ADJ <- spread(hat2ADJ,key=ageg,value=Happiness)

colnames(hat2ADJ)[1] <- "Country name"
hat2ADJ
write.csv2(hat2ADJ,"agegrouphappy.csv",na = "")

#### onyl selected countries
colnames(hat2ADJ)[1] <- "cntry"

hat2adj_selected <- hat2ADJ %>% filter(cntry %in% c("Belgium","Finland","France","Germany","Hungary","Ireland","Netherlands","Norway","Poland","Portugal","Slovenia","Spain","Sweden","Switzerland","UnitedKingdom"))
write.csv2(hat2adj_selected,"agegrouphappy_selected.csv",na = "")




#### GENDER - HAPPINESS - GAP

hgt2 <- long %>% filter(!is.na(gndr)) %>% group_by(cntryName,gndr) %>% summarise(Happiness=mean(happy,na.rm=T)) 
hgt2
# I check the maximum and minimum of voter rates: 55-95%
hgt2ADJ <- hgt2 
hgt2ADJ
#min 5,2 max 8,5
hgt2ADJ <- spread(hgt2,key=gndr,value=Happiness)
colnames(hgt2ADJ)[1] <- "Country name"
hgt2ADJ
write.csv2(hgt2ADJ,"genderhappy.csv",na = "")

##### bar charts for gender + regio

GH <- long %>% filter(!is.na(gndr)) %>%
  filter(region!="") %>% 
  group_by(region,gndr) %>% 
  summarize(Happiness=mean(happy,na.rm = T)) %>%
  mutate(Happiness=round(Happiness,2))

GH


GenderHappyDiffRegion <- newggslopegraph(GH, gndr, Happiness, region ,DataTextSize = 3.5, YTextSize = 3.5) +
  labs(title="Gender differences in overall happiness ", 
       subtitle="", 
       caption="data source: ESS 2002-2016")

ggsave(filename = "GenderHappyDiffRegion.jpg",GenderHappyDiffRegion)

