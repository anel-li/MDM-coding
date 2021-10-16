library(readstata13)
library(DescTools)
library(mboost)
#wave 1 data: for item nonresponse
i <- read.dta13(file="anchor1_50percent.dta")
# wave 2 for unit nonresponse
u <- read.dta13(file = "results_w2.dta", convert.factors = T)
#merged file 
p <- merge(i,u,by="id", all=T)

#variable names in the p dataset
pv <- names(p)
#number of variables in p
pvl <- length(pv)

#### GOAL 01: to find out what variables to include in the analysis 
#### First analysis: 
##to find out where we have a lot of cases not applicable
##we can not use variables with more than 50% of not applicable cases for the prediction 
# variables in the rows, + 5 columns all with default value 0 
overview <- matrix(0,ncol(p),5)
#a loop going from variable 1 until the last variable in p dataframe
for (i in 1:pvl){
  # The variable I am analyzing at the moment: the i-th variable (all rows, and the i-th variable)
  myvar <- p[,pv[i]] 
  #col1: frequency of -3s
  overview[i,1] <- length(myvar[myvar==-3 |
                                myvar=="-3 Does not apply" |
                                myvar=="-3 Trifft nicht zu"]) 
  #col2: frequency of -2s 
  overview[i,2] <- length(myvar[myvar==-2 |
                                myvar=="-2 Das möchte ich nicht beantworten." |
                                myvar=="-2 Das möchte ich nicht beantworten" |  
                                myvar=="-2 Keine Angabe"]) 
  #col3: frequency of -1s  
  overview[i,3] <- length(myvar[myvar==-1 |
                                myvar=="-1 Weiß nicht"]) 
  #col4: frequency of -7s 
  overview[i,4] <- length(myvar[myvar==-7 |
                                myvar=="-7 Incomplete data"]) 
  #col5: frequency of -4s
  overview[i,5] <- length(myvar[myvar==-4 |
                                myvar=="-4 Filter error / Incorrect entry"]) 
}


# I convert it to a data frame and I write a csv
o <- as.data.frame(overview)
# I add row names (variable names)
row.names(o) <- pv
# A new variable as a sum of -3 -2 -1 cases
o$sum <- o$V1 + o$V2 + o$V3
write.csv2(o,"o.csv")
# I opened the file in excel and I classified every variable in excel 


# list of potential item nonresponse exploratory variables: 250 variable
vl_itemNR <- c("val1i1",  "val1i2",  "val1i3",  "val1i4",  "val1i5",  "val1i6",  
               "val1i7",  "val1i8",  "sat1i1",  "sat1i2",  "sat1i3",  "sat1i4",  
               "srs1i1",  "srs1i2",  "srs1i3", "srs1i4",  "srs1i5",  "sd3",  "sd10", 
               "rtr5",  "bce1i1",  "bce1i2",  "bce1i3",  "bce1i4",  "bce1i5",  
               "bce1i6",  "bce1i7",  "bce1i8",  "bce1i9",  "bce1i10", "pa3",  
               "sdp2i1",  "sdp2i2",  "sdp3",  "sdp6",  "cps1",  "pa16i1",  
               "pa16i2",  "pa16i3",  "pa16i4",  "pa16i5",  "pa16i6",  "pa17i1",  
               "pa17i2",  "pa17i3", "pa17i4",  "pa17i5",  "pa17i6",  "pa17i7",  
               "pa17i8",  "pa18i1",  "pa18i2",  "pa18i3",  "pa18i4",  "pa18i5",  
               "pa18i6",  "pa18i7",  "pa18i8",  "pa18i9",  "pa18i10", 
               "pa18i11",  "pa18i12",  "pa18i13",  "pa18i14",  "pa18i15",  
               "pa18i16",  "pa18i17",  "pa18i18",  "sat3",  "sat4",  "pa19i1",  
               "pa19i2",  "pa19i3",  "pa19i4",  "pa19i5", "pa19i6",  "pa19i7",  
               "pa19i8",  "pa20i1",  "pa20i2",  "pa20i3",  "pa22pi1",  
               "pa22pi2",  "pa22pi3",  "pa22pi4",  "pa22pi5",  "pa22pi6",  
               "pa22pi7",  "pa22pi8",  "pa22ri1", "pa22ri2",  "pa22ri3",  
               "pa22ri4",  "pa22ri5",  "pa22ri6",  "pa22ri7",  "pa22ri8",  
               "pa23",  "pa24",  "pa25",  "sex5",  "sat5",  "frt1",  "frt3",  
               "frt5", "frt6",  "bce2i1",  "bce2i2",  "bce2i3",  "bce2i4",  
               "bce2i5",  "bce2i6",  "bce2i7",  "bce2i8",  "bce2i9",  
               "bce2i10",  "frt7",  "frt9",  "frt10i1",  "frt10i2", "frt11v1i1",  
               "frt11v1i2",  "frt11v1i3",  "frt11v1i4",  "frt11v1i5",  "frt12i1",  
               "frt12i2",  "frt12i3",  "frt12i4",  "lsr1i1",  "lsr1i2",  "lsr1i3",  
               "lsr1i4",  "lsr2",  "lsr3", "lsr4",  "lsr5i4",  "lsr5i5",  "hc1p1i1",  
               "hc1p2i1",  "hc1p1i2",  "hc1p2i2",  "hc4h1",  "hc5h1",  
               "hc7h1",  "hc8h1p1",  "hc9h1p1",  "hc13h1",  "hc14h1",  
               "hcp5", "mig1i1",  "mig1i2",  "mig1i3",  "mig1i4",  "mig1i5",  
               "mig1i6",  "mig1i7",  "mig1i8",  "mig1i9",  "mig1i10",  
               "mig1i11",  "mig1i12",  "mig2m",  "mig2y",  "mig4",  
               "igr1y",  "igr2y",  "igr3",  "igr4",  "igr5",  "igr7",  "igr9y",  
               "igr10",  "igr11",  "igr12",  "igr13",  "igr14",  "igr15",  
               "igr17i1", "igr17i2",  "igr17i3",  "igr17i4",  "igr18i1",  "igr18i2",  
               "igr18i3",  "igr18i4",  "igr19",  "igr20",  "sd23i10",  "sd23i11",  
               "sd23i12",  "sd23i13",  "sd23i14",  "sd23i15", "sd23i17",  
               "sd23i20",  "sd23i21",  "sd27",  "sd28",  "sd29i1",  "sd29i2",  
               "sd29i3",  "sd29i4",  "sd29i5",  "sd29i6",  "sd29i7",  "job14",  
               "inc7",  "inc9", "inc10i1",  "inc10i2",  "inc10i3",  "inc10i4",  
               "inc10i5",  "inc10i6",  "inc10i7",  "inc10i8",  "inc10i9",  
               "inc10i10",  "inc10i11",  "inc10i12",  "inc11",  "inc13",  
               "inc18", "inc19i1",  "inc19i2",  "inc19i3",  "inc19i4",  "inc19i5",  
               "sd30",  "sd31",  "per1i1",  "per1i2",  "per1i3",  "per1i4",  
               "per1i5",  "per1i6",  "per1i7",  "per1i8", "per1i9",  "per1i10",  
               "per1i11",  "per1i12",  "per1i13",  "hlt1",  "hlt2",  "hlt3",  
               "hlt4",  "hlt5",  "hlt7",  "sat6")
               
# number of variables 
vl_itemNRl <- length(vl_itemNR)



# GOAL: to have 2 odds ratio for every variable 
# column1 : if we see only NA-s
# column2: if we see NA+DK-s

# erg2: binary variable for erg
# 1 for refusal 0 for EVERY OTHER outcome 
p$erg2 <- 0
p$erg2[p$erg=="refusal due to time or other restrictions"] <- 1
p$erg2[p$erg=="refusal due to interview duration"] <- 1
p$erg2[p$erg=="refusal due to lack of interest in the topic"] <- 1
p$erg2[p$erg=="refusal due to other reasons"] <- 1


# erg3: binary variable for erg FOCUSING ON interviews and refusals
#p$erg3[p$erg=="Interview"] <- 0
#p$erg3[p$erg=="refusal due to time or other restrictions"] <- 1
#p$erg3[p$erg=="refusal due to interview duration"] <- 1
#p$erg3[p$erg=="refusal due to lack of interest in the topic"] <- 1
#p$erg3[p$erg=="refusal due to other reasons"] <- 1




odds <- matrix(0,vl_itemNRl,6)
for (i in 1:vl_itemNRl) {
  myvar <- p[,vl_itemNR[i]]
  # recode the variable into a binary variable having values: 
  # NULL as defaault
  p$myvarNA <- NULL
  # 1 if item nonresponse
  p$myvarNA[myvar==-2] <- 1
  p$myvarNA[myvar=="-2 Das möchte ich nicht beantworten."] <- 1
  p$myvarNA[myvar=="-2 Das möchte ich nicht beantworten"] <- 1
  p$myvarNA[myvar=="-2 Keine Angabe"] <- 1
    
  # 0 if valid value is in the variable: (positive values +  DK)
  p$myvarNA[myvar!=-2 &
          myvar!="-2 Das möchte ich nicht beantworten." &
          myvar!="-2 Das möchte ich nicht beantworten" &
          myvar!="-2 Keine Angabe" & 
          myvar!=-3 &
          myvar!="-3 Does not apply" &
          myvar!="-3 Trifft nicht zu" & 
          myvar!="-7 Incomplete data" & 
          myvar!=-4 &
          myvar!="-4 Filter error / Incorrect entry" &
          myvar!="-5" & 
          myvar!="-6" &
          myvar!=-7 ] <- 0

  
  # NULL as defaault
  p$myvarNADK <- NULL
  # 1 if item nonresponse
  p$myvarNADK[myvar==-2] <- 1
  p$myvarNADK[myvar=="-2 Das möchte ich nicht beantworten."] <- 1
  p$myvarNADK[myvar=="-2 Das möchte ich nicht beantworten"] <- 1
  p$myvarNADK[myvar=="-2 Keine Angabe"] <- 1
  p$myvarNADK[myvar==-1] <- 1
  p$myvarNADK[myvar=="-1 Weiß nicht"] <- 1
  
  
  # 0 if valid value is in the variable: (positive values )
  p$myvarNADK[myvar!=-1 &
            myvar!="-1 Weiß nicht" &
            myvar!=-2 &
            myvar!="-2 Das möchte ich nicht beantworten." &
            myvar!="-2 Das möchte ich nicht beantworten" &
            myvar!="-2 Keine Angabe" & 
            myvar!=-3 &
            myvar!="-3 Does not apply" &
            myvar!="-3 Trifft nicht zu" & 
            myvar!="-7 Incomplete data" & 
            myvar!=-4 &
            myvar!="-4 Filter error / Incorrect entry" &
            myvar!="-5" & 
            myvar!="-6" &
            myvar!=-7 ] <- 0
    #odds ratio with NA  
   odds[i,1] <- OddsRatio(table(p$myvarNA,p$erg2),conf.level=0.95)[1]                  
    #odds ratio Conf int lower bound NA
   odds[i,2] <- OddsRatio(table(p$myvarNA,p$erg2),conf.level=0.95)[2]
    #odds ratio Conf int upper bound NA
   odds[i,3] <- OddsRatio(table(p$myvarNA,p$erg2),conf.level=0.95)[3]
   #odds ratio with NADK  
   odds[i,4] <- OddsRatio(table(p$myvarNADK,p$erg2),conf.level=0.95)[1]                  
   #odds ratio Conf int lower bound NADK
   odds[i,5] <- OddsRatio(table(p$myvarNADK,p$erg2),conf.level=0.95)[2]
   #odds ratio Conf int upper bound NADK
   odds[i,6] <- OddsRatio(table(p$myvarNADK,p$erg2),conf.level=0.95)[3]
   print(i)
 }

# I convert it to a data frame and I write a csv
odds <- as.data.frame(odds)
# I add row names (variable names)
row.names(odds) <- vl_itemNR
# A new variable as a sum of -3 -2 -1 cases
write.csv2(odds,"odds.csv")


#### round2 for the excluded variablse

round2 <- c("co1i2", "sd5e1em", "sd5e1ey", "sd5e2bm", "sd5e2by", "sd8e1bm",
            "sd8e1by", "sd8e2bm", "sd8e2by", "sd8e2em", "sd8e3em",
            "sd8e2ey", "sd8e3ey", "sd11", "sd12m", "sd12y", "rtr1p3n",
            "rtr1p4n", "rtr1p3g", "rtr1p4g", "rtr2p2e1bm", "rtr2p3e1bm",
            "rtr2p4e1bm", "rtr2p5e1bm", "rtr2p6e1bm", "rtr2p2e1by",
            "rtr2p3e1by", "rtr2p4e1by", "rtr2p6e1by", "rtr2p2e1em",
            "rtr2p2e2em", "rtr2p2e3em", "rtr2p3e1em", "rtr2p3e2em",
            "rtr2p4e1em", "rtr2p5e1em", "rtr2p6e1em", "rtr2p2e1ey",
            "rtr2p2e2ey", "rtr2p2e3ey", "rtr2p3e1ey", "rtr2p3e2ey",
            "rtr2p4e1ey", "rtr2p5e1ey", "rtr2p6e1ey", "rtr3p1e2", "rtr3p2e1",
            "rtr3p2e2", "rtr3p2e3", "rtr3p3e1", "rtr3p5e1", "rtr2p1e2bm",
            "rtr2p2e2bm", "rtr2p2e3bm", "rtr2p2e2by", "rtr2p2e3by", "rtr4p2",
            "rtr4p3", "rtr4p4", "rtr4p5", "rtr4p6", "rtr6p1", "rtr6p2", "rtr6p3",
            "rtr6p4", "rtr6p5", "rtr6p6", "rtr6p7", "rtr6p8", "rtr7p1e1bm",
            "rtr7p2e1bm", "rtr7p3e1bm", "rtr7p4e1bm", "rtr7p5e1bm",
            "rtr7p1e1by", "rtr7p2e1by", "rtr7p3e1by", "rtr7p4e1by",
            "rtr7p5e1by", "rtr7p1e1em", "rtr7p2e1em", "rtr7p3e1em",
            "rtr7p4e1em", "rtr7p5e1em", "rtr7p1e1ey", "rtr7p2e1ey",
            "rtr7p3e1ey", "rtr7p4e1ey", "rtr7p5e1ey", "rtr8p1e1", "rtr8p3e1",
            "rtr9", "rtr10m", "rtr10y", "rtr11", "rtr12", "rtr13p1", "rtr13p2",
            "rtr13p3", "rtr13p4", "rtr13p5", "rtr13p6", "rtr13p8", "rtr14p1bm",
            "rtr14p2bm", "rtr14p1by", "rtr15p1", "rtr14p1em", "rtr14p2em",
            "rtr14p1ey", "rtr14p2ey", "sd14k2n", "sd14k3n", "sd14k3g",
            "sd15k1", "sd16k1", "sd16k2", "sd16k3", "sd17k1", "sd17k2",
            "sd17k3", "sd18k2", "sd19k1m", "sd19k2m", "sd19k3m",
            "sd19k4m", "sd19k5m", "sd19k6m", "sd19k1y", "sd19k2y",
            "sd19k3y", "sd19k4y", "sd19k5y", "sd19k6y", "sd19k1d", "sd19k2d",
            "sd19k3d", "sd19k4d", "sd19k5d", "sd19k6d", "sd20k1", "sd21k1",
            "sd21k5", "rtr16k1e1", "rtr17k1e1bm", "rtr17k2e1bm",
            "rtr17k1e1by", "rtr17k2e1by", "rtr17k1e1em", "rtr17k2e1em",
            "rtr17k3e1em", "rtr17k1e1ey", "rtr17k2e1ey", "rtr17k3e1ey",
            "rtr16k1e2", "rtr18k1m", "rtr18k1y", "rtr18k1d", 
            "rtr20k1m", "rtr20k1y", "rtr21k1", "sin1", "sin2", "sin3i1", "sin3i2",
            "sat2", "sin4i1", "sin4i2", "sin4i3", "sin5i1", "sin5i2", "sin5i3",
            "sin5i4", "sin6i1", "sin6i2", "sin6i3", "sin6i4", "sin6i5", "pa1i1",
            "pa1i2", "pa4", "pa5", "pa6", "pa7", "pa8", "pa9", "pa10i1",
            "pa10i2", "pa10i3", "pa11", "pa12i1", "pa12i2", "pa12i3", "pa13",
            "sdp9i1", "sdp9i2", "sdp9i3", "sdp9i4", "sdp9i5", "sdp9i6", "sdp9i7",
            "sdp11", "sdp12", "pa14i1", "pa14i2", "pa14i3", "pa14i4", "pa14i5",
            "pa15", "cps4", "pa21i1", "pa21i2", "pa21i3", "pa21i4", "pa21i5",
            "pa21i6", "pa26", "pa27", "pa28", "sex1i1", "sex1i2", "sex1i3",
            "sex2", "sex3", "sex4", "sex6i1", "sex6i2", "sex6i3", "sex6i4",
            "sex6i5", "sex6i6", "sex6i7", "sex6i8", "sex6i9", "sex6i10", "sex6i11",
            "sex7", "frt2", "frt4i1", "frt4i2", "frt4i3", "frt4i4", "frt4i5", "frt4i6",
            "frt4i7", "frt11v1i6", "frt11v1i7", "frt11v1i8", "frt11v2i1",
            "frt11v2i2", "frt11v2i3", "frt11v2i4", "frt11v2i5", "frt11v2i6",
            "frt11v2i7", "frt11v2i8", "frt13i1", "frt13i2", "frt13i3", "frt13i4",
            "frt13i5", "frt13i6", "frt13i7", "frt13i8", "frt13i9", "frt13i10",
            "frt13i11", "frt13i12", "frt13i13", "frt13i14", "lsr5i1", "lsr5i2",
            "lsr5i3", "lsr5i6", "lsr5i7", "lsr6", "lsr7", "lsr8", "hc1p3i2", "hc2",
            "hc3", "hc6h1", "hc9h1p2", "hc9h1p3", "hc9h1p4", "hc9h1p5",
            "hc9h1p6", "hc9h1p7", "hc9h1p8", "hc10h1p2", "hc10h1p3",
            "hc10h1p4", "hc10h1p5", "hc10h1p6", "hc8h1p2", "hc8h1p3",
            "hc8h1p4", "hc8h1p5", "hc8h1p6", "hc8h1p7", "hc8h1p8",
            "hc11h1", "hc12h1", "hc4h2", "hc5h2", "hc6h2", "hc7h2",
            "hc8h2p1", "hc9h2p1", "hc9h2p2", "hc9h2p3", "hc8h2p2",
            "hc8h2p3", "hc8h2p4", "hc8h2p5", "hc8h2p6", "hc8h2p7",
            "hc8h2p8", "hc8h2p9", "hc11h2", "hc12h2", "hcp1i1", "hcp2",
            "hcp3h", "hcp3m", "hcp4h", "hcp4m", "hcp6i1", "hcp7", "mig5m",
            "mig5y", "igr6m", "igr6y", "igr8m", "igr8y", "mig6i12o", "mig7i12o",
            "sd24", "sd26", "job1", "job3", "job4", "job5i1", "job5i2", "job5i3",
            "job5i4", "job7", "job8", "job9", "job10", "job12", "job13i1",
            "job13i2", "job13i3", "job13i4", "job13i5", "job13i6", "job13i7",
            "job15", "job16h", "job16m", "job17", "job18h", "job18m", "inc1",
            "inc2", "inc3", "inc4", "inc5", "inc6", "inc8", "inc12i2", "inc12i3",
            "inc12i4", "inc12i5", "inc12i6", "inc12i7", "inc12i8", "inc12i9",
            "inc12i11", "inc12i12", "inc14", "inc15", "inc16", "inc17", "inc20",
            "hlt6", "cps6", "int5" , "sd5e1bm", "rtr1p1n", "rtr1p2n", "rtr1p1g", "rtr1p2g", 
            "rtr2p1e1bm", "rtr2p1e1by", "rtr2p1e1em", "rtr2p1e1ey",
            "rtr3p1e1", "rtr4p1", "sd14k1n", "sd14k1g", "pa2m", "pa2y",
            "sdp1d", "sdp1m", "igr1m", "igr1d", "igr2m", "igr2d", "mig6i1",
            "mig6i2", "mig6i3", "mig6i4", "mig6i5", "mig6i6", "mig6i7", 
            "mig6i8", "mig6i9", "mig6i10", "mig6i11", "mig6i12", "mig7i1",
            "mig7i2", "mig7i3", "mig7i4", "mig7i5", "mig7i6", "mig7i7",
            "mig7i8", "mig7i9", "mig7i10", "mig7i11", "mig7i12", "igr9m",
            "sd23i1", "sd23i2", "sd23i3", "sd23i4", "sd23i5", "sd23i6", "sd23i7",
            "sd23i8", "sd23i9", "sd23i16", "sd23i18", "sd23i19")

round2l <- length(round2)



r2odds <- matrix(0,round2l,6)
for (i in 1:round2l) {
  myvar <- p[,round2[i]]
  # recode the variable into a binary variable having values: 
  # NULL as defaault
  p$myvarNA <- NULL
  # 1 if item nonresponse
  p$myvarNA[myvar==-2] <- 1
  p$myvarNA[myvar=="-2 Das möchte ich nicht beantworten."] <- 1
  p$myvarNA[myvar=="-2 Das möchte ich nicht beantworten"] <- 1
  p$myvarNA[myvar=="-2 Keine Angabe"] <- 1
  
  # 0 if valid value is in the variable: (positive values +  DK)
  p$myvarNA[myvar!=-2 &
              myvar!="-2 Das möchte ich nicht beantworten." &
              myvar!="-2 Das möchte ich nicht beantworten" &
              myvar!="-2 Keine Angabe" & 
              myvar!=-3 &
              myvar!="-3 Does not apply" &
              myvar!="-3 Trifft nicht zu" & 
              myvar!="-7 Incomplete data" & 
              myvar!=-4 &
              myvar!="-4 Filter error / Incorrect entry" &
              myvar!="-5" & 
              myvar!="-6" &
              myvar!=-7 ] <- 0
  
  
  # NULL as defaault
  p$myvarNADK <- NULL
  # 1 if item nonresponse
  p$myvarNADK[myvar==-2] <- 1
  p$myvarNADK[myvar=="-2 Das möchte ich nicht beantworten."] <- 1
  p$myvarNADK[myvar=="-2 Das möchte ich nicht beantworten"] <- 1
  p$myvarNADK[myvar=="-2 Keine Angabe"] <- 1
  p$myvarNADK[myvar==-1] <- 1
  p$myvarNADK[myvar=="-1 Weiß nicht"] <- 1
  
  
  # 0 if valid value is in the variable: (positive values )
  p$myvarNADK[myvar!=-1 &
                myvar!="-1 Weiß nicht" &
                myvar!=-2 &
                myvar!="-2 Das möchte ich nicht beantworten." &
                myvar!="-2 Das möchte ich nicht beantworten" &
                myvar!="-2 Keine Angabe" & 
                myvar!=-3 &
                myvar!="-3 Does not apply" &
                myvar!="-3 Trifft nicht zu" & 
                myvar!="-7 Incomplete data" & 
                myvar!=-4 &
                myvar!="-4 Filter error / Incorrect entry" &
                myvar!="-5" & 
                myvar!="-6" &
                myvar!=-7 ] <- 0
  #odds ratio with NA  
  r2odds[i,1] <- OddsRatio(table(p$myvarNA,p$erg2),conf.level=0.95)[1]                  
  #odds ratio Conf int lower bound NA
  r2odds[i,2] <- OddsRatio(table(p$myvarNA,p$erg2),conf.level=0.95)[2]
  #odds ratio Conf int upper bound NA
  r2odds[i,3] <- OddsRatio(table(p$myvarNA,p$erg2),conf.level=0.95)[3]
  #odds ratio with NADK  
  r2odds[i,4] <- OddsRatio(table(p$myvarNADK,p$erg2),conf.level=0.95)[1]                  
  #odds ratio Conf int lower bound NADK
  r2odds[i,5] <- OddsRatio(table(p$myvarNADK,p$erg2),conf.level=0.95)[2]
  #odds ratio Conf int upper bound NADK
  r2odds[i,6] <- OddsRatio(table(p$myvarNADK,p$erg2),conf.level=0.95)[3]
  print(i)
}

# I convert it to a data frame and I write a csv
r2odds <- as.data.frame(r2odds)
# I add row names (variable names)
row.names(r2odds) <- round2
# A new variable as a sum of -3 -2 -1 cases
write.csv2(r2odds,"oddsr2.csv")


# i had to delete
#table(p$rtr19k3)

round2[159]

##### Percent of possible NA-s NADK-s for each respondent
# counting for all respondents the number of -1 -2 and valid values
p$DKf <- NULL
p$NAf <- NULL
p$VAf <- NULL
for(i in 1:6201){
  print(i)
  respondent <- p[i,vl_itemNR]
  p[i,"DKf"] <- length(respondent[respondent==-1|respondent=="-1 Weiß nicht"])
  p[i,"NAf"] <- length(respondent[respondent==-2|
                             respondent=="-2 Das möchte ich nicht beantworten."|
                             respondent=="-2 Das möchte ich nicht beantworten"|
                             respondent=="-2 Keine Angabe"])
  p[i,"VAf"] <- length(respondent[respondent!=-1 &
                             respondent!="-1 Weiß nicht" &
                             respondent!=-2 &
                             respondent!="-2 Das möchte ich nicht beantworten." &
                             respondent!="-2 Das möchte ich nicht beantworten" &
                             respondent!="-2 Keine Angabe" & 
                             respondent!=-3 &
                             respondent!="-3 Does not apply" &
                             respondent!="-3 Trifft nicht zu" & 
                             respondent!=-4 &
                             respondent!="-4 Filter error / Incorrect entry" &
                             respondent!="-5" & 
                             respondent!="-6" &
                             respondent!="-7 Incomplete data" & 
                             respondent!=-7 ])
                }

table(p$NAf,useNA="always")
table(p$DKf)
table(p$VAf)
# percent of NA-s
p$NAp <- p$NAf / (p$NAf + p$DKf + p$VAf) * 100
# percent of DK-s
p$DKp <- p$DKf / (p$NAf + p$DKf + p$VAf) *100
# percent of VA-s
p$VAp <- p$VAf / (p$NAf + p$DKf + p$VAf) *100
p$temp <- NULL
p$temp = p$NAp +p$DKp + p$VAp
summary(p$NAp+p$DKp)
summary(p$NAp+p$DKp)

##### logistic regression model

#### I check the possible covariates
#gender
table(p$sex_gen, useNA = "always")
#age - we do not need this, cohort works better 
#table(p$age, useNA = "always")
#cohort 
table(p$cohort, useNA = "always")
p$cohortLO <- p$cohort
p$cohortLO <- relevel(as.factor(p$cohort), ref= "1 1991-1993")


# recode migstatus
table(p$migstatus, useNA= "always")
p$migstatusLO <- NA
p$migstatusLO <- "No migration background"
p$migstatusLO[p$migstatus=="2 1st generation"] <- "Migration background"
p$migstatusLO[p$migstatus=="3 2nd generation"] <- "Migration background"
p$migstatusLO[p$migstatus=="-7 Incomplete data"] <- "Incomplete data"
table(p$migstatusLO, useNA="always")
p$migstatusLO <- relevel(as.factor(p$migstatusLO), ref= "No migration background")

# education
#table(p$yeduc, useNA="always")  # we can not use this, 1778 cases for 0
table(p$sd27, useNA ="always") # 1778 "Does not Apply"
table(p$school, useNA="always")
table(p$isced2)
table(p$isced)
table(p$yeduc)
table(p$casmin)
#table(p$vocat)
# currently at school
table(p$sd23i1)
p$isced2LO <- p$isced2
p$isced2LO[p$isced2=="0 currently enrolled"] <- "-7 Incomplete data"
p$isced2LO <- relevel(as.factor(p$isced2LO), ref= "8 second stage of tertiary education (6)")

# language 
# no "sehr gut"
table(p$int1)
table(p$int2)
p$languageproblem <- 0
p$languageproblem[p$int1!="4 Sehr gut"|p$int2!="4 Sehr gut"] <- 1
table(p$languageproblem,useNA = "always")



# labor force status
#table(p$lfs)
table(p$casprim)

# prestige
table(p$isei)
table(p$siops)
table(p$mps)

#relationship status
table(p$relstat,useNA = "always")
p$relstatLO <- p$relstat
levels(p$relstatLO) <- c(levels(p$relstatLO),"Widowed")
p$relstatLO[p$relstat=="9 Widowed single"] <- "Widowed"
p$relstatLO[p$relstat=="10 Widowed LAT"] <- "Widowed"
p$relstatLO[p$relstat=="11 Widowed COHAB"] <- "Widowed"
p$relstatLO <- relevel(as.factor(p$relstatLO), ref= "1 Never married single")
table(p$relstatLO,useNA = "always")



#income
#table(p$hhincoecd)

# marital status
table(p$marstat, useNA="always")
p$marstatLO <- p$marstat
table(p$marstatLO,useNA="always")
p$marstatLO <- relevel(as.factor(p$marstatLO), ref= "1 Never married")


#number of previous partnerts
table(p$np, useNA = "always")
p$np[p$np==-7] <- NA
#number of previous cohabitated
table(p$ncoh)
p$ncoh[p$ncoh==-7] <- NA
#intdur
summary(p$intdur)
p$intdur[p$intdur==-7] <- NA 


# NADK-s for single variables
# item nonresponse for variables NADK is better predictor 
p$tmyvar <- NA
p$tmyvar <- p$pa13
p$pa13IN <- 0
p$pa13IN[p$tmyvar==-1|
         p$tmyvar=="-1 Weiß nicht"|
         p$tmyvar==-2|
         p$tmyvar=="-2 Das möchte ich nicht beantworten."|
         p$tmyvar=="-2 Das möchte ich nicht beantworten"|
         p$tmyvar=="-2 Keine Angabe"] <- 1 


p$tmyvar <- NA
p$tmyvar <- p$hc9h1p4
p$hc9h1p4IN <- 0
p$hc9h1p4IN[p$tmyvar==-1|
           p$tmyvar=="-1 Weiß nicht"|
           p$tmyvar==-2|
           p$tmyvar=="-2 Das möchte ich nicht beantworten."|
           p$tmyvar=="-2 Das möchte ich nicht beantworten"|
           p$tmyvar=="-2 Keine Angabe"] <- 1 

p$tmyvar <- NA
p$tmyvar <- p$hc8h1p3
p$hc8h1p3IN <- 0
p$hc8h1p3IN[p$tmyvar==-1|
           p$tmyvar=="-1 Weiß nicht"|
           p$tmyvar==-2|
           p$tmyvar=="-2 Das möchte ich nicht beantworten."|
           p$tmyvar=="-2 Das möchte ich nicht beantworten"|
           p$tmyvar=="-2 Keine Angabe"] <- 1 

# ...IN or ...INd

table(p$pa13IN)
table(p$hc9h1p4IN)
table(p$hc8h1p3IN)


# item nonresponse for variables NA is better predictor 
#sat1i1
p$tmyvar <- NA
p$tmyvar <- p$sat1i1
p$sat1i1IN <- 0
p$sat1i1IN[   p$tmyvar==-2|
              p$tmyvar=="-2 Das möchte ich nicht beantworten."|
              p$tmyvar=="-2 Das möchte ich nicht beantworten"|
              p$tmyvar=="-2 Keine Angabe"] <- 1 

#hc8h1p4
p$tmyvar <- NA
p$tmyvar <- p$hc8h1p4
p$hc8h1p4IN <- 0
p$hc8h1p4IN[   p$tmyvar==-2|
                p$tmyvar=="-2 Das möchte ich nicht beantworten."|
                p$tmyvar=="-2 Das möchte ich nicht beantworten"|
                p$tmyvar=="-2 Keine Angabe"] <- 1 

#sdp6
#p$tmyvar <- NA
#p$tmyvar <- p$sdp6
#p$sdp6IN <- 0
#p$sdp6IN[   p$tmyvar==-2|
#               p$tmyvar=="-2 Das möchte ich nicht beantworten."|
#                p$tmyvar=="-2 Das möchte ich nicht beantworten"|
#                p$tmyvar=="-2 Keine Angabe"] <- 1 
#table(p$sdp6IN)
#table(p$sdp6)
#problematic


#inc13
p$tmyvar <- NA
p$tmyvar <- p$inc13
p$inc13IN <- 0
p$inc13IN[   p$tmyvar==-2|
                p$tmyvar=="-2 Das möchte ich nicht beantworten."|
                p$tmyvar=="-2 Das möchte ich nicht beantworten"|
                p$tmyvar=="-2 Keine Angabe"] <- 1 


#hcp2
p$tmyvar <- NA
p$tmyvar <- p$hcp2
p$hcp2IN <- 0
p$hcp2IN[   p$tmyvar==-2|
               p$tmyvar=="-2 Das möchte ich nicht beantworten."|
               p$tmyvar=="-2 Das möchte ich nicht beantworten"|
               p$tmyvar=="-2 Keine Angabe"] <- 1 










# original
#plog01 <- glm (erg2 ~ sex_gen  + yeduc + cohort + migstatusLO + marstat +nkids 
#               + intdur + ncoh + np + NAp + DKp +pa13IN + hc9h1p4IN + hc8h1p3IN 
#               + sat1i1IN + hc8h1p4IN + inc13IN + hcp2IN ,
#               data=p, family = "binomial")

summary(plog01)

# how many cases we have in the model
length(plog01$fitted.values)

# probabilities
#plog01$fitted.values



#a <- predict(plog01, type="response")
#b <- residuals(plog01, type="deviance")





