library(tidyr)
library(dplyr)
library(stringr)
library(fastLink)


write_file <- function(df, filename) {
  write.csv(df,
            file = filename,
            row.names = FALSE)
}


preprocess <- function (x) {
  # the data frame is a list hence use lapply
  # all columns but the first (-1) are altered
  x[-1] <- lapply(x[-1], str_to_upper)
  x[-1] <- lapply(x[-1], str_replace_all, pattern="STR\\.", replacement='STRASSE')
  x[-1] <- lapply(x[-1], str_replace_all, pattern='[[:blank:][:punct:]]', replacement='')
  # ß to SS is already done in str_to_upper
  x[-1] <- lapply(x[-1], str_replace_all, pattern='Ä', replacement='AE')
  x[-1] <- lapply(x[-1], str_replace_all, pattern='Ö', replacement='OE')
  x[-1] <- lapply(x[-1], str_replace_all, pattern='Ü', replacement='UE')

  #x[-1] <- lapply(x[-1], str_replace, pattern='^$', replacement=NA)

  # add leading zeros to ZipCode
  x[c('ZipCode')] <- mutate(x[c('ZipCode')], ZipCode = formatC(
    as.numeric(unlist(ZipCode)),
    width = 5,
    format = "d",
    flag = "0"
  ))

  # add leading zeros to ZipCode3Dig
  x[c('ZipCode3Dig')] <- mutate(x[c('ZipCode3Dig')], ZipCode3Dig = formatC(
    as.numeric(unlist(ZipCode3Dig)),
    width = 3,
    format = "d",
    flag = "0"
  ))

  return(x)
}

csv_aFile <-
  read.csv(file = "a-file_German_p1.csv",
           stringsAsFactors = FALSE,
           strip.white = TRUE)        # leading and ending zeros excluded via import
csv_bFile <-
  read.csv(file = "b-file_German_p1.csv",
           stringsAsFactors = FALSE,
           strip.white = TRUE)        # leading and ending zeros excluded via import


csv_aFile <- preprocess(csv_aFile)
csv_bFile <- preprocess(csv_bFile)

# remove duplicates
#count(csv_aFile[1])
#count(csv_bFile[1])
csv_aFile <- csv_aFile[!duplicated(csv_aFile),]
csv_bFile <- csv_bFile[!duplicated(csv_bFile),]
#count(csv_aFile[1])
#count(csv_bFile[1])
# actually there weren't any... (at least no exact ones)

# write preprocessed files out
write_file(csv_aFile, "a-out.csv")
write_file(csv_bFile, "b-out.csv")

col_names_out <- c("Number", "NumberBfile")


# 1. task____________________
print("task 1")

result <- inner_join(csv_aFile, csv_bFile, by = colnames(csv_aFile)[-1])
nrow(result)
write_file(result[,col_names_out], "step1.csv")

rm(result) # clean variable


# 2. task____________________
print("task 2") 

max_val <- 0
max_col <- 0
names <- colnames(csv_aFile)[-1] # colnames without the first
result <- list()

for (i in 1:length(names)) {
  result[[i]] <- inner_join(csv_aFile, csv_bFile, by = names[-i]) # join without i'th column

  # cat("bf", sum(csv_bFile[i+1] == ""))   #finding out which one to omit best - bf sum of blanks in fileb which are leading to mismatches
  # print(names[i])
  # print(nrow(result[[i]]))

  num_rows <- nrow(result[[i]])
  if (max_val < num_rows) {
    max_val <- num_rows
    max_col <- i
  }
}

print(max_val)
print(names[max_col])
write_file(result[[max_col]][,col_names_out], "step2.csv")

rm(result)  #cleaning up


# 3. task____________________
print("task 3")

ignored <- c("Number", "ZipCode3Dig")
names <- colnames(csv_aFile)
vars <- names[! names %in% ignored]
exact <- c( "Title", "MiddleInitial", "Gender", "BirthdayYear", "BirthdayMonth", "BirthdayDay", "State", "City", "HouseNumber" )
partial <- vars[! vars %in% exact ]

result <- fastLink(
  dfA = csv_aFile, dfB = csv_bFile,
  varnames = vars,
  stringdist.match = vars,
  partial.match = partial,
  n.cores = 8
)

summary(result)

duplicated(result)

# rewriting column names
colnames(result$matches) <- col_names_out
write_file(result$matches, "step3.csv")

rm(result) #cleaning up


# 4. task____________________
print("task 4")
dfA <- csv_aFile
dfB <- csv_bFile

cl.out <- clusterMatch(dfA$ZipCode, dfB$ZipCode, nclusters = 5)

dfA$cluster <- cl.out$clusterA
dfB$cluster <- cl.out$clusterB

#result <- list()
#for(i in 1:cl.out$n.clusters) {
#  x <- fastLink(
#    dfA = subset(dfA, cluster == i), dfB = subset(dfB, cluster == i),
#    varnames = vars,
#    stringdist.match = vars,
#    partial.match = partial,
#    n.cores = 8
#  )
#  result <- list(result, list(x))
#}
#agg.out <- aggregateEM(em.list = result)
# does not work...

result.1 <- fastLink(
  dfA = subset(dfA, cluster == 1), dfB = subset(dfB, cluster == 1),
  varnames = vars,
  stringdist.match = vars,
  partial.match = partial,
  n.cores = 8
)
result.2 <- fastLink(
  dfA = subset(dfA, cluster == 2), dfB = subset(dfB, cluster == 2),
  varnames = vars,
  stringdist.match = vars,
  partial.match = partial,
  n.cores = 8
)
result.3 <- fastLink(
  dfA = subset(dfA, cluster == 3), dfB = subset(dfB, cluster == 3),
  varnames = vars,
  stringdist.match = vars,
  partial.match = partial,
  n.cores = 8
)
result.4 <- fastLink(
  dfA = subset(dfA, cluster == 4), dfB = subset(dfB, cluster == 4),
  varnames = vars,
  stringdist.match = vars,
  partial.match = partial,
  n.cores = 8
)
result.5 <- fastLink(
  dfA = subset(dfA, cluster == 5), dfB = subset(dfB, cluster == 5),
  varnames = vars,
  stringdist.match = vars,
  partial.match = partial,
  n.cores = 8
)

agg.out <- aggregateEM(em.list = list(result.1, result.2, result.3, result.4, result.5))
summary(agg.out)

# merge and eliminate duplicates
result <- agg.out[[1]]$matches %>%
  union(agg.out[[2]]$matches) %>%
  union(agg.out[[3]]$matches) %>%
  union(agg.out[[4]]$matches) %>%
  union(agg.out[[5]]$matches)

colnames(result$matches) <- col_names_out
write_file(result$matches, "step4.csv")

rm(result) #cleaning up


# 5. task____________________
print("task 5")

# [1] "Number"        "Title"         "Surname"       "ZipCode"
# [5] "City"          "GivenName"     "MiddleInitial" "Gender"
# [9] "BirthdayYear"  "BirthdayMonth" "BirthdayDay"   "State"
#[13] "HouseNumber"   "StreetName"    "ZipCode3Dig"

#num
#g_title <- gammaCK2par(csv_aFile$Title, csv_bFile$Title)
g_surname <- gammaCKpar(csv_aFile$Surname, csv_bFile$Surname)
#g_zipcode <- gammaCKpar(csv_aFile$ZipCode, csv_bFile$ZipCode)
g_city <- gammaCK2par(csv_aFile$City, csv_bFile$City)
g_givenname <- gammaCKpar(csv_aFile$GivenName, csv_bFile$GivenName)
#g_middleinitial <- gammaCK2par(csv_aFile$MiddleInitial, csv_bFile$MiddleInitial)
g_gender <- gammaCK2par(csv_aFile$Gender, csv_bFile$Gender)
g_birthdayyear <- gammaCK2par(csv_aFile$BirthdayYear, csv_bFile$BirthdayYear)
g_birthdaymonth <- gammaCK2par(csv_aFile$BirthdayMonth, csv_bFile$BirthdayMonth)
g_birthdayday <- gammaCK2par(csv_aFile$BirthdayDay, csv_bFile$BirthdayDay)
#g_state <- gammaCK2par(csv_aFile$State, csv_bFile$State)
g_housenumber <- gammaCK2par(csv_aFile$HouseNumber, csv_bFile$HouseNumber)
g_streetname <- gammaCKpar(csv_aFile$StreetName, csv_bFile$StreetName)
#g_zipcode3dig

#mu_Surname       <- c(list(0.85 ), list(0.0005))
#mu_GivenName     <- c(list(0.801), list(0.002))
#mu_Gender        <- c(list(0.988), list(0.5))
#mu_BirthdayDay   <- c(list(0.958), list(0.03))
#mu_BirthdayMonth <- c(list(0.967), list(0.08))
#mu_BirthdayYear  <- c(list(0.978), list(0.02))
#mu_StreetName    <- c(list(0.792), list(0.001))
#mu_HouseNumber   <- c(list(0.821), list(0.02))
#mu_City          <- c(list(0.876), list(0.012))

m = list( 0.85 , 0.801, 0.988, 0.958, 0.967, 0.978, 0.792, 0.821, 0.876)
u = list( 0.0005, 0.002, 0.5, 0.03, 0.08, 0.02, 0.001, 0.02, 0.012)

#mu <- data.frame(mu_Surname, mu_GivenName, mu_Gender, mu_BirthdayDay, mu_BirthdayMonth, mu_BirthdayYear, mu_StreetName, mu_HouseNumber, mu_City)

gammalist <- list(g_surname, g_givenname, g_gender, g_birthdayday, g_birthdaymonth, g_birthdayyear, g_streetname, g_housenumber, g_city)
tc <- tableCounts(gammalist, nobs.a = nrow(csv_aFile), nobs.b = nrow(csv_bFile))
em.out <- emlinkMARmov(tc, nobs.a = nrow(csv_aFile), nobs.b = nrow(csv_bFile), p.gamma.k.m = m, p.gamma.k.u = u)
matches.out <- matchesLink(gammalist, nobs.a = nrow(csv_aFile), nobs.b = nrow(csv_bFile), em = em.out, thresh = 0.85)

summary(em.out)

dfA.match <- csv_aFile[matches.out$inds.a,]
dfB.match <- csv_bFile[matches.out$inds.b,]

dm.out <- dedupeMatches(dfA.match, dfB.match, EM = em.out, matchesLink = matches.out,
                        varnames = c("Surname", "GivenName", "Gender", "BirthdayDay", "BirthdayMonth", "BirthdayYear", "StreetName", "HouseNumber", "City"),
                        stringdist.match = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                        partial.match = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                        numeric.match = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

result <- select(bind_cols(dm.out$matchesA, dm.out$matchesB), col_names_out)
write_file(result, "step5.csv")