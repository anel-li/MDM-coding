library(tidyr)
library(dplyr)
library(stringr)

csv <-
  read.csv(file = "fakedata_assignment1_German.csv",
           stringsAsFactors = FALSE,
           strip.white = TRUE)        # leading and ending zeros excluded via import

# 1. task____________________


a1 <- csv %>%
  separate(GivenNameFull,
           into = c("GivenName", "MiddleInitial"),
           sep = " ")
#a1 %>% head(8)                       # show first 8 cases as preview

as.data.frame(table(a1$GivenName, exclude = NULL))      # table of new column to check n/a
as.data.frame(table(a1$MiddleInitial, exclude = NULL))  # table of new column to check n/a
# helping source: https://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector


# 2. task____________________

# as.data.frame(table(csv$Title))     # check which kind of titles are in the data set and its freq
#   Var1  Freq
# 1  Dr.  1544
# 2  Mr. 24695
# 3 Mrs. 11371
# 4  Ms. 12390

# helping source: https://stackoverflow.com/questions/22337394/combine-mutate-with-conditional-values

a2 <- a1 %>%
  mutate(Gender = case_when(Title == "Mr." ~ 'm',
                            Title == "Mrs." ~ 'f',
                            Title == "Ms." ~ 'f'))

#as.data.frame(table(a2$Gender, exclude = NULL))       # table of new column to check n/a

females <- a2 %>% filter(Gender == 'f')                # building a subset/list with female names
males <- a2 %>% filter(Gender == 'm')                  # building a subset/list with male names

a2 <- a2 %>%
  mutate(Gender = ifelse(
    !is.na(Gender),
    Gender,
    ifelse(
      GivenName %in% females$GivenName,
      'f',
      ifelse(GivenName %in% males$GivenName, 'm', NA) 
    )
  ))

# first attempt to sort out the title 'Dr.' via list of common names
# helping source: https://www.quora.com/Where-can-I-find-a-dataset-of-first-names-categorized-by-gender
# helping source: http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/

#female_names = scan("female.txt", what = character(), comment.char = "#")
#female_names %>% head(8)                             # show first 8 cases as preview
#male_names = scan("male.txt", what = character(), comment.char = "#")
#male_names %>% head(8)                               # show first 8 cases as preview      

#a2_b <- a2 %>%                                       
#  mutate(Gender = ifelse(!is.na(Gender), Gender,
#                         ifelse(GivenName %in% female_names, 'f',
#                                ifelse(GivenName %in% male_names, 'm', NA))))
#as.data.frame(table(a2_b$Gender, exclude = NULL))



# 3. task____________________

a3 <- a2 %>%
  separate(
    Birthday,
    into = c("BirthdayMonth", "BirthdayDay", "BirthdayYear"),
    sep = "/"
  )
#a3 %>% head(8)                                       # show first 8 cases as preview


# 4. task____________________

a4 <- a3 %>%
  mutate(State = as.numeric(factor(StateFull)))       # factor related to data entries, sorting
#a4 %>% head(8)     # show first 8 cases as preview


# 5. task____________________

a5 <- a4 %>%
  extract(
    StreetAddress,
    into = c("StreetName", "HouseNumber"),
    regex = "([[:graph:][:blank:]]+) ([[:digit:]]+)",
    remove = FALSE
  )
#as.data.frame(table(a5$StreetName, exclude = NULL))    # table of new column to check n/a
#as.data.frame(table(a5$HouseNumber, exclude = NULL))   # table of new column to check n/a


# 6. task____________________

max_len = max(nchar(a5$ZipCode))
# as.data.frame(table(a5$ZipCode, exclude = NULL))      # table of new column to check n/a
# helping source: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r

a6 <- a5 %>%
  mutate(ZipCode3Dig = str_sub(formatC(
    ZipCode,
    width = max_len,
    format = "d",
    flag = "0"
  ), 1, 3))
#as.data.frame(table(a6$ZipCode3Dig, exclude = NULL))   #check n/a or missings, leading zero


# 7. task____________________

# no need to use stringr.str_trim since we did not add any whitespace, leading and ending space already removed
a7 <- a6
a7_b <- data.frame(lapply(a7, trimws), stringsAsFactors = FALSE) 


# 8. task____________________

write.csv(a7_b,
          file = "output.csv",
          row.names = FALSE,
          na = "XXX")

