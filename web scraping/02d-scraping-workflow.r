### -----------------------------
### workflow and good practice
### -----------------------------

## peparations -------------------

library(rvest)
library(stringr)
library(robotstxt)


## Staying friendly on the web ------

# work with informative header fields
# don't bombard server
# respect robots.txt


# add header fields with rvest + httr
url <- "https://nytimes.com"
session <- html_session(url, add_headers(From = "my@email.com"))
headlines <- session %>% html_nodes(xpath = "//*[@class = 'story-heading']") %>%  html_text()


# don't bombard server
for (i in 1:length(urls_list)) {
  if (!file.exists(paste0(folder, names[i]))) {
    download.file(urls_list[i], destfile = paste0(folder, names[i]))
    Sys.sleep(runif(1, 0, 1))
  }
}

# respect robots.txt
browseURL("https://www.google.com/robots.txt")
browseURL("http://www.nytimes.com/robots.txt")

# more info see here: https://cran.r-project.org/web/packages/robotstxt/vignettes/using_robotstxt.html
paths_allowed("/", "http://google.com/", bot = "*")
paths_allowed("/", "https://facebook.com/", bot = "*")

paths_allowed("/imgres", "http://google.com/", bot = "*")
paths_allowed("/imgres", "http://google.com/", bot = "Twitterbot")
