### -----------------------------------------------------
### Example: setting your user agent
### Simon Munzert
### -----------------------------------------------------


# load packages
library(tidyverse)
library(rvest)
library(httr)


# add header fields with httr::GET
browseURL("http://httpbin.org")
GET("http://httpbin.org/headers")
GET("http://httpbin.org/headers", add_headers(`User-Agent` = R.Version()$version.string))
GET("http://httpbin.org/headers", add_headers(From = "my@email.com"))
GET("http://httpbin.org/headers", add_headers(From = "my@email.com",
                                              `User-Agent` = "Simonbot"))

# add header fields with rvest + httr
url <- "http://washingtonpost.com"
session <- html_session(url, add_headers(From = "my@email.com", `User-Agent` = "Simonbot"))
headlines <- session %>% html_nodes(".text-align-inherit a") %>%  html_text()
head(headlines)
