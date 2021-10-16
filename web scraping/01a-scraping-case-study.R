### -----------------------------
## web scraping
### -----------------------------

## load packages -----------------
library(rvest)
library(stringr)
library(ggmap)
library(maps)

# parse URL
parsed_url <- read_html("https://en.wikipedia.org/wiki/Berlin")

# extract data
parsed_nodes <- html_nodes(parsed_url, xpath = "//div[contains(@class, 'column-count-3')]//li")
cities <- html_text(parsed_nodes)
cities[1:10]

# tidy data
cities <- str_replace(cities, "\\[\\d+\\]", "")
cities[1:10]

# extract variables
year <- str_extract(cities, "\\d{4}")
city <- str_extract(cities, "[[:alpha:] ]+") %>% str_trim
country <- str_extract(cities, "[[:alpha:] ]+$") %>% str_trim
year[1:10]
city[1:10]
country[1:10]

# make data frame
cities_df <- data.frame(year, city, country)
head(cities_df)

# geocode observations
cities_coords <- geocode(paste0(cities_df$city, ", ", cities_df$country), source = "google")
cities_df$lon <- cities_coords$lon
cities_df$lat <- cities_coords$lat
cities_df$lon[1:10]
cities_df$lat[1:10]

# map observations
map_world <- borders("world", colour = "gray50", fill = "white") 
ggplot() + map_world + geom_point(aes(x = cities_df$lon, y = cities_df$lat), color = "red", size = 1) + theme_void()
