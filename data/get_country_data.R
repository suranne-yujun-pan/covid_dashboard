# Get data ready
library(readr)
library(dplyr)
library(scales)
library(egg)
library(writexl)
library(ggrepel)
library(lubridate)
library(tidyr)
require(gridExtra)

urls <- c(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
)

condition_name <- c("positive", "deaths", "recovered")

get_data <- function(url, condition_name){
  df <- read.csv(url)
  gather_cols <- colnames(df)[5:ncol(df)]
  df <- df %>% 
    filter(Province.State == "") %>% 
    select(-Province.State, -Lat, -Long) %>% 
    gather_(., "Date", condition_name, gather_cols) %>% 
    mutate(Date = gsub("X", "", Date, fixed= TRUE),
           Date = as.Date(Date, format = "%m.%d.%y"))
  return(df)
}

list <- lapply(1:3, function(n) get_data(urls[n], condition_name[n]))

set_cty_name <- function(x){
  x <- ifelse(grepl("Czechia", x), "Czech Republic", x)
  x <- ifelse(grepl("Korea, South", x), "South Korea", x)
  x <- ifelse(x == "Burma", "Myanmar", x)
  x <- ifelse(x == "US", "United States", x)
  return(x)
}

countries_ts_c <- full_join(list[[1]], list[[2]]) %>% 
  full_join(list[[3]]) %>% 
  mutate(currently_positive = positive - deaths- recovered) %>% 
  mutate(Country.Region = as.character(Country.Region),
    Country.Region = set_cty_name(Country.Region))

countries_ts_c <- countries_ts_c %>% 
  filter(!is.na(positive)) 

# all.equal(countries_ts_c, test)

countries_ts <- countries_ts_c %>% 
  gather(., condition, count, deaths:currently_positive, factor_key = TRUE) %>%
  mutate(condition = as.character(condition)) %>%
  select(-positive) %>%
  mutate(condition = condition %>% factor(
    levels = c("recovered",
               "deaths",
               "currently_positive"))) 

# check country
check_country <- function(country){
  countries_ts_c %>% 
    filter(Country.Region == country) %>% 
    tail(n = 2)
}

# check_country("France")  #%>% View()

countries_ts$Country.Region <- as.character(countries_ts$Country.Region)
countries_ts_c$Country.Region <- as.character(countries_ts_c$Country.Region)


# Australia
# get country data
source("~/Documents/Crona/Country_Charts/Australia.R")
source("~/Documents/Crona/Country_Charts/China.R")
source("~/Documents/Crona/Country_Charts/Canada.R")


countries_ts <- bind_rows(countries_ts, Australia_ts, Canada_ts, China_ts)
countries_ts_c <- bind_rows(countries_ts_c, Australia_ts_c, Canada_ts_c, China_ts_c)

rm(Australia_ts, Canada_ts, China_ts,
    Australia_ts_c, Canada_ts_c, China_ts_c)

get_diff <- function(df){
  df %>% 
    mutate(positive_diff = c(0, diff(positive)),
           deaths_diff = c(0, diff(deaths)),
           recovered_diff = c(0, diff(recovered))
    )
}


countries_ts_c <- countries_ts_c %>% arrange(Country.Region, Date)

#test <- split(x, x$Country)

# countries_ts_cn <- lapply(test, get_diff) %>% bind_rows()
countries_ts_c  <- lapply(split(countries_ts_c, countries_ts_c$Country.Region), 
               get_diff) %>% bind_rows()

n_1 <- countries_ts_c %>% 
  group_by(Country.Region) %>% 
  summarise(n_dates = n(),
            n_distinct_dates = n_distinct(Date)) %>% 
  filter(n_dates != n_distinct_dates) %>% 
  nrow()

n_2 <- countries_ts %>% 
  group_by(Country.Region, Date, condition) %>% 
  summarise(n = n()) %>% 
  filter(n != 1) %>% 
  nrow()

if(n_1 + n_2 != 0){
  cat("Warnings: data contains error \nmultiple entries for one observation")
}


