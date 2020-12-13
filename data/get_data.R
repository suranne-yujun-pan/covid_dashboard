# data is obtained from JHU CSSE
# population data is obtained from UN Population Prospect

source("~/Documents/Crona/Country_Charts/get_country_data.R")
source("~/Documents/Portfolio/learning/covid_dashboard/source code/scatter_plot/get_population_data.R")

x <- countries_ts_c

x <- x %>% 
  arrange(Country.Region, Date)

list <- split(x, x$Country.Region)

list <- lapply(list, function(x) x %>% 
                 mutate(roll_avg = rollmean(positive_diff, k = 7, fill = NA)))

x <- do.call("rbind", list)

countries_ts_c <- x

setwd("/Users/panyujun/Documents/Portfolio/covid_dashboard/data")

write.csv(countries_ts, "countries_ts.csv", row.names = FALSE)
write.csv(countries_ts_c, "countries_ts_c.csv", row.names = FALSE)
write.csv(pop, "pop.csv", row.names = FALSE)

spdata <- countries_ts_c %>% 
  filter(Date == max(Date)
  ) %>% 
  filter(!is.na(positive)) %>% 
  mutate(Country.Region = as.character(Country.Region)) %>% 
  full_join(., pop, by = c("Country.Region" = "Location"))%>% 
  mutate(over_65_per_population = pop_over_65/population*100,
         cases_per_population = positive/population,
         death_rate = deaths/positive*100
  )

write.csv(spdata, "spdata.csv", row.names = FALSE)

# country_list <- countries_ts_c %>% 
#   filter(Date == max(Date)) %>% 
#   arrange(desc(positive)) %>% 
#   slice(1:50) %>% 
#   select(Country.Region) %>% pull()