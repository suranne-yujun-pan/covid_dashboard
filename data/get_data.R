# data is obtained from JHU CSSE
# population data is obtained from UN Population Prospect

source("~/Documents/Crona/Country_Charts/get_country_data.R")
source("~/Documents/Portfolio/learning/covid_dashboard/source code/scatter_plot/get_population_data.R")

setwd("/Users/panyujun/Documents/Portfolio/covid_dashboard/data")

write.csv(countries_ts, "countries_ts.csv", row.names = FALSE)
write.csv(countries_ts_c, "countries_ts_c.csv", row.names = FALSE)
write.csv(pop, "pop.csv", row.names = FALSE)