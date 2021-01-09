# get_population data
library(janitor)

source("~/Documents/Portfolio/learning/covid_dashboard/source code/scatter_plot/set_who_name.R")

pop <- read_excel("~/Documents/Crona/Country_Charts/UN Population Prospects Data_2018_by age group.xlsx", 
                  sheet = "Data", skip = 1) %>% 
  slice(-c(1:26)) 
pop$pop_over_65 <- rowSums(pop[,19:ncol(pop)])
pop$population <- rowSums(pop[6:26])

pop_china <- pop %>% 
  filter(grepl("China", Location))

pop_china <- pop_china %>%
  adorn_totals("row") %>% 
  filter(Location == "-") %>% 
  mutate(Location = "China")


pop <- pop %>% 
  filter(! grepl("Southern", Location) ,
           ! grepl("Central", Location) ,
           ! grepl("Eastern", Location),
           ! grepl("Northern", Location),
           ! grepl("Western", Location),
         ! grepl("Middle", Location),
           ! grepl("Australia/New Zealand", Location),
           ! grepl("Oceania", Location),
         ! grepl("Europe", Location),
         ! grepl("Caribbean", Location),#South America
         ! grepl("South America", Location),
         ! grepl("China", Location)
         )

rm_list_pop <- c("Mayotte",
                 "Réunion",
                 "Turkmenistan",
                 "Dem. People's Republic of Korea"
                 )

pop$`ISO 3166-1 numeric code` <- as.character(pop$`ISO 3166-1 numeric code`)

pop  <- bind_rows(pop, pop_china) #%>% View()
rm(pop_china)

pop <- pop %>% 
  filter(! Location %in% rm_list_pop) %>% 
  select(Location, pop_over_65, population) 


pop$Location <- pop$Location %>% set_who_name()


# cty_names <- unique(countries_ts$Country.Region)
# pop_names <- pop$Location %>% unique()
# 
# pop_names[!pop_names %in% cty_names]
# 
# rm_list_pop <- c("Mayotte",
#                  "Réunion",
#                  "Turkmenistan")
# 
# pop_o <- read_excel("~/Documents/Crona/Country_Charts/UN Population Prospects Data_2018_by age group.xlsx", 
#                     sheet = "Data", skip = 1) 


cty_list <- countries_ts$Country.Region %>% unique()

pop <- pop %>% 
  filter(Location %in% cty_list) #%>% View()

rm(cty_list, rm_list_pop, urls, n_1, n_2, list, condition_name)
