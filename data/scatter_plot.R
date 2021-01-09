source("~/Documents/Crona/Country_Charts/get_country_data.R")
source("~/Documents/Portfolio/learning/covid_dashboard/source code/scatter_plot/get_population_data.R")

# country_list <- countries_ts_c %>% 
#   filter(Date == max(Date)) %>% 
#   arrange(desc(positive)) %>% 
#   slice(1:50) %>% 
#   select(Country.Region) %>% pull()
# 
# pop_50 <- pop %>%
#   filter(Location %in% country_list)

spdata <- countries_ts_c %>%
  filter(Date == max(Date)#,
         #Country.Region %in% country_list
  ) %>%
  filter(!is.na(positive)) %>%
  mutate(Country.Region = as.character(Country.Region)) %>%
  full_join(., pop, by = c("Country.Region" = "Location"))%>%
  mutate(over_65_per_population = pop_over_65/population*100,
         cases_per_population = positive/population,
         death_rate = deaths/positive*100
  )

set.seed(42)


base_p <- 
#  spdata %>% 
  # ggplot(aes(x = over_65_per_population, y = cases_per_population, 
  #            size = death_rate))+
  ggplot()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray50", size = 0.2),
        panel.background = element_blank(),
        axis.ticks.y = element_blank()
  )+
  geom_point( data = spdata,
              aes(x = over_65_per_population, y = cases_per_population, 
                            size = death_rate,color = Country.Region),
    shape = 1, fill = "White", stroke = 1,
    #aes(color = Country.Region), 
    alpha = 0.3)+
  geom_text_repel(
    data = spdata,
    aes(x = over_65_per_population, y = cases_per_population, 
        size = death_rate, label = Country.Region),
    size = 4, alpha = 0.3) +
  labs(x ="Age 65 and Above (% of Total Population)",
       y = "Cases Per 1,000 Population")+
  scale_size(range = c(1, 14))+
  expand_limits(y = -1)+
  guides(color=FALSE)+
  guides(size = guide_legend(title="Death Rate (%)")) +
  theme(legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "transparent"))


add_point_df <- spdata %>% filter(Country.Region == "United Kingdom")

base_p +
  geom_point( data = add_point_df,
              aes(x = over_65_per_population, y = cases_per_population, 
                  size = death_rate,color = Country.Region),
              shape = 1,  stroke = 1)+
  geom_text_repel(
    data = add_point_df,
    aes(x = over_65_per_population, y = cases_per_population, 
        size = death_rate, label = Country.Region),
    size = 4)
  

setwd("/Users/panyujun/Documents/Portfolio/covid_dashboard/data")

#write.csv(pop_50, "pop_50.csv",row.names=FALSE)
write.csv(pop, "pop.csv", row.names = FALSE)
write.csv(spdata, "spdata.csv", row.names = FALSE)
write.csv(countries_ts, "countries_ts.csv", row.names = FALSE)
write.csv(countries_ts_c, "countries_ts_c.csv", row.names = FALSE)


country_list <- 
  spdata %>% 
  arrange(desc(positive)) %>% 
  slice(1:50) %>% 
  select(Country.Region) %>% pull()
