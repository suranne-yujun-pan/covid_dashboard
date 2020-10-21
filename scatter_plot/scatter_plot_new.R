library(readxl)
library(ggplot2)

source("~/Documents/Crona/Country_Charts/get_country_data.R")
source("~/Documents/Crona/Country_Charts/get_population_data.R")

country_list <- countries_ts_c %>% 
  filter(Date == max(Date)) %>% 
  arrange(desc(positive)) %>% 
  slice(1:50) %>% 
  select(Country.Region) %>% pull()


pop <- pop %>% 
  filter(Location %in% country_list)

spdata <- countries_ts_c %>% 
  filter(Date == max(Date),
         Country.Region %in% country_list
         ) %>% 
  filter(!is.na(positive)) %>% 
  mutate(Country.Region = as.character(Country.Region)) %>% 
  full_join(., pop, by = c("Country.Region" = "Location"))%>% 
  mutate(over_65_per_population = pop_over_65/population*100,
          cases_per_population = positive/population,
         death_rate = deaths/positive*100
         )
names(spdata)

# ex <- spdata %>% arrange(desc(cases_per_population))  %>% slice(1:2)
# ex$cases_per_population <- c(9.3,8.55) # Q # B

spdata$cases_per_population[spdata$Country.Region == "Qatar"] <- 9.3
spdata$cases_per_population[spdata$Country.Region == "Bahrain"] <- 8.55
scatter_plot <-
  spdata %>% 
  ggplot(aes(x = over_65_per_population, y = cases_per_population, 
             size = death_rate))+
    theme(panel.grid.minor = element_blank(),
          #panel.grid.minor = element_line(color = "gray50", size = 0.2),
          panel.grid.major.x = element_blank(),
          #panel.grid.minor.x = element_blank(),
          #panel.grid.major.y = element_blank(),
          panel.grid.major.y = element_line(color = "gray50", size = 0.2),
          panel.background = element_blank(),
          axis.ticks.y = element_blank()
          #line = element_blank()
    )+
    geom_hline(yintercept = c(8.5, 9), color = "white")+
    geom_hline(yintercept = c(8.5, 9), color = "salmon4", size = 0.2, linetype = "dashed")+
  geom_point(#alpha = 0.5, 
             #color = "red", 
             shape = 1, fill = "White", stroke = 1,
             aes(color = Country.Region))+
  geom_text_repel(size = 4, aes(label = Country.Region)) +
  labs(x ="Age 65 and Above (% of Total Population)",
       y = "Cases Per 1,000 Population")+
    scale_size(range = c(1, 14))+
    scale_y_continuous(breaks=c(seq(0, 8, 2),8.5,9),#seq(0, 10, 2)
                       limits = c(-1, 10),#c(-1, 9.45)
                       expand = c(0,0),
                       minor_breaks = seq(0, 8, 2), #seq(0, 10, 2)
                       labels = c(seq(0, 8, 2), 10, 25) %>% as.character()
                       )+
  #scale_size(range = c(1, 14))+
    expand_limits(y = -1)+
    #scale_size(range=c(2,12),expand=c(2,0),breaks=c(0,1,10,100,1000,1000000),labels=c(">=0",">=1",">=10",">=100",">=1000",">=1000000"),guide="legend")
  #   annotate(geom="text", label=10, x=0, y=8.5,color = "salmon4") +
  #   annotate(geom="text", label=25, x=0, y=9,color = "salmon4") +
    # geom_text(data=data.frame(over_65_per_population=0,cases_per_population =10.5), 
    #           aes(over_65_per_population, cases_per_population), label= 25, vjust=-1)+
  guides(color=FALSE)+
  guides(size = guide_legend(title="Death Rate (%)")) +
  theme(legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "transparent"))#+
    # geom_point(data = ex, aes(x = over_65_per_population,
    #                           y = cases_per_population,
    #                           size = death_rate,
    #                           color = Country.Region),
    #            shape = 1, fill = "White", stroke = 1)+
    # geom_text_repel(data = ex, size = 4, aes(label = Country.Region))+
    


setwd("~/Documents/Crona/Charts/0612")
ggsave("scatter_plot.png", scatter_plot, 
       height = 5.83, width = 10.31, units = "in") 

#https://stackoverflow.com/questions/31361900/is-there-a-way-to-use-characters-instead-of-numbers-for-axes-tick-marks-in-r-gg
#https://stackoverflow.com/questions/54831853/change-a-single-gridline-color-in-ggplot2
  
