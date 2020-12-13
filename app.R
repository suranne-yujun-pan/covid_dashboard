library(shiny)
library(tidyverse)

x <- read.csv("https://raw.githubusercontent.com/suranne-yujun-pan/covid_dashboard/master/data/countries_ts_c.csv",
              stringsAsFactors=FALSE) #%>% View()
x$Date <- x$Date %>% as.character() %>% as.Date()

x <- x %>% 
  filter(positive_diff >= 0)

df <- read.csv("https://raw.githubusercontent.com/suranne-yujun-pan/covid_dashboard/master/data/countries_ts.csv",
               stringsAsFactors=FALSE)
df$Date <- df$Date %>% as.character() %>% as.Date()

names(df)[3] <- "cases"
df$cases <- ifelse(df$cases == "currently_positive", 
                   "currently positive", df$cases)

spdata <- read.csv("https://raw.githubusercontent.com/suranne-yujun-pan/covid_dashboard/master/data/spdata.csv")

spdata$Date <- spdata$Date %>% as.character() %>% as.Date()

# Define ui 
ui <- fluidPage(
  titlePanel("Covid-19 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Countries or Territories", sort(unique(x$Country.Region))),
      dateInput('start_date', 'Start Date', value = '2020-01-22',
                min = '2020-01-22', max = '2020-12-12'),
      dateInput('end_date', 'End Date', value = '2020-12-12',
                min = '2020-01-22', max = '2020-12-12')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cases", plotly::plotlyOutput('p1_bar')),
        tabPanel("Recovered Cases", plotly::plotlyOutput('p2_re')),
        tabPanel("Epidemiological Characteristics", plotOutput('p3_epi'))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$p1_bar <- plotly::renderPlotly({
    x %>% 
      filter(Date >= input$start_date & Date <= input$end_date,
             Country.Region == input$country) %>% # check
      ggplot(aes(x = Date, y = positive_diff, group = 1))+
      geom_bar(stat="identity", fill="steelblue")+
      geom_line(aes(x = Date, y = roll_avg, col = "7-day moving average"),
                col = "red")+
      ylab("Daily New Cases")+
      theme_minimal()+
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ',',
                                      decimal.mark = '.'))+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom")+
      labs(subtitle="Daily New Cases with 7-day Moving Average")
  })
  
  output$p2_re <- plotly::renderPlotly({
   df %>% filter(
      Date >= input$start_date & Date <= input$end_date,
      Country.Region == input$country
    ) %>% 
    ggplot() + 
      geom_area(aes(y = count, x = Date, fill = cases), 
                stat="identity")+
      scale_fill_manual(values=c("#0AB3A3", "#404747", "#FAA36B"),
                        labels = c("recovered", "deaths", "currently positive"))+
      theme_minimal()+
      xlab("") + ylab("")+
      theme(#legend.position = "none",
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_text(size = 14) , #size=12, angle=90, hjust=1,
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #plot.title = element_text(size = 20, face = "bold"),
        axis.text.y=element_text(size = 12),
        plot.title = element_text(hjust = 0.5)
        #panel.grid.major.y = element_line(color = "black",size=0.05),
        #panel.grid.major = element_blank(),
      )+
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ',',
                                      decimal.mark = '.'))
  })
  
  output$p3_epi <- renderPlot({
    country_list <- 
      spdata %>% 
      arrange(desc(positive)) %>% 
      slice(1:50) %>% 
      select(Country.Region) %>% pull()
    spdata_base <-spdata %>% 
      filter(Country.Region != input$country,
             Country.Region %in% country_list)
    base_p <- 
      ggplot()+
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray50", size = 0.2),
            panel.background = element_blank(),
            axis.ticks.y = element_blank()
      )+
      geom_point( data = spdata_base,
                  aes(x = over_65_per_population, y = cases_per_population, 
                      size = death_rate,color = Country.Region),
                  shape = 1, fill = "White", stroke = 1,
                  #aes(color = Country.Region), 
                  alpha = 0.3)+
      geom_text_repel(
        data = spdata_base,
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
    
    
    add_point_df <- spdata %>% filter(Country.Region == input$country)
    
    base_p +
      geom_point( data = add_point_df,
                  aes(x = over_65_per_population, y = cases_per_population, 
                      size = death_rate,color = Country.Region),
                  shape = 1,  stroke = 1)+
      geom_text_repel(
        data = add_point_df,
        aes(x = over_65_per_population, y = cases_per_population, 
            size = death_rate, label = Country.Region),
        size = 6)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

