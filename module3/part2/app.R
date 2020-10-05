#
# Data 608 - Assignment 3
# Question 2
# Often you are asked whether particular States are improving their mortality rates (per cause) faster than, or slower than, the national average. 
# Create a visualization that lets your clients see this for themselves for one cause of death at the time. Keep in mind that the national
# average should be weighted by the national population
#

library(tidyverse)
library(plotly)
library(rsconnect)
library(RCurl)
library(shiny)

rsconnect::setAccountInfo(name='****', token='****', secret='****')

# get data
the_url <- getURL('https://raw.githubusercontent.com/amit-kapoor/data608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')
mortality_df <- read_csv(the_url)

# mutate new col for national avg
mortality_df <- mortality_df %>% 
  group_by(ICD.Chapter, Year) %>% 
  mutate(national.avg = sum(Deaths)*1e5/sum(Population))

ui <- fluidPage(
  
  headerPanel("Mortality Rate by State for a given cause"),
  # Feature selection
  sidebarPanel(
    selectInput(inputId = "state", label = "Select State", choices = mortality_df$State, selected = "NY"),
    selectInput(inputId = "cause", label = "Select Cause", choices = mortality_df$ICD.Chapter, selected = "Neoplasms")),
  mainPanel(
    plotlyOutput('trends')
  )
)

server <- function(input, output){
  
  observeEvent(input$cause,{
    
    # select dataset per inputs
    mortality_subdf <-mortality_df[which(mortality_df$State == input$state & mortality_df$ICD.Chapter == input$cause),]
    
    # plot the comparison
    output$trends <- renderPlotly({
      plot_ly(mortality_subdf, x = ~Year, y = ~Crude.Rate, type = "scatter",  name = input$state, mode = "markers") %>%
        layout(title = paste("For ",input$state, " and ", input$cause),
               xaxis = list(title = 'Year'),
               yaxis = list(title = 'Deaths per 100,000'),
               dragmode =  "select",
               plot_bgcolor = "white")%>%
        add_trace(y = ~mortality_subdf$national.avg, name = 'National Average', mode = 'markers')
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
