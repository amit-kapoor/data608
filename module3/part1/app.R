#
# Question 1:
# As a researcher, you frequently compare mortality rates from particular causes across different States. 
# You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, 
# from one cause (for example, Neoplasms, which are effectively cancers). Create a visualization that allows 
# you to rank States by crude mortality for each cause of death
#

library(RCurl)
library(rsconnect)
library(tidyverse)
library(plotly)
library(shiny)

rsconnect::setAccountInfo(name='****', token='****', secret='****')

# get data
the_url <- getURL('https://raw.githubusercontent.com/amit-kapoor/data608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')
mortality_df <- read_csv(the_url)

# filter for year 2010
mortality_df2010 <- filter(mortality_df, Year==2010)

# ui display
ui <- fluidPage(
  headerPanel('Compare Mortality Rates'),
  sidebarPanel(
    selectInput('cause', 'Cause', unique(mortality_df2010$ICD.Chapter), selected='Neoplasms')
  ),
  mainPanel(
    plotlyOutput('plot1')
  )
)

# server logic
server <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    
    # filter per cause from input
    mlty_cause_df <- filter(mortality_df2010, ICD.Chapter==input$cause)
    # plot
    plot <- plot_ly(x = mlty_cause_df$Crude.Rate, 
                    y = mlty_cause_df$State, 
                    type = 'bar', 
                    text = mlty_cause_df$Crude.Rate, 
                    textposition = 'auto',  
                    color = I("grey"), 
                    orientation = 'h')
    plot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
