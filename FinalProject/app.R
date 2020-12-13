# title: "Data 608 - Final Project"
# author: "Amit Kapoor"
# date: "12/01/2020"

# load necessary libraries
library(shiny)
library(maps)
library(RCurl)
library(tidyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(janitor)
library(plotly)
library(shinybusy)
library(ggiraph)

# get data
airlines_url <- getURL('https://raw.githubusercontent.com/amit-kapoor/data608/master/FinalProject/airlines.csv')
airports_url <- getURL('https://raw.githubusercontent.com/amit-kapoor/data608/master/FinalProject/airports.csv')
routes_url <- getURL('https://raw.githubusercontent.com/amit-kapoor/data608/master/FinalProject/routes.csv')

# read csv
airlines <- read_csv(airlines_url)
airports <- read_csv(airports_url)
routes <- read_csv(routes_url)

# update country values
airports$Country[airports$Country=='Congo (Kinshasa)'] <- 'Democratic Republic of the Congo'
airports$Country[airports$Country=='Congo (Brazzaville)'] <- 'Republic of Congo'

#rename column
colnames(routes)[1]<-'IATA'

# add new column in airports by combining IATA and Airport Name
airports <- mutate(airports, iata_name = paste(IATA, "-" , Country)) 


# **** Routes ******  
#merge routes and airline data
routes1<-data.frame(merge(routes, airlines %>% select(IATA, Name), by='IATA',sort=F))

countries_map <-map_data("world")
world_map <- ggplot() + 
  geom_map(data = countries_map, 
           map = countries_map,aes(x = long, y = lat, map_id = region, group = group),
           fill = "white", color = "black", size = 0.2)

# To find routes from a given airport, we first create a function routes_through as below:
  ## create a dataframe filtering the given airport - d1
  ## get all the destination airports
  ## merge it with the airports.csv data - d2
  ## cbind d1 and d2

routes_through <- function(iata_start){
  
  d1 <- data.frame(routes1 %>% filter(source_airport == iata_start))
  d1.iata.start<-data.frame(d1 %>% select(destination_airport) %>% rename(IATA = destination_airport))
  
  #merge with the airport data
  d2<-data.frame(merge(d1.iata.start,airports %>% select(Name,City,Country,IATA,Latitude,Longitude),by='IATA', sort=F))
  colnames(d2)<-c("iata_end","arp_name_dest","city_name_dest","sntry_name_dest","lat_end","long_end")
  
  #get geo locations of source.airport
  lat.start<-rep(airports[airports$IATA==iata_start,'Latitude'],nrow(d1))
  long.start<-rep(airports[airports$IATA==iata_start,'Longitude'],nrow(d1))
  d1$lat.start = lat.start
  d1$long.start = long.start
  
  d1_row_count <- nrow(d1)
  d2_row_count <- nrow(d2)
  
  if ( (d1_row_count == 0 & d2_row_count ==0 ) | (d1_row_count != d2_row_count)) {
    res <- NULL
  } else {
    #cbind all
    res <- data.frame(cbind(d1,d2))
  }
  
  return(res)
}
# **** Routes ******  



# **** Busiest ******  
# This function accepts number of aircrafts and returns a dataframe with relevant details. 
# In this function first we subset data for given airline, loop over and get log and lat. 
# We will then get routes of these 4 types and finall draw on map

airlineConnect <- function(routes, name){
  
  arpt_src<-c()
  arpt_dest<-c()
  arpt_src_long<-c()
  arpt_src_lat<-c()
  arpt_dest_long<-c()
  arpt_dest_lat<-c()
  
  for(i in 1:nrow(routes)){
    arpt_src[i]<-routes$source_airport[i]
    arpt_dest[i]<-routes$destination_airport[i]
    arpt_src_long[i]<- airports[airports$IATA==arpt_src[i],'Longitude']
    arpt_src_lat[i]<- airports[airports$IATA==arpt_src[i],'Latitude']
    arpt_dest_long[i]<- airports[airports$IATA==arpt_dest[i],'Longitude']
    arpt_dest_lat[i]<- airports[airports$IATA==arpt_dest[i],'Latitude']
  }
  
  res<-data.frame('arl_name' = rep(name,nrow(routes)),
                  'arpt_src'= arpt_src,
                  'arpt_dest'= arpt_dest,
                  'arpt_src_long'= unlist(arpt_src_long),
                  'arpt_src_lat'= unlist(arpt_src_lat),
                  'arpt_dest_long'= unlist(arpt_dest_long),
                  'arpt_dest_lat'= unlist(arpt_dest_lat))
  
  return(res)
}



routes$num_aircraft <- sapply(routes$Equipment, function(x) length(strsplit(x," ")[[1]]))

routes_6_aircrafts <- routes %>% dplyr::filter(num_aircraft==6)
routes_7_aircrafts <- routes %>% dplyr::filter(num_aircraft==7)
routes_8_aircrafts <- routes %>% dplyr::filter(num_aircraft==8)
routes_9_aircrafts <- routes %>% dplyr::filter(num_aircraft==9)

routes_6 <- airlineConnect(routes_6_aircrafts, 'routes having 6 aircrafts')
routes_7 <- airlineConnect(routes_7_aircrafts, 'routes having 7 aircrafts')
routes_8 <- airlineConnect(routes_8_aircrafts, 'routes having 8 aircrafts')
routes_9 <- airlineConnect(routes_9_aircrafts, 'routes having 9 aircrafts')

tot_routes <- rbind(routes_6, routes_7, routes_8, routes_9)



# For Airports dropdown
airport_dd <- airports %>% select(Name, IATA, Country) %>% filter(IATA != '\\N') %>% unique
# remove special characters from dataframe
airport_dd <- data.frame(sapply(airport_dd, function(x) gsub("[^\x20-\x7E]", "", x)), stringsAsFactors=FALSE)
# mutate label as IATA - Airport name
airport_dd <- mutate(airport_dd, iata_name = paste(IATA, "-" , Name)) 



# ******** Airline routes

#merge routes and airline data
airlines_dd <- data.frame(merge(routes %>% select(AirlineID) , airlines %>% select(AirlineID, ICAO, Name), by='AirlineID',sort=F)) %>% unique
#  mutate(aid_name = paste(ICAO, Name, sep = ' - ')) %>% unique

# remove special characters from dataframe
airlines_dd <- data.frame(sapply(airlines_dd, function(x) gsub("[^\x20-\x7E]", "", x)), stringsAsFactors=FALSE)

my_choices <- as.list(airlines_dd$Name)
names(my_choices) <- airlines_dd$Name


routes<-data.frame(merge(routes, airlines %>% select(IATA, Name), by='IATA',sort=F))



# ******** Airline routes

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # for showing notification
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(30%);
             left: calc(40%);
             }
             "
      )
    ),
    tags$div(class="title", titlePanel("Data 608 Final Project - OpenFlights"), align="center")
  ),
  # spinner during page load
  add_busy_spinner(spin = "fading-circle", position = c("bottom-left")),
  br(),
  # show country, airport and airlines drop down
  fluidRow(
    column(3, selectInput('cntry', 'Countries:', sort(unique(airports$Country)), selected='United States' )),
    column(4, uiOutput("airprt")),
    column(4, selectInput('airline', 'Airlines:', choices = my_choices, selected='American Airlines'))
  ), 
  hr(),
  # tabs for various visualization
  tabsetPanel(type = "tabs",
              tabPanel("Most Airports", plotOutput("mostairp", height=700)),
              tabPanel("Most Airlines", plotOutput("mostairl", height=700)),
              tabPanel("Routes from Airport", plotOutput("route", height=700)),
              tabPanel("Airline routes", plotOutput("alroute", height=700)),
              tabPanel("Busiest", plotOutput("busiest", height=700))
  )
  
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
#  output$sel_cntry <- renderText({
#    input$cntry
#  })
  
  # render airport based on country selected
  output$airprt <- renderUI({
    airport_dd <- airport_dd %>% filter(Country == input$cntry)
    ap_choices <- as.list(airport_dd$IATA)
    names(ap_choices) <- airport_dd$iata_name
    selectInput('airprt', 'Airports:', choices = ap_choices, selected='JFK', width="500px")
  })
  
  # Countries having most airports
  output$mostairp <- renderPlot({
    data.frame(table(airports$Country)) %>% 
      arrange(desc(Freq)) %>% 
      head(25) %>% 
      ggplot(aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1, label = Freq)) + 
      geom_bar(stat = "identity", show.legend = F) +
      labs(title = "Top 25 Countries having most Airports", 
           x = "Country", y = "The number of Airports") +
      geom_text(show.legend = F, vjust = -.5) + 
      scale_fill_viridis_d(option = "cividis") +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 40, size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  })
  
  # Countries running most airports
  output$mostairl <- renderPlot({
    data.frame(table(airlines$Country)) %>% 
      arrange(desc(Freq)) %>% head(25) %>% 
      ggplot(aes(x = reorder(Var1, -Freq), y = Freq, 
                 fill = Var1, label = Freq)) + 
      geom_bar(stat = "identity", show.legend = F) +
      geom_text(show.legend = F, vjust = -.5) + 
      scale_fill_viridis_d(option = "cividis") +  #plasma, magma, inferno, cividis
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 40, size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = "Country", y = "The number of Airlines", 
           title = "Top 25 Countries having most airlines")
  })
  
  # Airports routes
  output$route <- renderPlot({
    
    arp_origin <- routes_through(input$airprt)
    
    if ( !is.null(arp_origin) ) {
      arp_origin_grpd <- arp_origin %>% 
        select(arp_name_dest ,lat.start, long.start,lat_end,long_end) %>% 
        group_by(arp_name_dest) %>% 
        mutate(count=n()) %>% 
        distinct()

      maxFlights <- max(arp_origin_grpd$count)
      
      world_map + 
        geom_curve(data=arp_origin_grpd, 
                   aes(x=unlist(long.start),
                       y=unlist(lat.start),
                       xend=unlist(long_end),
                       yend=unlist(lat_end),
                       color=factor(count)),
                   curvature = 0.25, arrow = arrow(length = unit(0.008, "npc")), alpha=.70,size=1) + 
        geom_point_interactive(data=arp_origin_grpd, 
                               aes( tooltip=arp_name_dest, label= arp_name_dest, x=unlist(long_end),y=unlist(lat_end)), size=0.5) + 
        theme_fivethirtyeight() + theme(
          legend.title = element_text(face = "bold", size=15), 
          legend.text = element_text(colour="black", size=15, color = "orangered4"), 
          panel.grid.major = element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) + 
        scale_color_manual(name="Routes serving airlines",values=rev(viridis::viridis(maxFlights))) +
        ggtitle(paste0('Departures from ',(airports %>% filter(IATA==input$airprt))$Name))
      
    } else {
      showNotification('Not enough data available.')
    }

    
  })
  
  # Busiest routes
  output$busiest <- renderPlot({
    world_map + 
      geom_curve(data=tot_routes,aes(x=arpt_src_long,
                                     y=arpt_src_lat,
                                     xend=arpt_dest_long,
                                     yend=arpt_dest_lat,
                                     color=arl_name),
                 curvature = 0.2, arrow = arrow(length = unit(0.005, "npc")), alpha=1,size=.5) + 
      theme_fivethirtyeight() + 
      theme(
        legend.position="bottom",
        legend.text = element_text(colour="black", size=15, color = "orangered4"), 
        panel.grid.major = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),plot.title=element_text(face="bold",hjust=0,vjust=.75,colour="#3C3C3C",size=19),
        plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black")) + 
      labs(
        title="Busiest Routes in the World",
        subtitle="Routes shown here use 6 or more different aircrafts") +
      scale_color_manual(name="",values= c("orange" ,"red", "green", "blue"))
    
  })
  
  # Airline Routes
  output$alroute <- renderPlot({
    
    al_noncs <- routes %>% dplyr::filter(Name==input$airline) %>% dplyr::filter(is.na(Codeshare))
    al_cs <- routes %>% dplyr::filter(Name==input$airline & Codeshare == 'Y')

    # To check error
    assign("alroute-error", FALSE, env=globalenv())
    
    tryCatch({
      
      al_noncs_routes <- airlineConnect(al_noncs, input$airline)
      al_cs_routes <- airlineConnect(al_cs, input$airline)
      
    }, warning = function(w) {
      assign("alroute-error", TRUE, env=globalenv())
    }, error = function(e) {
      assign("alroute-error", TRUE, env=globalenv())
    })

    if(!get("alroute-error", env=globalenv())) {
      
      w1 <- world_map + 
        geom_curve(data=al_noncs_routes, aes(x=arpt_src_long, 
                                                y=arpt_src_lat, 
                                                xend=arpt_dest_long, 
                                                yend=arpt_dest_lat #, color=Codeshare
        ), 
        curvature = 0.3, 
        arrow = arrow(length = unit(0.005, "npc")), 
        color = "orangered2",
        alpha=.5,size=.25) + 
        theme_fivethirtyeight() +
        #theme_classic() +
        theme(legend.position=c(.85,1.04),
              panel.grid.major = element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              plot.title=element_text(face="bold",hjust=0,vjust=.8,colour="#3C3C3C",size=20),
              plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black")) + 
        labs(title=paste0("Routes taken by airplanes for ",al_noncs_routes$arl_name),
             subtitle="Operated directly by airline") + 
        scale_color_brewer(palette='Set1')
      
      w2<- world_map + 
        geom_curve(data=al_cs_routes, aes(x=arpt_src_long, 
                                          y=arpt_src_lat, 
                                          xend=arpt_dest_long, 
                                          yend=arpt_dest_lat #, color=Codeshare
        ), 
        curvature = 0.3, 
        arrow = arrow(length = unit(0.005, "npc")), 
        color = "green4",
        alpha=.5,size=.25) + 
        theme_fivethirtyeight() +
        theme(legend.position=c(.85,1.04),
              panel.grid.major = element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              plot.title=element_text(face="bold",hjust=0,vjust=.8,colour="#3C3C3C",size=20),
              plot.subtitle=element_text(size=15, hjust=0, face="italic", color="black")) + 
        labs(title=paste0("Routes taken by airplanes for ",al_cs_routes$arl_name),
             subtitle="Operated not directly by airline but through another carrier") + 
        scale_color_brewer(palette='Set1')
      
      w1+w2
      
    } else {
      showNotification('Not enough data available.')
    }

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
