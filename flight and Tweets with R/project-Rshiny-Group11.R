library(shiny)
library(tidyverse)
library(psych)
library(ggthemes)
library(shinythemes)


## import data for airplane delay
newdata <- read_csv("newdata.csv")
usa <- read_csv("usa.csv")
a <- read_csv("Tweets.csv")
## vec1 for sentiment 
d <- a %>% select(c(airline_sentiment, airline))
e1 <- as.data.frame(unique(d %>% select(airline_sentiment)))

vec1 <- vector()
for (i in (1 : dim(e1)[1])){
  vec1[[i]] <- e1[i, 1]
}

## vec for company 
e <- as.data.frame(unique(d %>% select(airline)))

vec <- vector()
for (i in (1 : dim(e)[1])){
  vec[[i]] <- e[i, 1]
}

ui <- navbarPage("Flight Delay Visualization",
                 inverse = TRUE,
                 theme = shinythemes::shinytheme("readable"),
           tabPanel("Exploratory Visualization",
                    sidebarPanel(
                      selectInput(
                        inputId = "mainone",
                        label = "Please select",
                        choices = c("Map","Jitter","Bar"),
                        selected = "twitter"),
                      
                      conditionalPanel(
                        condition = "input.mainone == 'Map'",
                        selectInput(
                          inputId = "size",
                          label = "Select the Variable",
                          choices = c("arr_flights",	"arr_del15",	"carrier_ct",	"weather_ct",
                                      "nas_ct","security_ct",	"late_aircraft_ct",	"arr_cancelled",
                                      "arr_diverted",	"arr_delay",	"carrier_delay",
                                      "weather_delay","nas_delay",	"security_delay",	
                                      "late_aircraft_delay"),
                          selected = "arr_cancelled"
                        ),
                      ),
                      
                      conditionalPanel(
                        condition = "input.mainone == 'Bar'",
                        selectInput(
                          inputId = "airname",
                          label = "Select Airport",
                          choices = newdata$airport,
                          selected = "JFK"
                        ),
                        selectInput(
                          inputId = "varbar",
                          label = "Select the Variable",
                          choices = c("arr_flights",	"arr_del15",	"carrier_ct",	"weather_ct",
                                      "nas_ct","security_ct",	"late_aircraft_ct",	"arr_cancelled",
                                      "arr_diverted",	"arr_delay",	"carrier_delay",
                                      "weather_delay","nas_delay",	"security_delay",	
                                      "late_aircraft_delay"),
                          selected = "arr_cancelled"
                        ),
                      ),
                      
                      conditionalPanel(
                        condition = "input.mainone == 'Jitter'",
                        selectInput(
                          inputId = "airnamechart",
                          label = "Select Airport",
                          choices = newdata$airport,
                          selected = "JFK"
                        ),
                        selectInput(
                          inputId = "year",
                          label = "Select Year",
                          choices = c(2015, 2016, 2017, 2018, 2019, 2020, 2021),
                          selected = 2015
                        ),
                        selectInput(
                          inputId = "month",
                          label = "Select Month",
                          choices = c(1,2,3,4,5,6,7,8,9,10,11,12),
                          selected = 2
                        )
                      )
                    ),
                    mainPanel(
                      plotOutput(outputId = "plot")
                    )
                   
), 
           tabPanel("Twitter Sentiment",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("one", "Select one", c(choose = "Choice...","sentiment", "company")),
                        uiOutput("obsl")),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("General",  plotOutput("q2")),
                          tabPanel("Specific",plotOutput("q3"))
                        )
                      )
                    )
           )
)

server <- function(input, output, session) {

  output$plot <- renderPlot({
    
    if (input$mainone == "Map") {
      ggplot(NULL) +
        geom_polygon(data = usa, mapping = aes(x = long,y = lat,group = group),
                     fill = "white", color = "black" ) +
        geom_point(data = newdata, 
                   aes(x = Longitude, y = Latitude, 
                       size = newdata[[input$size]], 
                       color = newdata[[input$size]]),
                   shape = 21
                       ) +
        xlab("Longitude") + ylab("Latitude") + 
        scale_size_continuous(name = input$size, range = c(0,20)) +
        theme_classic() + 
        scale_color_continuous(name = input$size, type = "viridis")
      
    } else if (input$mainone == "Bar") {
      df <- newdata %>% subset(airport == input$airname)
      ggplot(df) +
        geom_bar(mapping = aes(x = carrier_name, y = df[[input$varbar]], fill = carrier_name), 
                 stat = "identity", show.legend = FALSE) +
        theme(axis.text.x = element_text(angle = 75, vjust = 0.5, size = 7)) + 
        labs(x = "Carrier Name",y = input$varbar)
      
    } else if (input$mainone == "Jitter") {
      h1 = filter(newdata, carrier_name != "ExpressJet Airlines LLC" )
      h1 <- h1 %>% 
        subset(airport == input$airnamechart) %>% 
        subset(month == input$month) %>% 
        subset(year == input$year)
      
      ggplot(h1) +
        geom_jitter(aes(x = h1$airport, y = h1$arr_del15/h1$arr_flights * 100,
                        color = h1$carrier_name,
                        alpha = h1$arr_cancelled/h1$arr_del15 *100,
                        size = h1$arr_del15
                        )) +
        theme(axis.text.y = element_text(color = "black"),
              legend.text = element_text(size=8)) + 
        coord_flip() +
        labs(title="Flight Cancellation and Delays",
             caption="source: Bureau of Transportations Statistics", 
             x = "Airport",
             y = "Chance of Delay (%)",
             color = "Carrier Name/Airline Company",
             size = "# of delayed flights",
             alpha = "Cancelled flights/Delayed flights (%)"
             )+ 
        scale_alpha_continuous(trans = 'reverse')
    }
  }
  )

  
  # plot of general sentiment and company 
  
  observeEvent(input$one, {
    
    if (input$one == "sentiment"){
      output$obsl <- renderUI({
        selectInput(
          inputId = "senti",
          label = "Select sentiment",
          choices =  c(choose = "Choice..." ,vec1)
        )
      })
      
    } 
    else if(input$one == "company"){
      output$obsl <- renderUI({
        selectInput(
          inputId = "air",
          label = "Select Company",
          choices = c(choose = "Choice..." ,vec)
        )
      })
      
    } else{output$obsl <- NULL}
  })
  
  output$q2 <- renderPlot({
    if(input$one == "sentiment"){
      # Bar chart of airline sentiment attitude
      b <- a %>% select(airline_sentiment)
      b1 <- na.omit(b)
      ggplot(b1) +
        geom_bar(aes(airline_sentiment, y = ..prop.., group = 1),
                 fill = c("#0072B2", "#D55E00", "#CC79A7")) +
        geom_hline(yintercept = c(0.2, 0.4, 0.6), color = "black",
                   linetype = "longdash")+
        theme_tufte()+
        theme(axis.text = element_text(color = "black", size = 15), 
              axis.title = element_text(color = "black", size = 17),
              plot.title = element_text(size = 20, color = "firebrick"))+
        labs(title = "Airline Sentiment Analysis", x = "Sentiment")
    } 
    else if (input$one == "company"){
      ## Lolipop chart of airline sentiment
      
      c <- a %>% select(airline)
      c1 <- c %>%
        group_by(airline) %>%
        mutate(count = n())
      c2 <- mutate(c1, Freq = count/dim(c1)[1])
      
      ggplot(c2 %>% distinct(), aes(x = airline, y = Freq)) +
        geom_point(color = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2"), size =15) +
        geom_segment(aes(x = airline,
                         xend = airline,
                         y = 0,
                         yend = Freq),linetype = "dashed", size = 0.5) +
        geom_text(aes(label = round(Freq, 2)), color = "white", size = 5) +
        theme_tufte() +
        theme(axis.text = element_text(color = "black", size = 15),
              axis.title = element_text(color = "black", size = 17),
              plot.title = element_text(size = 20, color = "firebrick"))+
        labs(title = "Distribution of Airline Companies", x = "Airline",
             y = "% of Airline Companies")
    } 
    else {
      NULL
    }
    
  })
  
  output$q3 <- renderPlot({
    
    if (input$one == "company"){
      
      ggplot(filter(d, airline == input$air)) +
        geom_bar(aes(airline_sentiment),
                 fill = c("#F0E442", "#0072B2", "#D55E00"),
                 show.legend = FALSE,
                 width = 1) +
        labs(title = (paste(input$air, "Airline Sentiment")))+
        theme(aspect.ratio = 1) +
        theme(axis.text = element_text(color = "black", size = 15),
              axis.title = element_text(color = "black", size = 17),
              plot.title = element_text(size = 20, color = "ivory4")) +
        labs(x = NULL, y = NULL) +
        coord_polar()+
        theme(plot.background = element_rect(fill = "lightblue3", colour = NA))
      
      
    }
    else if (input$one == "sentiment"){
      
      ggplot(filter(d, airline_sentiment == input$senti)) +
        geom_bar(aes(airline),
                 fill = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2"),
                 show.legend = FALSE,
                 width = 1) +
        labs(title = (paste(input$senti, "Attitude")))+
        theme(aspect.ratio = 1) +
        theme(axis.text = element_text(color = "black", size = 10),
              axis.title = element_text(color = "black", size = 15),
              plot.title = element_text(size = 20, color = "ivory4"),
              axis.text.x = element_text(angle=-75)) +
        labs(x = NULL, y = NULL) +
        coord_polar()+
        theme(plot.background = element_rect(fill = "lightblue3", colour = NA))
    } else {}
    
    
    
  })

  
  
}

shinyApp(ui = ui, server = server)