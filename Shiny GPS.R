### Load Packages ---------------------------------------
library(tidyverse)
library(shiny)
library(ggplot2)
library(ggExtra)
library(data.table)
library(lubridate)
library(tidyquant)
library(purrr)
library(zoo)
library(qcc)
library(ggpubr)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(matrixStats)


### Read Data --------------------------------------
GPS_DATA <- read.csv("4 Sessions-14 Players-2021 Data Dump CSV Export  Feb.csv")
GPS_DATA <- GPS_DATA %>% rename(Date = `Session.Date`, Player = `Player.Display.Name`, Position = `Player.Position`, Session = `Session.Type`, Drill = `Drill.Title`)

GPS_DATA %>% head()

# Insert a 0 workload value for every metric on days when no data present for that metric
impute_0_on_missing_days <- function(GPS_DATA) {
  Player <- GPS_DATA$Player[[1]]
  all_dates <- data.frame(Date = seq.Date(min(GPS_DATA$Date), max(GPS_DATA$Date), by = "day"), 
                          Player = Player)
  
  GPS_DATA <- merge(all_dates, GPS_DATA, by = c("Date","Player"), all.x = TRUE)  
  
  GPS_DATA[is.na(GPS_DATA)] <- 0
  GPS_DATA
}


### Shiny App ------------------------------------------
# User Interface

ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("GPS Analysis & Drill Output"),
  br(),
  br(),
  sidebarPanel(
    width = 3,
    h2("Please select from the inputs below to generate the outputs"),
    br(),

    selectizeInput(inputId = "Position",
                   label = "Position",
                   choices = unique(GPS_DATA$Position),
                   selected = "",
                   multiple = FALSE),
    
    selectizeInput(inputId = "Player", 
                   label = "Player",
                   choices = unique(GPS_DATA$Player),
                   selected = "",
                   multiple = TRUE),
    
    selectizeInput(inputId = "Drill", 
                   label = "Drill",
                   choices = unique(GPS_DATA$Drill),
                   selected = "",
                   multiple = FALSE)
  ),
  
  mainPanel(
    plotOutput(outputId = "Total.Distance"),
    plotOutput(outputId = "Distance.Zone.5..Relative."),
    plotOutput(outputId = "Distance.Zone.6..Relative."),
    plotOutput(outputId = "Accelerations"),
    plotOutput(outputId = "Decelerations"),
    plotOutput(outputId = "Explosive.Distance..Relative."),
    plotOutput(outputId = "High.Speed.Running..Relative."),
    plotOutput(outputId = "HSR.Per.Minute..Relative."),
    plotOutput(outputId = "Max.Speed"),
    plotOutput(outputId = "Acute"),
    plotOutput(outputId = "Chronic"),
    plotOutput(outputId = "Acute.Chronic.Ratio"),
  )
)


## server

server <- function(input, output, session){
  
  dat <- reactive({
    
    d <- GPS_DATA %>%
      filter(Position == input$Position)
    
    updateSelectizeInput(session, "Player", choices = unique(d$Player))
    
    d
  })
  
  output$Total.Distance <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Total.Distance,
          color = Player,
          fill = Player,
          group = Player,
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Total Distance (m)") + geom_text(aes(label=Total.Distance), position=position_dodge(width=0.9), vjust=-0.5) + ggtitle("Total Distance (m)") + geom_text(aes(label=Total.Distance), position=position_dodge(width=0.9), vjust=-0.5)
  })
  

  
  output$Distance.Zone.5..Relative. <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Distance.Zone.5..Relative.,
          color = Player,
          fill = Player,
          group = Player,           
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Relative Distance Zone 5 (m)") + geom_text(aes(label=Distance.Zone.5..Relative.), position=position_dodge(width=0.9), vjust=-0.5) 
    
  })
  

  
  output$Distance.Zone.6..Relative. <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Distance.Zone.6..Relative.,
          color = Player,
          fill = Player,
          group = Player,           
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Relative Distance Zone 6 (m)") + geom_text(aes(label=Distance.Zone.6..Relative.), position=position_dodge(width=0.9), vjust=-0.5)  
    
  })
  

  
  output$Accelerations <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Accelerations,
          color = Player,
          fill = Player,
          group = Player,           
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Total Number of Accelerations") + geom_text(aes(label=Accelerations), position=position_dodge(width=0.9), vjust=-0.5)  
    
  })
  

  
  output$Decelerations <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Decelerations,
          color = Player,
          fill = Player,
          group = Player,           
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Total Number of Decelerations") + geom_text(aes(label=Decelerations), position=position_dodge(width=0.9), vjust=-0.5)  
    
  })
  

  
  output$Explosive.Distance..Relative. <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Explosive.Distance..Relative.,
          color = Player,
          fill = Player,
          group = Player,           
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Relative Explosive Distance (m)") + geom_text(aes(label=Explosive.Distance..Relative.), position=position_dodge(width=0.9), vjust=-0.5)  
    
  })
  

  
  output$High.Speed.Running..Relative. <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = High.Speed.Running..Relative.,
          color = Player,
          fill = Player,
          group = Player,           
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Relative High Speed Running Distance (m)") + geom_text(aes(label=High.Speed.Running..Relative.), position=position_dodge(width=0.9), vjust=-0.5)  
    
  })
  

  
  output$HSR.Per.Minute..Relative. <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = HSR.Per.Minute..Relative.,
          color = Player,
          fill = Player,
          group = Player,           
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Relative High Speed Running Distance per Minute (m/min)") + geom_text(aes(label=HSR.Per.Minute..Relative.), position=position_dodge(width=0.9), vjust=-0.5)  
    
  })
  

  
  output$Max.Speed <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Max.Speed,
          color = Player,
          fill = Player,
          group = Player,
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Maximum Speed Achieved (m/s)") + geom_text(aes(label=Max.Speed), position=position_dodge(width=0.9), vjust=-0.5)  
    
  })
  

  
  output$Acute <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Acute,
          color = Player,
          fill = Player,
          group = Player,
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Acute Training Load (AU)") + geom_text(aes(label=Acute), position=position_dodge(width=0.9), vjust=-0.5) 
    
  })
  

  
  output$Chronic <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Chronic,
          color = Player,
          fill = Player,
          group = Player,
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Chronic Training Load (AU)") + geom_text(aes(label=Chronic), position=position_dodge(width=0.9), vjust=-0.5) 
    
  })
  

  
  output$Acute.Chronic.Ratio <- renderPlot({
    
    dat() %>% 
      filter(Player %in% input$Player) %>% 
      ggplot(
        aes(
          x = Date,
          y = Acute.Chronic.Ratio,
          color = Player,
          fill = Player,
          group = Player,
          
        )) +
      geom_histogram(stat = 'identity', position = 'dodge') + ggtitle("Acute:Chronic Workload Ratio") + geom_text(aes(label=Acute.Chronic.Ratio), position=position_dodge(width=0.9), vjust=-0.5)
  
})

}
  
shinyApp(ui, server)
