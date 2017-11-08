library(shiny)
library(tidyverse)
library(devtools)
library(ggplot2)
library(plotly)
library(shinythemes)

#EmotiVis, Bucknell Senior Design

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File From Valid EmotiVis Affectiva Trial",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr("Displayed is gathered user emotional-response data over the time for which the specific Affectiva trial run.
              Values are scaled from -100 to 100, with 100 being the highest measured response for the given emotion."),
      # Horizontal line ----
      tags$hr(),
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","), 
      

      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(All = "all",
                               head = "head"),
                   #Defaut to displaying all data in table
                   selected = "all") 
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",  plotlyOutput("timeSeries")), 
        tabPanel("Summary", plotOutput("summary")),
        tabPanel("Emotional Averages", plotlyOutput("avg")),
        tabPanel("Testing New", plotOutput("test")),
        tabPanel("Table", tableOutput("table"))
      )
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  #Read in CSV Data
  plotdata <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = input$sep, header = TRUE)
  })
  
  
  #Create a ggplot from the data within the given constraints
  output$timeSeries <- renderPlotly({
    #Using the user-uploaded csv file
    # inFile = input$file1
    # if (is.null(inFile))
    #   return(NULL)
    # affect <- read.csv(inFile$datapath, header = input$header)
    # req(input$file1)
    # 
    emotiplot = plotdata()
    emoti_sub = emotiplot[,c(2:10)]
    toPlot <- ggplot(emotiplot, aes(x = time)) +
      geom_line(aes(y = emotions_joy, colour="Joy")) +
      geom_line(aes(y = emotions_sadness, colour = "Sadness")) +
      geom_line(aes(y = emotions_disgust, colour = "Disgust")) +
      geom_line(aes(y = emotions_contempt, colour="Contempt")) +
      geom_line(aes(y = emotions_anger, colour = "Anger")) +
      geom_line(aes(y = emotions_fear, colour = "Fear")) +
      geom_line(aes(y = emotions_surprise, colour = "Surprise")) +
      geom_line(aes(y = emotions_valence, colour="Valence")) +
      geom_line(aes(y = emotions_engagement, colour = "Engagement")) +
      ylab(label="Emotional Measure") + xlab("Time") +
      labs(title = "Time Varied Emotional Response")+
      scale_fill_manual(name = "Emotions")
    ggplotly(toPlot)
  })
  
  #Visualize Table with Option to display desired number of rows of data
  output$table <- renderTable({
    if(input$disp == "head") {
      return(head(plotdata()))
    }
    else {
      return(plotdata())
    }
  })

  
  #Visualize Barchart taking means of every emotion column in the CSV
  output$summary <- renderPlot({
    emo = plotdata()
    #Removing the Time Column for the Purposes of This Plot
    emo[1] = NULL
    emo[10] = NULL
 

    
    ggplot(gather(emo, cols, value), aes(x = value)) +
      geom_line(stat = "count") + facet_grid(.~cols)
    # ggplot(stack(emo), aes(x = cols, y = values)) +
    #   geom_boxplot()
    
  })
  
  output$avg <- renderPlotly({
    emo = plotdata()
    emo$joy <- mean(emo$emotions_joy)
    emo$sadness <- mean(emo$emotions_sadness)
    emo$disgust <- mean(emo$emotions_disgust)
    emo$contempt <- mean(emo$emotions_contempt)
    emo$anger <- mean(emo$emotions_anger)
    emo$fear <- mean(emo$emotions_fear)
    emo$surprise <- mean(emo$emotions_surprise)
    emo$valence <- mean(emo$emotions_valence)
    emo$engagement <- mean(emo$emotions_engagement)
    
    emo2 <- emo %>%  
      gather(
        `joy`, 
        `sadness`, 
        `disgust`, 
        `contempt`, 
        `anger`, 
        `fear`, 
        `surprise`, 
        `valence`, 
        `engagement`, 
        key = "emotion", 
        value = "value") %>%
      select(emotion, value, time, key)
    
    ggplot(emo2) +
      geom_bar(aes(x=emotion, y=value, fill=emotion), stat = "identity") +
      facet_wrap(~ key, ncol=2) + 
      labs(title ="Mean Affectiva Emotions Across Participants") +
      theme_bw()
   
   
  })
  
  
  output$test <- renderPlot({
    
    emo = plotdata()
    emoMean = NULL
    emoMean$joy <- mean(emo$emotions_joy)
    emoMean$sadness <- mean(emo$emotions_sadness)
    emoMean$disgust <- mean(emo$emotions_disgust)
    emoMean$contempt <- mean(emo$emotions_contempt)
    emoMean$anger <- mean(emo$emotions_anger)
    emoMean$fear <- mean(emo$emotions_fear)
    emoMean$surprise <- mean(emo$emotions_surprise)
    emoMean$valence <- mean(emo$emotions_valence)
    emoMean$engagement <- mean(emo$emotions_engagement)
    
    listum <- list(c("Joy" = mean(emo$emotions_joy), "Sadness" = mean(emo$emotions_sadness), "Disgust" = mean(emo$emotions_disgust), 
                     "Contempt" = mean(emo$emotions_contempt), "Anger" = mean(emo$emotions_Anger), "Fear" = mean(emo$emotions_fear), 
                     "Surprise" = mean(emo$emotions_surprise), "Valence" = mean(emo$emotions_valence), "Engagement" = mean(emo$emotions_engagement) ))
    #df <- data.frame(x = emoMean)
    #attr(df, "col.names") <- c("Joy", "Sadness", "Disgust", "Contempt", "Anger", "Fear", "Surprise", "Valence", "Engagement")
    plotIt <- as.data.frame(listum)
    ggplot(data=plotIt, aes(x = plotIt[columnName])) + geom_histogram()
    
    
  })

  
}
# Run the app ----
shinyApp(ui, server)