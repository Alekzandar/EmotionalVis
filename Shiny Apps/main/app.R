library(shiny)
library(tidyverse)
library(devtools)
library(ggplot2)

#EmotiVis, Bucknell Senior Design

# Define UI for data upload app ----
ui <- fluidPage(
  
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
        tabPanel("Plot",  plotOutput("timeSeries")), 
        tabPanel("Summary", plotOutput("summary")),
        tabPanel("Emotional Averages", plotOutput("avg")),
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
  output$timeSeries <- renderPlot({
    #Using the user-uploaded csv file
    # inFile = input$file1
    # if (is.null(inFile))
    #   return(NULL)
    # affect <- read.csv(inFile$datapath, header = input$header)
    # req(input$file1)
    # 
    emotiplot = plotdata()
    emoti_sub = emotiplot[,c(2:10)]
    ggplot(emotiplot, aes(x = Time, group = 1)) +
      geom_line(aes(y = Joy, colour="Joy")) +
      geom_line(aes(y = Sadness, colour = "Sadness")) +
      geom_line(aes(y = Disgust, colour = "Disgust")) +
      geom_line(aes(y = Contempt, colour="Contempt")) +
      geom_line(aes(y = Anger, colour = "Anger")) +
      geom_line(aes(y = Fear, colour = "Fear")) +
      geom_line(aes(y = Surprise, colour = "Surprise")) +
      geom_line(aes(y = Valence, colour="Valence")) +
      geom_line(aes(y = Engagement, colour = "Engagement")) +
      ylab(label="Emotional Measure") + xlab("Time") +
      labs(title = "Time Varied Emotional Response")+
      scale_fill_manual(name = "Emotions", values = c("blue", "red", "orange"))
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

    
    ggplot(gather(emo, cols, value), aes(x = value)) +
      geom_histogram(binwidth = 20) + facet_grid(.~cols)
    # ggplot(stack(emo), aes(x = cols, y = values)) +
    #   geom_boxplot()
    
  })
  
  output$avg <- renderPlot({
    emo = plotdata()
    emoMean = NULL
    emoMean$joy <- mean(emo$Joy)
    emoMean$sadness <- mean(emo$Sadness)
    emoMean$disgust <- mean(emo$Disgust)
    emoMean$contempt <- mean(emo$Contempt)
    emoMean$anger <- mean(emo$Anger)
    emoMean$fear <- mean(emo$Fear)
    emoMean$surprise <- mean(emo$Surprise)
    emoMean$valence <- mean(emo$Valence)
    emoMean$engagement <- mean(emo$Engagement)
    
    listum <- list(c("Joy" = mean(emo$Joy), "Sadness" = mean(emo$Sadness), "Disgust" = mean(emo$Disgust), 
                     "Contempt" = mean(emo$Contempt), "Anger" = mean(emo$Anger), "Fear" = mean(emo$Fear), 
                     "Surprise" = mean(emo$Surprise), "Valence" = mean(emo$Valence), "Engagement" = mean(emo$Engagement) ))
    #df <- data.frame(x = emoMean)
    #attr(df, "col.names") <- c("Joy", "Sadness", "Disgust", "Contempt", "Anger", "Fear", "Surprise", "Valence", "Engagement")
    plotIt <- as.data.frame(listum)
    ggplot(data=plotIt, aes(x = plotIt[columnName])) + geom_histogram()
    
     #emoMean <- colMeans(plotdata())
    
    # emo2 <- emoMean %>%
    #   gather(
    #     'joy',
    #     'sadness',
    #     'disgust',
    #     'contempt',
    #     'anger',
    #     'fear',
    #     'surprise',
    #     'valence',
    #     'engagement',
    #     key = "emotion",
    #     value = "value") %>%
    #   select(emotion, value, time)
    
    
    # ggplot(emo2) +
    #   geom_bar(aes(x=emotion, y=value, fill=emotion), stat = "identity") +
    #   facet_wrap(~ key, ncol=2) + 
    #   labs(title ="Mean Affectiva Emotions Across Participants", subtitle="June 26, 2017 to July 28, 2017") +
    #   theme_bw()
    
  })
  
}
# Run the app ----
shinyApp(ui, server)