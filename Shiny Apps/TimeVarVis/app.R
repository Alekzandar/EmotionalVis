library(shiny)
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
      tags$hr()
      
      
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("timeSeries")
      
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  #Read in CSV Data
  plotdata <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = input$sep)
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
      geom_line(aes(y = Joy), colour="blue") +
      geom_line(aes(y = Sadness), colour = "red") +
      geom_line(aes(y = Disgust), colour = "green") +
      geom_line(aes(y = Contempt), colour="orange") +
      geom_line(aes(y = Anger), colour = "yellow") +
      geom_line(aes(y = Fear), colour = "pink") +
      geom_line(aes(y = Surprise), colour = "grey") +
      geom_line(aes(y = Valence), colour="coral") +
      geom_line(aes(y = Engagement), colour = "skyblue") +
      ylab(label="Emotional Measure") + xlab("Time") +
      labs(title = "Time Varied Emotional Response")
  })
  
}
# Run the app ----
shinyApp(ui, server)