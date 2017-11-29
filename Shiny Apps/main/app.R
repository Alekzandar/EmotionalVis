library(shiny)
library(devtools)
library(ggplot2)
library(plotly)
library(shinythemes)
library(ggthemes)

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
                      tabPanel("Boxplot", plotOutput("boxplot")),
                      tabPanel("Table", tableOutput("table")),
                      tabPanel("Gauge Plot", plotlyOutput("gauge"), uiOutput("slider"))
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
  
  
  ##########################################NEWPLOT##########################################
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
  
  ##########################################NEWPLOT##########################################
  #Visualize Table with Option to display desired number of rows of data
  output$table <- renderTable({
    if(input$disp == "head") {
      return(head(plotdata()))
    }
    else {
      return(plotdata())
    }
  })
  
  ##########################################NEWPLOT##########################################  
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
  ##########################################NEWPLOT##########################################  
  #Visualize Barchart taking means of every emotion column in the CSV
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
    
    #Function generates new barchart for every seperate user detected
    ggplot(emo2) +
      geom_bar(aes(x=emotion, y=value, fill=emotion), stat = "identity") +
      facet_wrap(~ key, ncol=2) + 
      labs(title ="Mean Affectiva Emotions Across Participants") +
      theme_bw()
    
    
  })
  
  #Testing Random Charts for Translation and Rendering into R
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
                     "Surprise" = mean(emo$emotions_surprise), "Valence" = mean(emo$emotions_valence), 
                     "Engagement" = mean(emo$emotions_engagement) ))
    #df <- data.frame(x = emoMean)
    #attr(df, "col.names") <- c("Joy", "Sadness", "Disgust", "Contempt", "Anger", "Fear", "Surprise", "Valence", "Engagement")
    plotIt <- as.data.frame(listum)
    ggplot(data=plotIt, aes(x = plotIt[columnName])) + geom_histogram()
    
    
  })
  
  output$boxplot <- renderPlot({
    dat <- plotdata()
    emotions <- c("Joy", "Sadness", "Disgust", "Fear", "Contempt", "Surprise", "Anger")
    emo_to_color <- c("gold", "steelblue2", "sienna4", "grey69", "pink2", "tan2", "firebrick2")
    
    bp <- ggplot(dat) + geom_boxplot(aes(y =emotions_joy, x= factor("Joy")), color = "gold") + 
      geom_boxplot(aes(y =emotions_sadness, x= factor("Sadness")), color = "steelblue2") +
      geom_boxplot(aes(y =emotions_disgust, x= factor("Disgust")), color = "sienna4") + 
      geom_boxplot(aes(y =emotions_fear, x= factor("Fear")), color = "grey69") +
      geom_boxplot(aes(y =emotions_contempt, x= factor("Contempt")), color = "pink2") + 
      geom_boxplot(aes(y =emotions_surprise, x= factor("Surprise")), color = "tan2") + 
      geom_boxplot(aes(y =emotions_anger, x= factor("Anger")), color = "firebrick2") + 
      ylab("Mean of each emotion detected") + xlab("Emotion") 
    bp
    
    
  })
  
  output$slider <- renderUI({
    emo = plotdata()
    engageArr <- emo$emotions_engagement 
    timeArr <- emo$time
    
    sliderInput("inSlider", "Time Value: ", min = 1, max = length(timeArr), value = 1, step =1)}) 
  
  ##########################################GAUGE PLOT##########################################  
  #Visualize Gauge Plot taking means of every emotion column in the CSV
  output$gauge <- renderPlotly({ 
    #Put Code Here
    
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, 10, 10, 10, 10, 10, 10),
      labels = c("-", "0", "20", "40", "60", "80", "100"),
      rotation = 108,
      direction = "clockwise",
      hole = 0.4,
      textinfo = "label",
      textposition = "outside",
      hoverinfo = "none",
      domain = list(x = c(0, 0.48), y = c(0, 1)),
      marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)')),
      showlegend = FALSE
    )
    
    base_plot <- add_trace(
      base_plot,
      type = "pie",
      values = c(50, 10, 10, 10, 10, 10),
      labels = c("Engagement Meter", "Not Engaged", "Not Very Engaged", "Somewhat Engaged", "Very Engaged", "Fully Engaged"),
      rotation = 90,
      direction = "clockwise",
      hole = 0.3,
      textinfo = "label",
      textposition = "inside",
      hoverinfo = "none",
      domain = list(x = c(0, 0.48), y = c(0, 1)),
      marker = list(colors = c('rgb(255, 255, 255)', 'rgb(232,226,202)', 'rgb(226,210,172)', 'rgb(223,189,139)', 'rgb(223,162,103)', 'rgb(226,126,64)')),
      showlegend= FALSE
    )
    
    
    emo = plotdata()
    engageArr <- emo$emotions_engagement 
    timeArr <- emo$time
    
    sliderValue <- input$inSlider
    sliderText <- paste('\n At time ', timeArr[sliderValue] , '\n Engagement = ' , engageArr[sliderValue])
    
    a <- list(
      showticklabels = FALSE,
      autotick = FALSE,
      showgrid = FALSE,
      zeroline = FALSE)
    
    b <- list(
      xref = 'paper',
      yref = 'paper',
      x = 0.23,
      y = 0.45,
      showarrow = FALSE,
      text = sliderText)
    
    
    
    #currEngagement = emotions_engagement
    #These are the potential paths for engagement....
    #Only need to adjust middle tw values (could be better specified)
    
    nEngaged = 'M 0.235 0.5 L 0.18 0.54 L 0.245 0.5 Z'
    nVEngaged = 'M 0.235 0.5 L 0.20 0.58 L 0.245 0.5 Z'
    sEngaged = 'M 0.235 0.5 L 0.24 0.62 L 0.245 0.5 Z'
    vEngaged = 'M 0.235 0.5 L 0.28 0.58 L 0.245 0.5 Z'
    fEngaged = 'M 0.235 0.5 L 0.30 0.54 L 0.245 0.5 Z'
    
    if (engageArr[sliderValue] < 20){
      currEngage = nEngaged
    }else if (engageArr[sliderValue] < 40){
      currEngage = nVEngaged
    }else if (engageArr[sliderValue] < 60){
      currEngage = sEngaged
    }else if (engageArr[sliderValue] < 80){
      currEngage = vEngaged
    }else{
      currEngage = fEngaged
    }
    
    base_chart <- layout(
      base_plot,
      shapes = list(
        list(
          type = 'path',
          path = currEngage,
          xref = 'paper',
          yref = 'paper',
          fillcolor = 'rgba(44, 160, 101, 0.5)'
        )
      ),
      xaxis = a,
      yaxis = a,
      annotations = b
    )
    
    
  })
  
  
}


# Run the app ----
shinyApp(ui, server)
