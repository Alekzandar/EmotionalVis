library(shiny)
library(devtools)
library(ggplot2)
library(plotly)
library(shinythemes)
library(tidyverse)
library(tools)
library(shinydashboard)

#EmotiVis, Bucknell Senior Design

#Changing Maximum Allowable FileSize to 15MB
options(shiny.maxRequestSize = 15*1024^2)


ui <- dashboardPage(
  dashboardHeader(title = "Learn about your data!"),
  dashboardSidebar(fileInput("file1", "Choose CSV File From Valid EmotiVis Affectiva Trial",
                              multiple = TRUE,
                             accept = c(".csv")),
                   tags$hr("Displayed is gathered user emotional-response data over the time for which the specific Affectiva trial run.
                            Values are scaled from 0 to 100, with 100 being the highest measured response for the given emotion."),
                   
                   # Horizontal line ----
                   tags$hr(),
                   # Input: Select number of rows to display ----
                   radioButtons("disp", "Data Table Control",
                                choices = c(All = "all",
                                            head = "head"),
                                #Defaut to displaying all data in table
                                selected = "head"),
                   # Horizontal line ----
                   tags$hr("For larger datasets select 'head' to trim the data initially displayed."),
                   # Horizontal line ----
                   tags$hr()
                   ),
  dashboardBody(
    fluidRow(
      box(title="Summary of all your emotional response data",
          plotlyOutput("timeSeries"), width = 6),
      box(plotlyOutput("avg"), width = 6 )
    ),
    fluidRow(
      box(title="Distribution of data by emotion",
          plotOutput("boxplot")),
      box(tableOutput("table"))
    ),
    fluidRow(
      box(title="Amount of user engagement at \n different time intervals", 
          plotlyOutput("gauge"), 
          uiOutput("slider")),
      box(uiOutput("select1"), 
          uiOutput("select2"), 
          plotlyOutput("scatter"))
    )
  )
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  #Read in CSV Data
  plotdata <- function()({
    req(input$file1)
    validate(
      need(file_ext(input$file1$name) %in% c(
        'csv'
      ), "Wrong File Format try again!"))
    read.csv(input$file1$datapath, header = TRUE)# sep = input$sep, header = TRUE)
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
  
  ##########################################TABLE##########################################
  #Visualize Table with Option to display desired number of rows of data
  output$table <- renderTable({
    plotdata()
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
    emots <- c("emotions_joy" = "joy", 
               "emotions_sadness" = "sadness", 
               "emotions_disgust" = "disgust", 
               "emotions_contempt" = "contmept",
               "emotions_anger" = "anger", 
               "emotions_fear" = "fear",
               "emotions_surprise" = "surprise")
      
      
    #only graphing over emotion, so removing engagement, key, valence and time columns 
    emo$emotions_engagement <- NULL
    emo$emotions_valence <- NULL
    emo$time <- NULL
    emo$key <- NULL
    matplot(emo[, 1], type="l")
    matplot(emo[, 2], type="l")
    matplot(emo[, 3], type="l")
    matplot(emo[, 4], type="l")
    matplot(emo[, 5], type="l")
     
    # sum <- ggplot(gather(emo, cols, value), aes(x= value)) +
    #   geom_line(stat = "count") + facet_grid(.~cols, labeller = as_labeller(emots))
    # sum + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
  })
  ##########################################AVERAGE BAR CHART##########################################  
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
    myPlot <- ggplot(emo2) +
      geom_bar(aes(x=emotion, y=value, fill=emotion), stat = "identity") +
      facet_wrap(~ key, ncol=2) + 
      labs(title ="Mean Affectiva Emotions Across Participants") +
      theme_bw()
    myPlot
    
    
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
  
  ##########################################BOXPLOT##########################################   
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
  
  
  ##########################################NEWPLOT##########################################
  #Scatter Plot
  
  
  output$select1 <- renderUI({
    selectInput("inSelect1", 
                "Select One", 
                c("Joy" = "joy", 
                  "Sadness" = "sadness",
                  "Disgust" = "disgust",
                  "Contempt" = "contempt",
                  "Anger" = "anger",
                  "Fear" = "fear",
                  "Surprise" = "surprise")
    ) 
  })
  
  getAns <- function(potA){
    emo = plotdata()
    ans <- switch(potA,
                  "joy" = emo$emotions_joy,
                  "sadness" = emo$emotions_sadness,
                  "disgust" = emo$emotions_disgust,
                  "contempt" = emo$emotions_contempt,
                  "anger" = emo$emotions_anger,
                  "fear" = emo$emotions_fear,
                  "surprise" = emo$emotions_surprise
    )
    return(ans)
  }
  
  output$select2 <- renderUI({
    selectInput("inSelect2", 
                "Select Another", 
                choices = c("Joy" = "joy", 
                            "Sadness" = "sadness",
                            "Disgust" = "disgust",
                            "Contempt" = "contempt",
                            "Anger" = "anger",
                            "Fear" = "fear",
                            "Surprise" = "surprise")
    ) 
  })
  
  
  output$scatter <- renderPlotly({
    emo = plotdata()
    
    s1 <- input$inSelect1
    s2 <- input$inSelect2
    
    s1Val <- getAns(input$inSelect1)
    s2Val <- getAns(input$inSelect2)
    
    toPlot <- ggplot(emo, aes(x = emo$time, y= emo$emotions_engagement , col= "engagement")) + 
      geom_point() + 
      geom_smooth(method = lm, se=FALSE) +
      geom_point(aes(y= s1Val, col = s1)) +
      geom_point(aes(y= s2Val, col = s2))
    
    
    ggplotly(toPlot)
  })
  
  
  
  
  output$downloadPlot <- downloadHandler({
    downloadHandler(
      filename =  function() {
        paste("boxplot", input$var3, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$var3 == "png")
          png(file) # open the png device
        else
          pdf(file) # open the pdf device
        myPlot # draw the plot
        dev.off()  # turn the device off
        
      } 
    )
    })  
  
}


# Run the app ----
shinyApp(ui, server)
