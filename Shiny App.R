## Author: PENG Yizhuang

library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(ROCR)
library(grid)
library(reshape2)
library(viridis)
library(rpart)
library(rpart.plot)
library(caret)
library(shiny)
library(shiny)
library(shinydashboard)
library(DT)


## Application of random forest model in shiny


#function of making a prediction on 'input data' using the final model
prediction_p<- function(input_data){
  #load the model
  super_model <- readRDS("./final_model.rds")
  final_prediction<- predict(super_model,input_data)
  return (final_prediction)
}

final_result<-function(a){
  if(a[[1]]==1){
    x='Yes'
  }else{
    x='No'
  }
  return(x)
}

col_name <- c("acousticness","danceability","duration_ms", "energy",
              "explicit",    "instrumentalness","key",  "liveness",   
              "loudness",    "mode",       "speechiness",
              "tempo",       "valence")
test_data <- data.frame(matrix(ncol = 13, nrow = 1))
colnames(test_data) <- col_name

#setȱʡֵ
test_data[1,]<-rep(1,13)



spotify_A <- read.csv('spotify_subset.csv')
spotify_A$id<-NULL
spotify<- spotify_A[!duplicated(spotify_A),]


#Application try

ui<-fluidPage(titlePanel(h1("Application of random forest model", align = "center")),
              fluidRow(theme = "bootstrap.css",
                       column(tags$img(src="spotify_logo.png",width="150px",
                                       height="80px"),width=2),
                       column(br(),
                              p("Please input the ",
                                strong("attributes"), " of a song.","When you finish input, please click",strong("'Submit',"),"and we will tell you if this song is popular by using our random forest model",
                                style="text-align:justify;color:black;text-align:center;background-color:lavender;padding:15px;border-radius:10px"),
                              width=10),
                       column(numericInput(inputId ="acousticness",
                                           label = "Acousticness",
                                           value = 0.0306),
                              width=2
                       ),
                       column(
                         numericInput(inputId ="danceability",
                                      label = "Danceability",
                                      value = 0.747),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="duration_ms",
                                      label = "Duration_ms",
                                      value = 213132),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="energy",
                                      label = "Energy",
                                      value = 0.524),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="explicit",
                                      label = "Explicit",
                                      value = 1),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="instrumentalness",
                                      label = "Instrumentalness",
                                      value = 0),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="key",
                                      label = "Key",
                                      value = 10),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="liveness",
                                      label = "Liveness",
                                      value = 0.2),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="loudness",
                                      label = "Loudness",
                                      value = -6.807),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="mode",
                                      label = "Mode",
                                      value = 0),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="speechiness",
                                      label = "Speechiness",
                                      value = 0.245),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="tempo",
                                      label = "Tempo",
                                      value = 140.053),
                         width=2
                       ),
                       column(
                         numericInput(inputId ="valence",
                                      label = "Valence",
                                      value = 0.363),
                         width=2
                       ),
                       column(
                         br(),
                         actionButton("click_counter","Submit"),
                         br(),
                         width=12
                       )
              ),
              mainPanel(
                fluidRow(column(br(),
                                h3(textOutput("Result"), style="text-align:justify;color:black;text-align:center;background-color:pink;padding:15px;border-radius:10px"),
                                width=12),
                         column(br(),
                                p(strong("Note: "), "If the predicted popularity of a new song is above the average popularity of the songs in our database, it will be regarded as popular and worthy of an investment",
                                  style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                width=12)), width = 12
              )    
)   

server<-shinyServer(function(input,output){
  values<- reactiveValues(
    df = test_data
  ) 
  #values$df<- data.frame()
  
  observeEvent(input$click_counter, {
    acousticness<-(input$acousticness - min(spotify$acousticness)) / (max(spotify$acousticness) - min(spotify$acousticness))
    danceability<- (input$danceability - min(spotify$danceability)) / (max(spotify$danceability) - min(spotify$danceability))
    duration_ms<- (input$duration_ms - min(spotify$duration_ms)) / (max(spotify$duration_ms) - min(spotify$duration_ms))
    energy<-  (input$energy - min(spotify$energy)) / (max(spotify$energy) - min(spotify$energy))
    explicit<-  (input$explicit - min(spotify$explicit)) / (max(spotify$explicit) - min(spotify$explicit))
    instrumentalness<-  (input$instrumentalness - min(spotify$instrumentalness)) / (max(spotify$instrumentalness) - min(spotify$instrumentalness))
    key<-  (input$key - min(spotify$key)) / (max(spotify$key) - min(spotify$key))
    liveness<- (input$liveness - min(spotify$liveness)) / (max(spotify$liveness) - min(spotify$liveness))
    loudness<- (input$loudness - min(spotify$loudness)) / (max(spotify$loudness) - min(spotify$loudness))
    mode<-  (input$mode - min(spotify$mode)) / (max(spotify$mode) - min(spotify$mode))
    speechiness<- (input$speechiness - min(spotify$speechiness)) / (max(spotify$speechiness) - min(spotify$speechiness))
    tempo<- (input$tempo - min(spotify$tempo)) / (max(spotify$tempo) - min(spotify$tempo))
    valence<- (input$valence - min(spotify$valence)) / (max(spotify$valence) - min(spotify$valence))
    
    new_row<- c(acousticness,danceability,duration_ms,energy,explicit,instrumentalness,key,liveness,loudness,mode,speechiness,tempo,valence)
    values$df[1,]<- new_row
  })
  output$Result<-renderText({
    if(final_result(prediction_p(values$df))=='Yes'){
      a<-"This song is popular"
    }else{
      a<-"This song is not popular"
    }
  })
}
)

shinyApp(ui = ui, server = server)
