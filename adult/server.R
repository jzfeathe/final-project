#title: "Final Project - Adult"
#author: "Justin Feathers"
#date: '2022-12-05'

# Loads packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(mathjaxr)
library(caret)
library(DT)
library(ggplot2)


# Reads in data
data <- read_csv("./adult.data") %>% as.tibble()
names(data) <- c("Age", "Workclass", "Fnlwgt", "Education", "Education Num", "Marital Status",
                 "Occupation", "Relationship", "Race", "Sex", "Capital Gain", "Capital Loss",
                 "Hours Per Week", "Native Country", "Yearly Income")
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

# Server
shinyServer(function(input, output, session){

  observe({
    updateSliderInput(session, "slider1",
                      value = input$slider1)
    })
  
  getData <- reactive({
    
    data$Outcome<-as.factor(data$Outcome)
    newData <- data %>% filter(!!sym(input$x) <= input$slider1)
  })

  output$plot <- renderPlot({
    plotType <- input$plotType
    x <- input$x
    x2 <- input$x2
    x3 <- input$x3
    y <- input$y
    con <- input$con
    
    if(plotType == "Bar"){
      g <- ggplot(data, aes(x = get(x)))
      g + geom_bar()
    }
    else if(plotType == "Histogram"){
      g <- ggplot(data, aes(x = get(x2)))
      g + geom_histogram()
    }
    else if(plotType == "Scatter"){
      g <- ggplot(data, aes(x = get(x3)))
      g + geom_point(aes(y = get(y)))
    }
  })
  
  output$table <- renderTable({
    #get new data 
    newData <- getData()
    
    if(input$rbSum == "Mean"){
      newData %>% group_by(Outcome) %>%
        summarise(
          Avg = round(mean(!!sym(input$x)), 0),
          Sd = round(sd(!!sym(input$x)), 0))
    }else if(input$rbSum == "Median"){
      newData %>% group_by(Outcome) %>%
        summarise(
          Median = median(!!sym(input$x)),
          IQR = round(IQR(!!sym(input$x)), 0))
    }
  })
  
  output$mean <- renderPrint({
    if(input$plotType == "Histogram"){
      mean(get(input$x2))
    }
    else if (input$plotType == "Scatter"){
      mean(input$x3)
      mean(input$y)
    }
  })
  
  #GLM
  output$models <- renderPrint({
    
  })
  
  #create a new dataset with selected variables
  Data_model<-eventReactive(input$run,{
    
    data$Outcome<-as.factor(data$Outcome)
    
    data_selected<-select(data,c(input$variables, "Outcome"))
    
    set.seed(250)
    index <- initial_split(data_selected,
                           prop = input$slider2)
    train <- training(index)
    test <- testing(index)
    list(train, test, data_selected)
    
  })
    
})
  
  
  
  
  
  