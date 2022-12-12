#title: "Final Project - Adult"
#author: "Justin Feathers"
#date: '2022-12-05'

# Loads packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(dplyr)
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
  
  output$table <- renderDataTable({
    plotType <- input$plotType
    x <- input$x
    con <- input$con
    if(plotType == "Bar" & con){
      table(data, get(x))
    }
  })
  
  output$mean <- renderPrint({
    if(input$plotType == "Histogram"){
      colMeans()
    }
    else if (input$plotType == "Scatter"){
      mean(input$x3)
      mean(input$y)
    }
  })
  
  #MLR
  output$models <- renderPrint({
    
  })
    
})
  
  
  
  
  
  