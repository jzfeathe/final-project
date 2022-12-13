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
                 "Hours Per Week", "Native Country", "Income")
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

# Server
shinyServer(function(input, output, session){

  observe({
    updateSliderInput(session, "slider1",
                      value = input$slider1)
    })
  
  getData <- reactive({

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
      newData %>% group_by(Income) %>%
        summarise(
          Avg = round(mean(!!sym(input$x)), 0),
          Sd = round(sd(!!sym(input$x)), 0))
    } else if(input$rbSum == "Median"){
      newData %>% group_by(Income) %>%
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
  
  #create a new dataset with selected variables
  model<-eventReactive(input$run,{
    
    data_selected<-select(data, c(input$variables, "Income"))
    
    set.seed(250)
    index <- initial_split(data_selected,
                           prop = input$slider2)
    train <- training(index)
    test <- testing(index)
    list(train, test, data_selected)
    
  })
  
  #GLM
  log <- reactive({
    #fit training data with glm
    glmFit <-
      glm(Income ~ .,
          data = model()[[1]],
          family = "binomial")
    
    # predicted probability of glm
    glmProb <-
      predict(glmFit, 
              newdata = model()[[2]], 
              type = "response")
    
    # predicted outcome of glm
    glmPred <- rep(0, length(glmProb))
    glmPred[glmProb > 0.5] <- 1
    
    #glm performance
    glmConf <-
      confusionMatrix(data = factor(glmPred) ,
                      reference = model()[[2]]$Income)
    glmAcc <- glmConff$overall[[1]] #accuracy
    
    glmRoc <-
      roc(response = model()[[2]]$Income,
          predictor = glmProb)
    glmAuc <- auc(glmRoc)
    
    list(glmFit, glmAcc, glmAuc)
  })
  
  #create glmSummary text info
  output$glmSummary <- renderPrint({
    summary(log()[[1]])
  })
  
  
  #### Classification 
  tree <- reactive({
    #fit training data with tree
    tree_class <- rpart(
      Income ~ .,
      data = model()[[1]],
      method = 'class',
      parms = list(split = "information"),
      control = rpart.control(
        xval = 5,
        minbucket = 2,
        cp = 0
      )
    )
  })
  
 subData <- reactive({
    input$subCol
    updateSelectInput(session, "subRow", choices = input$subCol)
    subData <- data %>% select(input$subCol) %>%
      group_by(input$subRow)
  })
  
  output$scroll <- renderDataTable({
    subData()
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.csv(subData, file)
    }
  )

 
  
  
 
})
  
  
  