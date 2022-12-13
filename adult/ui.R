#title: "Final Project - Adult"
#author: "Justin Feathers"
#date: '2022-12-05'

# Loads packages
library(readr)
library(caret)
library(shiny)
library(shinydashboard)
library(DT)

#Creates UI

shinyUI(navbarPage("Final Project",
                   #Creates About page
                   tabPanel("About",
                            fluidRow(
                              column(6, 
                                     h1("What does this app do?"),
                                     wellPanel("This application utilizes a collection of data from the 1994 census database
                                            to predict whether income exceeds $50,000 annually."),
                                     ),
                              column(6,
                                     h1("Variables of interest are:"),
                                     h3("Response: Yearly Income"),
                                     br(),
                                     h3("Predictors: Age, Workclass, Fnlwgt, Education, Education Num, Marital Status,
                                        Occupation, Relationship, Race, Sex, Capital Gain, Capital Loss, Hours Per Week,
                                        and Native Country"),
                              ),
                            ),
                            img(src = "https://www.casanc.org/wp-content/uploads/census-map-of-people.jpg",
                                align = "center", width = "800px"),
                            fluidRow(
                              column(12,
                                     wellPanel("More information can be found ", 
                                               a(href = "https://archive.ics.uci.edu/ml/datasets/Adult",
                                                 target = "_blank", "here."),
                                     ),
                            ),
                            ),
                   ),


                   #Creates Data Exploration page
                   tabPanel("Data Exploration",
                            sidebarLayout(
                              sidebarPanel(
                                h4("You can create graphs using different options below."),
                                radioButtons("plotType",
                                             label = "Select the Plot Type",
                                             choices = c("Bar", "Histogram", "Scatter"),
                                             selected = "Bar"
                                             ),
                              
                                conditionalPanel(condition = "input.plotType == 'Bar'",
                                                 selectInput("x", h4("Which variable would you like to plot?"),
                                                             choices = colnames(factor),
                                                             selected = "Education"),
                                ),

                                conditionalPanel(condition = "input.plotType == 'Histogram'",
                                                 selectInput("x2", h4("Which variable would you like to plot?"),
                                                             choices = colnames(numeric),
                                                             selected = "Age"),
                                ),

                                conditionalPanel(condition = "input.plotType == 'Scatter'",
                                                 selectInput("x3", h4("Which variable would you like to plot
                                                                      on the x-axis?"),
                                                             choices = colnames(numeric),
                                                             selected = "Age"),
                                                 selectInput("y", h4("Which variable would you like to plot
                                                                     on the y-axis?"),
                                                             choices = colnames(numeric),
                                                             selected = "Hours Per Week"),
                                ),
                                radioButtons(
                                  "rbSum",
                                  label = "Which type of summary?",
                                  choices = list(
                                    "Mean",
                                    "Median")),
                                menuSubItem("summary", tabName = "Summary"),
                                
                                sliderInput("slider1",
                                            h4("How many rows of data do you want to use?"),
                                            min = 0,
                                            max = 32560,
                                            value = 32560,
                                            step = 500,
                                ),
                              ),
                              
                              mainPanel(
                                plotOutput("plot"),
                                dataTableOutput("table"),
                                verbatimTextOutput("mean"),
                              ),
                            ),
                   ),
                            
                   tabPanel("Modeling",
                                tabsetPanel(
                                  tabPanel("Modeling Info",
                                           fluidRow(
                                             column(12,
                                                    h4("Just testing some things"),
                                                    )
                                           )
                                  ),
                                  tabPanel("Model Fitting",
                                             fluidRow(
                                               column(6,
                                                      h4("More stuff here"),
                                                      #GLM
                                                      checkboxGroupInput("variables", h4("Choose desired predictor variable(s)"),
                                                                         choices = colnames(data)),
                                                      sliderInput("slider2",
                                                                  h4("Train/Test Split %"),
                                                                  min = 0,
                                                                  max = 1,
                                                                  value = 0.75
                                                                  ),
                                                      actionButton("run", h5("Crunch the numbers!")),
                                                      ),
                                               column(6,
                                                      tabsetPanel(
                                                        tabPanel("GLM", 
                                                                 h3("Fit of Logistic Regression"),
                                                                 fluidRow(
                                                                   box(title = "",  width = 10,  
                                                                       solidHeader = TRUE, collapsible = FALSE,
                                                                       verbatimTextOutput("glmSummary"),
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Decision Tree",
                                                                 h3("Fit of Decision Tree")
                                                        ),
                                                        tabPanel("Random Forest",
                                                                 h3("Fit of Random Forest")
                                                        ),
                                                      )
                                               )
                                             )
                                  ),
                                  tabPanel("Prediction",
                                           fluidRow(
                                             column(6,
                                                    h4("This is where some form of prediction interval should be created
                                                       to allow the user to input a value for x and get the predicted
                                                       value.")
                                             )
                                           )
                                  )
                                )
                   ),
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                h4("You can subset and save the data as a .csv file"),
                                checkboxGroupInput("subCol",
                                            h4("Which variables would you like to include?"),
                                            choices = colnames(data),
                                ),
                                selectInput("subRow",
                                            h4("Which variable would you like to group by?"),
                                            choices = colnames(data),
                                            ),
                                h4("Save the dataset?"),
                                downloadButton("download", "Download")
                              ),
                              mainPanel(
                                dataTableOutput("scroll")
                                
                              )
                            )
                   )
))
                         
                            
                            
                            
                            
                            
                            