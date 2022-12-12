#title: "Final Project - Adult"
#author: "Justin Feathers"
#date: '2022-12-05'

# Loads packages
library(readr)
library(caret)
library(shiny)
library(shinydashboard)
library(DT)

# Creates UI

shinyUI(navbarPage("Final Project",
                   # Creates About page
                   tabPanel("About",
                            fluidRow(
                              column(6, 
                                     h1("What does this app do?"),
                                     wellPanel("This application utilizes a collection of data from the 1994 census database
                                            to predict whether income exceeds $50,000 annually. Here is some more filler
                                            information that I will go back and edit later!!!!"),
                                     ),
                              column(6,
                                     h1("Some More Stuff"),
                                     wellPanel("More information can be found here"),
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
                   
                   # Creates Data Exploration page
                   tabPanel("Data Exploration",
                            sidebarLayout(
                              sidebarPanel(
                                h4("You can create graphs using different options below."),
                                radioButtons("plotType",
                                             label = "Select the Plot Type",
                                             choices = c("Bar", "Histogram", "Scatter"),
                                             selected = "Bar"),
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
                                                 selectInput("x2", h4("Which variable would you like to plot
                                                                      on the x-axis?"),
                                                             choices = colnames(numeric),
                                                             selected = "Age"),
                                                 selectInput("y", h4("Which variable would you like to plot
                                                                     on the y-axis?"),
                                                             choices = colnames(numeric),
                                                             selected = "Hours Per Week"),
                                ),
                                conditionalPanel(condition = "input.plotType != 'Bar'",
                                                 checkboxInput("con", h5("Also display contingency table?")),
                                                 ),
                              ),
                              mainPanel(
                                plotOutput("plot"),
                                dataTableOutput("table"),
                              )
                            )
                   )
                   )
)
                             