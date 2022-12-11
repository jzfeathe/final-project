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
})