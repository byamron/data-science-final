library(tidyverse)
library(rvest)
library(stringr)
library(tidytext)
library(tidyr)
library(lubridate)
library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = ((strong("Investigating Sexual Assault Occurrences on College Campuses"))),
    #tabPanel(h3("Creators: Divya Gudur, Ben Yamron, Selin Everett", align = "center")),
    tabPanel("Schools Reporting Levels",
             selectizeInput(inputId = "reportinglevels",
                         label = "Choose a school",
                         choices = c("Alabama University", "Alabama A & M University", "Amherst College", "Bucknell University", "Colorado College", "Middlebury College", "University of Michigan")),
             selectInput(inputId = "numberofyears",
                         label = "Number of years of missing data:",
                         choices = c("One year", "Two years", "Three years")),
             selectInput(inputId = "location",
                         label = "CLERY location:",
                         choices = c("On campus", "Off campus", "Student housing")),
             selectizeInput(inputId = "institutiontype",
                            label = "Pick an institution type",
                            choices = c("Public, 4-year or above",
                                        "Private not-for-profit, 4-year",
                                        "Private for-profit, 4-year",
                                        "Public, 2-year",
                                        "Private not-for-profit, 2-year",
                                        "Private for-profit, 2-year",
                                        "Public, less-than 2-year"))),
             
             tabPanel("Comparing Schools Sexual Assault Rates",
                      selectizeInput(inputId = "bargraphs",
                                     label = "Choose a school",
                                     choices = c("Alabama University", "Alabama A & M University", "Amherst College", "Bucknell University", "Colorado College", "Middlebury College", "University of Michigan")),
                      selectizeInput(inputId = "bargraphs",
                                     label = "Choose a school",
                                     choices = c("Alabama University", "Alabama A & M University", "Amherst College", "Bucknell University", "Colorado College", "Middlebury College", "University of Michigan"))
                      
                      ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
