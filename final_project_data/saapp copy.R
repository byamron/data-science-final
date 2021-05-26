library(tidyverse)
library(rvest)
library(stringr)
library(tidytext)
library(tidyr)
library(lubridate)
library(shiny)
library(geojsonio)
library(leaflet)
library(sf)
library(sp)
library(ggtext)

#read in data
noncampus.crime <- read_csv("noncampuscrime161718.csv")
noncampus.vawa <- read_csv("noncampusvawa161718.csv")
oncampus.crime <- read_csv("oncampuscrime161718.csv")
oncampus.vawa <- read_csv("oncampusvawa161718.csv")
reshall.crime <- read_csv("residencehallcrime161718.csv")
reshall.vawa <- read_csv("residencehallvawa161718.csv")

#select columns for crime data
noncampus.crime2 <- noncampus.crime %>%
  select("UNITID_P",
         "INSTNM",
         "BRANCH",
         "Address",
         "City",
         "State",
         "ZIP",
         "sector_cd",
         "Sector_desc",
         "men_total",
         "women_total",
         "Total",
         "RAPE16",
         "FONDL16",
         "STATR16",
         "RAPE17",
         "FONDL17",
         "STATR17",
         "RAPE18",
         "FONDL18",
         "STATR18")

oncampus.crime2 <- oncampus.crime %>%
  select("UNITID_P",
         "INSTNM",
         "BRANCH",
         "Address",
         "City",
         "State",
         "ZIP",
         "sector_cd",
         "Sector_desc",
         "men_total",
         "women_total",
         "Total",
         "RAPE16",
         "FONDL16",
         "STATR16",
         "RAPE17",
         "FONDL17",
         "STATR17",
         "RAPE18",
         "FONDL18",
         "STATR18")

reshall.crime2 <- reshall.crime %>%
  select("UNITID_P",
         "INSTNM",
         "BRANCH",
         "Address",
         "City",
         "State",
         "ZIP",
         "sector_cd",
         "Sector_desc",
         "men_total",
         "women_total",
         "Total",
         "RAPE16",
         "FONDL16",
         "STATR16",
         "RAPE17",
         "FONDL17",
         "STATR17",
         "RAPE18",
         "FONDL18",
         "STATR18")

crime.list <- list(noncampus.crime2,
                   oncampus.crime2,
                   reshall.crime2)

vawa.list <- list(noncampus.vawa,
                  oncampus.vawa,
                  reshall.vawa)

#begin joining
crime.joined <- crime.list %>% reduce(inner_join, by = c("UNITID_P",
                                                         "INSTNM",
                                                         "BRANCH",
                                                         "Address",
                                                         "City",
                                                         "State",
                                                         "ZIP",
                                                         "sector_cd",
                                                         "Sector_desc",
                                                         "men_total",
                                                         "women_total",
                                                         "Total"))

vawa.joined <- vawa.list %>% reduce(inner_join, by = c("UNITID_P",
                                                       "INSTNM",
                                                       "BRANCH",
                                                       "Address",
                                                       "City",
                                                       "State",
                                                       "ZIP",
                                                       "sector_cd",
                                                       "Sector_desc",
                                                       "men_total",
                                                       "women_total",
                                                       "Total"))

#join all data sets
total.joined <- crime.joined %>%
  inner_join(vawa.joined, by = c("UNITID_P",
                                 "INSTNM",
                                 "BRANCH",
                                 "Address",
                                 "City",
                                 "State",
                                 "ZIP",
                                 "sector_cd",
                                 "Sector_desc",
                                 "men_total",
                                 "women_total",
                                 "Total")) %>%
  filter(Sector_desc %in% c("Public, 4-year or above", "Private nonprofit, 4-year or above")) %>%
  filter(!(is.na(State)))

#rename columns based on original data sets
colnames(total.joined) <- str_replace_all(colnames(total.joined),"x", "OFF")
colnames(total.joined) <- str_replace_all(colnames(total.joined),"y", "ON")

#create data frame where NAs are replaced with zeros
total.joined.replaced.na <- total.joined %>%
  filter(!(is.na(State)))
total.joined.replaced.na[is.na(total.joined.replaced.na)] = 0

#calculate total metrics and rates by population
total.metrics <- total.joined.replaced.na %>%
  group_by(INSTNM) %>%
  summarize(total.rape = sum(RAPE16,RAPE16.OFF, RAPE16.ON,
                             RAPE17 , RAPE17.OFF, RAPE17.ON,
                             RAPE18,RAPE18.OFF,RAPE18.ON),
            total.statr = sum(STATR16,STATR16.OFF,STATR16.ON,
                              STATR17,STATR17.OFF,STATR17.ON ,
                              STATR18,STATR18.OFF,STATR18.ON),
            total.fondl = sum(FONDL16 ,FONDL16.OFF ,FONDL16.ON , 
                              FONDL17,FONDL17.OFF ,FONDL17.ON ,
                              FONDL18 ,FONDL18.OFF ,FONDL18.ON),
            total.vawa = sum(DOMEST16 ,DOMEST16.OFF , DOMEST16.ON ,
                             DOMEST17, DOMEST17.OFF,DOMEST17.ON,
                             DOMEST18,DOMEST18.OFF,DOMEST18.ON,
                             DATING16, DATING16.OFF, DATING16.ON,
                             DATING17,DATING17.OFF,DATING17.ON,
                             DATING18,DATING18.OFF,DATING18.ON,
                             STALK16,STALK16.OFF,STALK16.ON,
                             STALK17,STALK17.OFF,STALK17.ON,
                             STALK18, STALK18.OFF,STALK18.ON),
            Total.pop = mean(Total),
            men_total = mean(men_total),
            women_total = mean(women_total),
            rate.rape = (total.rape/Total.pop)*100,
            rate.statr = (total.statr/Total.pop)*100,
            rate.fondl = (total.fondl/Total.pop)*100,
            rate.vawa = (total.vawa/Total.pop)*100)

#add type of school back to total metrics df
school.type <- total.joined.replaced.na %>%
  select(INSTNM, Sector_desc)

metrics.full <- total.metrics %>%
  inner_join(school.type, by = "INSTNM") %>%
  unique()

#Compare two schools
#create df to display reported cases over time
total.long <- total.joined.replaced.na %>%
  select(-c(UNITID_P, Address, CitON, ZIP, sector_cd)) %>%
  pivot_longer(-c(INSTNM, BRANCH, State, Sector_desc, men_total, women_total, Total),
               names_to = "crime.details",
               values_to = "reported cases") %>%
  mutate(location = ifelse(str_detect(crime.details, "ON"),
                           "On Campus",
                           ifelse(str_detect(crime.details, "OFF"),
                                  "Off Campus",
                                  "Residence Hall"))) %>%
  mutate(crime = ifelse(str_detect(crime.details, "RAPE"),
                        "Rape",
                        ifelse(str_detect(crime.details, "FONDL"),
                               "Fondling",
                               ifelse(str_detect(crime.details, "STATR"),
                                      "Statutory Rape",
                                      ifelse(str_detect(crime.details, "DOMEST"),
                                             "Domestic Violence",
                                             ifelse(str_detect(crime.details, "STALK"),
                                                    "Stalking", "Dating Violence")))))) %>%
  mutate(year = ifelse(str_detect(crime.details, "16"),
                       "2016",
                       ifelse(str_detect(crime.details, "17"),
                              "2017", "2018")))

#school comparison - single metric
total.long %>%
  filter(INSTNM %in% c("Middlebury College", "Williams College")) %>%
  filter(location == "On Campus") %>%
  filter(crime == "Fondling") %>%
  ggplot(aes(x = as.numeric(year),
             y = `reported cases`,
             fill = INSTNM)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_bw() + 
  labs(title = "Reported Cases over Time",
       subtitle = "2016-2018",
       x = "Year",
       y = "Reported Cases") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "School", values = c("dodgerblue3", "orange"))

#create long version of metrics df
metrics.long <- metrics.full %>%
  pivot_longer(-c('INSTNM',
                  'total.rape',
                  'total.statr',
                  'total.fondl',
                  'total.vawa',
                  'Total.pop',
                  'men_total',
                  'women_total',
                  'Sector_desc'),
               names_to = "crime",
               values_to = "rate") %>%
  #str_replace_all(metrics.full$crime, "rate.rape", "Rape") %>%
  #str_replace_all(crime, "rate.fondl", "Fondling") %>%
  # str_replace_all(crime, "rate.statr", "Statutory Rape") %>%
  # str_replace_all(crime, "rate.vawa", "VAWA") %>%
  select(c(INSTNM,
           Sector_desc,
           crime,
           rate))

metrics.long$crime <- str_replace_all(metrics.long$crime, "rate.rape", "Rape") 
metrics.long$crime <- str_replace_all(metrics.long$crime, "rate.fondl", "Fondling") 
metrics.long$crime <- str_replace_all(metrics.long$crime, "rate.statr", "Statutory Rape") 
metrics.long$crime <- str_replace_all(metrics.long$crime, "rate.vawa", "VAWA")

#school comparison - rates
metrics.long %>%
  filter(INSTNM %in% c("Middlebury College", "University of Michigan-Ann Arbor")) %>%
  ggplot(aes(x = INSTNM,
             y = rate,
             fill = crime)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_bw() + 
  labs(title = "Cases per 100 Students",
       subtitle = "2016-2018",
       x = "School",
       y = "Cases per Total Population (%)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "Crime", values = c("dodgerblue3", "orange", "plum1", "olivedrab3")) #+
  #coord_flip()

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = strong("Investigating Sexual Assault Occurrences on College Campuses"),
    #tabPanel(h3("Creators: Divya Gudur, Ben Yamron, Selin Everett", align = "center")),
    tabPanel("Cover",
             h3("Math 216 Final Project", align = "center"),
             h3("Creators: Divya Gudur, Ben Yamron, Selin Everett", align = "center"),
             h4(em("We have neither given nor received unauthorized aid on this assignment"),  align = "center")),
             
    tabPanel("Schools' Reporting Levels",
             selectizeInput(inputId = "reportinglevels",
                         label = "Choose a school",
                         choices = c(unique(metrics.long$INSTNM))),
            
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
             
             tabPanel("Comparing Schools' Sexual Assault Rates",
                      selectizeInput(inputId = "school1",
                                     label = "Choose a school",
                                     choices = c(unique(total.long$INSTNM)),
                                     multiple = F),
                      selectizeInput(inputId = "school2",
                                     label = "Choose another school",
                                     choices = c(unique(total.long$INSTNM)),
                                     multiple = F),
                      selectInput(inputId = "loc",
                                  label = "Choose a CLERY location",
                                  choices = c(unique(total.long$location))),
                      selectInput(inputId = "offense",
                                  label = "Choose an offense",
                                  choices = c(unique(total.long$crime))),
                      plotOutput("crimes_over_time")
                      #plotOutput("rates")
                      ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$crimes_over_time <- renderPlot(
    total.long %>%
      #filter(INSTNM %in% c("Middlebury College", "Williams College")) %>%
      #filter(location == "On Campus") %>%
      #filter(crime == "Fondling") %>%
      filter(INSTNM %in% c(input$school1, input$school2)) %>%
      filter(location == input$loc) %>%
      filter(crime == input$offense) %>%
      ggplot(aes(x = as.numeric(year),
                 y = `reported cases`,
                 fill = INSTNM)) +
      geom_bar(stat = "identity",
               position = "dodge") +
      theme_bw() + 
      labs(title = "Reported Cases over Time",
           subtitle = "2016-2018",
           x = "Year",
           y = "Reported Cases") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      scale_fill_manual(name = "School", values = c("dodgerblue3", "orange"))
    
  )
}

shinyApp(ui = ui, server = server)
