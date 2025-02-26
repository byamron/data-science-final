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
  scale_fill_manual(name = "Crime", values = c("dodgerblue3", "orange", "plum1", "olivedrab3"))
  # + coord_flip()

#################### Missing Data Graphs ####################

#create data frame and filter out some school types
total.joined.na.narrative <- crime.joined %>%
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
  filter(!(is.na(State))) %>%
  filter(!(Sector_desc %in% c("Administrative Unit Only",
                              "Private for-profit, less-than 2-year",
                              "Private nonprofit, less-than 2-year",
                              "Public, less-than 2-year")))

#rename columns based on original data sets
colnames(total.joined.na.narrative) <- str_replace_all(colnames(total.joined.na.narrative),"x", "OFF")
colnames(total.joined.na.narrative) <- str_replace_all(colnames(total.joined.na.narrative),"y", "ON")

total.na.edited <- total.joined.na.narrative %>% select(RAPE16.ON,RAPE17.ON, RAPE18.ON,
                                                        RAPE16.OFF, RAPE17.OFF, RAPE18.OFF,
                                                        RAPE16, RAPE17, RAPE18, 
                                                        DOMEST16.OFF, DOMEST17.OFF, DOMEST18.OFF,
                                                        DOMEST16.ON, DOMEST17.ON, DOMEST18.ON,
                                                        DOMEST16, DOMEST17, DOMEST18)

#convert missing data to binary values
total.na.edited[!is.na(total.na.edited)] = 0
total.na.edited[is.na(total.na.edited)] = 1

#create totals for each metric
total.na.scores <- total.na.edited %>% cbind(total.joined.na.narrative$INSTNM,
                                             total.joined.na.narrative$Sector_desc) %>%
  mutate(on.campus.sex.offenses = RAPE16.ON + RAPE17.ON + RAPE18.ON,
         off.campus.sex.offenses = RAPE16.OFF + RAPE17.OFF + RAPE18.OFF,
         residential.hall.sex.offenses = RAPE16 + RAPE17 + RAPE18,
         on.campus.VAWA.crimes = DOMEST16.ON + DOMEST17.ON + DOMEST18.ON,
         off.campus.VAWA.crimes = DOMEST16.OFF + DOMEST17.OFF + DOMEST18.OFF,
         residential.hall.VAWA.crimes = DOMEST16 + DOMEST17 + DOMEST18) %>%
  select(19:26)

#organize by type of school
total.per.type <- total.joined.na.narrative %>%
  count(Sector_desc)

total.na.scores2 <- left_join(total.na.scores, total.per.type, 
                              by = c("total.joined.na.narrative$Sector_desc" = "Sector_desc"))

#lengthen data
total.na.scores.long <- total.na.scores2 %>% 
  pivot_longer(-c(`total.joined.na.narrative$INSTNM`, `total.joined.na.narrative$Sector_desc`, n),
               names_to = "location",
               values_to = "frequency")

#calculate overall proportion of missing values for each school type
heat.map.na.values <- total.na.scores.long %>% 
  add_count(`total.joined.na.narrative$Sector_desc`, 
            location, 
            frequency,
            name = "number")  %>%
  mutate(proportion = number/n)

#add placeholder NA values for columns with missing info
fix.na <- tibble(`total.joined.na.narrative$Sector_desc` = c("Private nonprofit, 2-year",
                                                             "Private nonprofit, 2-year",
                                                             "Private for-profit, 2-year",
                                                             "Private for-profit, 2-year"),
                 n = c(165, 165, 760, 760),
                 location = c("residential.hall.sex.offenses", 
                              "residential.hall.VAWA.crimes", 
                              "residential.hall.sex.offenses",
                              "residential.hall.VAWA.crimes"),
                 frequency = c(2, 2, 2, 2),
                 number = c(NA, NA, NA, NA),
                 proportion = c(NA, NA, NA, NA))

#rejoin data
heat.map.full <- full_join(heat.map.na.values, fix.na, by = c("total.joined.na.narrative$Sector_desc",
                                                              "n",
                                                              "location",
                                                              "frequency",
                                                              "number",
                                                              "proportion"))

### graphs to show missing values
heat.map.full %>%
  ggplot(aes(x = factor(location),
             y = factor(`total.joined.na.narrative$Sector_desc`))) +
  geom_tile(aes(fill = proportion)) +
  scale_fill_gradient(high = "green",
                      low = "black",
                      na.value = "#ffffff") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_discrete(labels = c("Off Campus\nSex Offenses",
                              "Off Campus\nVAWA Crimes",
                              "On Campus\nSex Offenses",
                              "On Campus\nVAWA Crimes",
                              "Residential Hall\nSex Offenses",
                              "Residential Hall\nVAWA Crimes")) +
  xlab('Location and Category of Crime') +
  ylab('Type of School') +
  ggtitle(label = "Proportion of Schools with Missing Data for n Years, by Type") +
  theme(plot.title = element_text(hjust = 0.5))

#define frequency variable for input
frequency <- NULL

#create a list of schools that shows how much missing info each has
unreported.list <- total.joined %>%
  select(RAPE16,RAPE16.OFF, RAPE16.ON,
         RAPE17, RAPE17.OFF, RAPE17.ON,
         RAPE18, RAPE18.OFF, RAPE18.ON,
         STATR16, STATR16.OFF, STATR16.ON,
         STATR17, STATR17.OFF, STATR17.ON ,
         STATR18, STATR18.OFF, STATR18.ON,
         FONDL16, FONDL16.OFF, FONDL16.ON , 
         FONDL17, FONDL17.OFF, FONDL17.ON ,
         FONDL18, FONDL18.OFF, FONDL18.ON,
         DOMEST16, DOMEST16.OFF, DOMEST16.ON ,
         DOMEST17, DOMEST17.OFF, DOMEST17.ON,
         DOMEST18, DOMEST18.OFF, DOMEST18.ON,
         DATING16, DATING16.OFF, DATING16.ON,
         DATING17, DATING17.OFF, DATING17.ON,
         DATING18, DATING18.OFF, DATING18.ON,
         STALK16, STALK16.OFF, STALK16.ON,
         STALK17, STALK17.OFF, STALK17.ON,
         STALK18, STALK18.OFF, STALK18.ON)

#convert missing data to binary values
unreported.list[!is.na(unreported.list)] = 0
unreported.list[is.na(unreported.list)] = 1

#rejoin with names
unreported.list2 <- cbind(total.joined$INSTNM, 
                          total.joined$Sector_desc,
                          total.joined$Total,
                          unreported.list)

#calculate total metrics to display
unreported.list3 <- unreported.list2 %>%
  group_by(`total.joined$INSTNM`) %>%
  summarize(missing.sex.offenses16.on = sum(RAPE16.ON, STATR16.ON, FONDL16.ON),
            missing.sex.offenses17.on = sum(RAPE17.ON, STATR17.ON, FONDL17.ON),
            missing.sex.offenses18.on = sum(RAPE18.ON, STATR18.ON, FONDL18.ON),
            missing.sex.offenses16.off = sum(RAPE16.OFF, STATR16.OFF, FONDL16.OFF),
            missing.sex.offenses17.off = sum(RAPE17.OFF, STATR17.OFF, FONDL17.OFF),
            missing.sex.offenses18.off = sum(RAPE18.OFF, STATR18.OFF, FONDL18.OFF),
            missing.sex.offenses.16.res = sum(RAPE16, STATR16, FONDL16),
            missing.sex.offenses.17.res = sum(RAPE17, STATR17, FONDL17),
            missing.sex.offenses.18.res = sum(RAPE18, STATR18, FONDL18),
            missing.vawa.on16 = sum(DOMEST16.ON, STALK16.ON, DATING16.ON),
            missing.vawa.on17 = sum(DOMEST17.ON, STALK17.ON, DATING17.ON),
            missing.vawa.on18 = sum(DOMEST18.ON, STALK18.ON, DATING18.ON),
            missing.vawa.off16 = sum(DOMEST16.ON, STALK16.ON, DATING16.ON),
            missing.vawa.off17 = sum(DOMEST17.ON, STALK17.ON, DATING17.ON),
            missing.vawa.off18 = sum(DOMEST18.ON, STALK18.ON, DATING18.ON),
            missing.vawa.res16 = sum(DOMEST16, STALK16, DATING16),
            missing.vawa.res17 = sum(DOMEST17, STALK17, DATING17),
            missing.vawa.res18 = sum(DOMEST18, STALK18, DATING18))

#create lists of schools to filter by
NESCAC <- c("Middlebury College", 
            "Amherst College", 
            "Bates College", 
            "Bowdoin College", 
            "Hamilton College", 
            "Connecticut College", 
            "Tufts University",
            "Trinity College", 
            "Williams College",
            "Colby College")

IvyLeague <- c("Brown University",
               "Columbia University in the City of New York",
               "Cornell University",
               "Dartmouth College",
               "Harvard University",
               "University of Pennsylvania",
               "Princeton University",
               "Yale University")

Big10 <- c("Indiana University-Bloomington",
           "University of Maryland-College Park",
           "University of Michigan-Ann Arbor",
           "Michigan State University",
           "Ohio State University-Main Campus",
           "Pennsylvania State University-Main Campus",
           "Rutgers University-New Brunswick",
           "University of Illinois at Urbana-Champaign",
           "University of Iowa",
           "University of Minnesota-Twin Cities",
           "University of Nebraska-Lincoln",
           "Northwestern University",
           "Purdue University-Main Campus",
           "University of Wisconsin-Madison")

#lengthen unreported df for interactive table
unreported.long <- unreported.list3 %>%
  pivot_longer(-`total.joined$INSTNM`,
               names_to = "crime.details",
               values_to = "Reporting Categories Missing") %>%
  mutate(location = ifelse(str_detect(crime.details, ".on"),
                           "On Campus",
                           ifelse(str_detect(crime.details, ".res"),
                                  "Residence Hall",
                                  "Off Campus"))) %>%
  mutate(crime = ifelse(str_detect(crime.details, "sex.offenses"),
                        "Sex Offenses",
                        "VAWA")) %>%
  mutate(year = ifelse(str_detect(crime.details, "16"),
                       "2016",
                       ifelse(str_detect(crime.details, "17"),
                              "2017", "2018"))) %>%
  mutate(Institution = `total.joined$INSTNM`)
  


#calculate total missing reports by schools in table
unreported.totals <- unreported.long %>%
  group_by(Institution) %>%
  summarize(`Total Reports Missing` = sum(`Reporting Categories Missing`))


library(shiny)
library("readxl")

college.shapes <- geojson_read("Colleges_and_Universities.geojson",
                               what = "sp")
college.report.links <- read_excel("colleges.reports.xlsx")
college.report.links.edited <- college.report.links %>% mutate(capital.college = str_to_upper(NAME))
college.report.links.edited %>% anti_join(college.shapes@data, by = c("capital.college" = "NAME"))
college.shapes.copy <- college.shapes
college.shapes.copy@data <- college.shapes@data %>% left_join(college.report.links.edited, by = c("NAME" = "capital.college"))
college.shapes.copy@data <- college.shapes.copy@data %>% filter(!is.na(NAME.y))
library(leaflet)
library(tidyverse)

############################## SHINY APP ##############################

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = strong("Investigating Sexual Assault Occurrences on College Campuses"),
    tabPanel("Cover",
             h3("Math 216 Final Project", align = "center"),
             h3("Creators: Divya Gudur, Ben Yamron, Selin Everett", align = "center"),
             h4(em("We have neither given nor received unauthorized aid on this assignment"),  align = "center")),
    
    tabPanel("Overview",
             h4("Introduction"),
             h5("A key value of the CLERY Act is transparency, and an integral part of this transparency are the clear CLERY Geographies that the act lays out for reporting. CLERY geographies are specific locations at a university where a sexual assault crime occurred, separated into on-campus, off-campus, residential halls, reported to local police, and having occurred in public. While there is information on the number of cases at various geographies, different schools have different reporting requirements on which locations qualify for reporting.It is important for students to better understand which locations qualify for reporting at their school, and which don’t."),
             
             h4("How we collected our Data"),
             h5("The CLERY Act, since it was enacted in 1990, requires every university and college to report to the U.S. Department of Education their number of cases of criminal offenses each year. Therefore we chose the dataset provided by this institution to conduct our analyses and visualizations on. Although this data is the most comprehensive dataset currently available on criminal offenses on college campuses, there is no macro-level information on what locations fail to report for. We wanted to fill this unmet need and also allow parents and potential college students to better understand the prevalence of crime, specifically sexual assault crimes, at schools they were interested or planning on attending."),
                
                h4("Our Research Goals:"),
                h5("We had 3 Overarching Research Questions:"),
                h5("1. How much do sexual assault reporting statistics vary across U.S. colleges and universities?"),
                h5("2: How do different types of schools (e.g. public vs. private, NESCAC vs. Big 10) compare?"),
                h5("3: How do issues of underreporting and a lack of reporting play into the data? Are there visible trends related to reporting itself?"),
                
                h4("Limitations"),
                h5("One of the limitations was that we could not capture the complete picture with regards to underreporting assault statistics. We made the assumption that all NA values on a campus are missing even if a school was able to report for a location, when in reality, a campus might not have that location. This could be especially evident with regards to missing values for residential halls, given that some campuses don’t have residential living so would not report those numbers. For these types of schools, we would over-estimate the amount of underreporting."),
                h5("Another limitation was that trying to find the main campus institution name was difficult because we were unfamiliar with many of the 11,000 total universities in our dataset. To get around this, we subsetted the dataset into conferences that we were familar enough with to choose the correct name for its main campus (Ex: University of Maryland - College Park)")
                
             ),
    
    tabPanel("Schools' Reporting Levels",
             # selectizeInput(inputId = "reportinglevels",
             #             label = "Choose a school",
             #             choices = c(unique(metrics.long$INSTNM))),
            
             selectInput(inputId = "numberofyears",
                         label = "Number of years of missing data:",
                         choices = c("1", "2", "3")),
             plotOutput("heatmap"),
             flowLayout(selectInput(inputId = "conference.picker",
                         label = "Choose a Conference",
                         choices = c("NESCAC", "IvyLeague", "Big10"),
                         multiple = F),
                        selectInput(inputId = "crime.picker",
                                    label = "Choose an Offense",
                                    choices = c(unique(unreported.long$crime))),
                        selectInput(inputId = "location.picker",
                                    label = "Choose a Location",
                                    choices = c(unique(unreported.long$location))),
                        selectInput(inputId = "year.picker",
                                    label = "Choose a Year",
                                    choices = c(unique(unreported.long$year)))),
             splitLayout(tableOutput("conference.report"),
                        tableOutput("conference.totals"))
             # selectInput(inputId = "loc2",
             #             label = "CLERY location:",
             #             choices = c(unique(total.long$location))),
             # selectizeInput(inputId = "institutiontype",
             #                label = "Pick an institution type",
             #                choices = c("Public, 4-year or above",
             #                            "Private not-for-profit, 4-year",
             #                            "Private for-profit, 4-year",
             #                            "Public, 2-year",
             #                            "Private not-for-profit, 2-year",
             #                            "Private for-profit, 2-year",
             #                            "Public, less-than 2-year"))),
    ),
             
             tabPanel("Comparing Schools' Sexual Assault Rates",
                      flowLayout(selectizeInput(inputId = "school1",
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
                                             choices = c(unique(total.long$crime)))), 
                      plotOutput("crimes_over_time"),
                      plotOutput("comparison_rates")
             ),
    
    tabPanel("CLERY Violation Reports since 2008",
                          p("CLERY violations can occur on college campuses for a variety of reasons.
                            Most often, reviews are done on colleges when a complaint is received,
                            when there is some event reported by the media, when an independent party does a review, or
                            through a review selection processs. Below are mapped all the colleges that have had reports since 
                            2008. There are three colleges, Arlington Medical Institute, Plaza Beauty School, and Wards Corner Beauty Academy
                            that do not have locations here but have had CLERY reports. If you click on each school, you can connect to a link
                            with the US Department of Education's review determination or fine letter. Schools that have multiple reports will be shown, but 
                            schools that do not have multiple reports are displayed as NA values. This data was taken from the Federal Student Aid government website."),
                          leafletOutput(outputId = "leaflet")))
    

# Define server logic
server <- function(input, output) {
  #### Reporting (page2) graph1
  output$heatmap <- renderPlot(
  heat.map.full %>%
    filter(frequency == input$numberofyears) %>%
    ggplot(aes(x = factor(location),
               y = factor(`total.joined.na.narrative$Sector_desc`))) +
    geom_tile(aes(fill = proportion)) +
    scale_fill_gradient(high = "green",
                        low = "black",
                        na.value = "#ffffff") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0)) +
    scale_x_discrete(labels = c("Off Campus\nSex Offenses",
                                "Off Campus\nVAWA Crimes",
                                "On Campus\nSex Offenses",
                                "On Campus\nVAWA Crimes",
                                "Residential Hall\nSex Offenses",
                                "Residential Hall\nVAWA Crimes")) +
    xlab('Location and Category of Crime') +
    ylab('Type of School') +
    ggtitle(label = paste("Proportion of Schools with Missing Data for",input$numberofyears, "Years, by Type")) +
    theme(plot.title = element_text(hjust = 0.5))
  )
  
  #### Reporting (page2) table1 ####
  output$conference.report <- renderTable(
  unreported.long %>%
    filter(`Institution` %in% get(input$conference.picker)) %>%
    filter(crime == input$crime.picker) %>%
    filter(location == input$location.picker) %>%
    filter(year == input$year.picker) %>%
    select(Institution, `Reporting Categories Missing`)
  )
  
  #### Reporting (page2) table2 ####
  output$conference.totals <- renderTable(
  unreported.totals %>%
    filter(Institution %in% get(input$conference.picker))
  )
  
  #### Comparison (page3) graph1 ####
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
  #### Comparison (page3) graph2 #### 
  output$comparison_rates <- renderPlot(
    metrics.long %>%
      filter(INSTNM %in% c(input$school1, input$school2)) %>%
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
      scale_fill_manual(name = "Crime", values = c("dodgerblue3", "orange", "plum1", "olivedrab3"))
    # + coord_flip()
  )
  
  output$leaflet <- renderLeaflet(
    college.shapes.copy %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(lat = ~LATITUDE, 
                 lng = ~LONGITUDE,
                 label = ~NAME,
                 popup =  ~paste("Year:",Year, "<br>", "<a href =\"",Hyperlink,"\", target=\"_blank\">",`Hyperlink name`,"</a>",
                                 "<br>", "Year:",Year_2, "<br>", "<a href =\"",Hyperlink_2,"\", target=\"_blank\">",`Hyperlink_name_2`,"</a>"),
                 clusterOptions = markerClusterOptions())
  )
}

shinyApp(ui = ui, server = server)
