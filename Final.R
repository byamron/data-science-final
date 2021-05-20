#this is a test commit
#hi ben
#hi ben
#hi ben

#whatsup

install.packages("rgdal")
install.packages("spdplyr")
install.packages("geojsonio")
install.packages("rmapshaper")

library(tidyverse)
library(rvest)
library(pdftools)
library(tm)
library(stringr)
library(tidytext)
library(tidyr)
library(reshape2)
library(lubridate)


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

frames.list <- list(noncampus.crime,
                    noncampus.vawa, 
                    oncampus.crime, 
                    oncampus.vawa,
                    reshall.crime,
                    reshall.vawa)

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
                                "Total")) 
   

colnames(total.joined) <- str_replace_all(colnames(total.joined),"x", "OFF")
colnames(total.joined) <- str_replace_all(colnames(total.joined),"y", "ON")

##later: can rename res hall columns but for now have no suffix 
#colnames(total.joined) <- str_c(colnames(total.joined), c("6 ", "7 ", "8 "), ".RES")


#this is the code for refined state and public colleges list 
state.schools <- read_csv("colleges.edit copy.csv")
names(state.schools)[1] <- "INSTNM"


nescacs <- tibble(school = c("Middlebury College", 
                             "Amherst College", 
                             "Bates College", 
                             "Bowdoin College", 
                             "Hamilton College", 
                             "Connecticut College", 
                             "Tufts University",
                             "Trinity College", 
                             "Williams College",
                             "Colby College"))

names(nescacs)[1] <- "INSTNM"

#schools that're missing
state.schools.total.not.joined <- state.schools %>%
  anti_join(total.joined, by = "INSTNM") 

#most of the state schools 
state.schools.total.joined <- total.joined %>%
  inner_join(state.schools, by = "INSTNM") 
  
nescacs.data <- total.joined %>%
  inner_join(nescacs, by = "INSTNM")

nescac.and.state.data <- state.schools.total.joined %>%
  full_join(nescacs.data, by = c("UNITID_P",
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


top.200.url <- "https://www.4icu.org/top-universities-north-america/"
top.200.colleges <- top.200.url %>%
  read_html() %>%
  html_node("table") %>%
  html_table()

#see what schools this leaves out from the top 200
top200.leftout <- top.200.colleges %>%
  anti_join(total.joined, by = c("University" = "INSTNM")) 
#84 schools from this top 200 list have different syntax than the OG list


top200.included <- total.joined %>%
  inner_join(top.200.colleges, by = c("INSTNM" = "University"))



