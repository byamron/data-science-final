

library(tidyverse)
library(rvest)
library(pdftools)
library(tm)
library(stringr)
library(tidytext)
library(tidyr)
library(reshape2)
library(lubridate)
library(geojsonio)
library(leaflet)
library(sf)
library(sp)

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

total.joined.replaced.na %>%
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
            women_total = mean(women_total)) %>%
  view()


#new code 5/20/21 ###

#reading in shape file 
colleges.points.shape <- st_read("Colleges_and_Universities.shp")
colleges.transform <- st_transform(colleges.points.shape, CRS("+proj=longlat +datum=WGS84 +no_defs"))

colleges.transform %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

#Trying to graph a leaflet is soooo slow, so we are going to try to use ggplot
ggplot() + 
  geom_sf(data = colleges.points.shape, size = 1, color = "black", fill = "cyan1") + 
  coord_sf()


#read in geojson file
college.shapes <- geojson_read("Colleges_and_Universities.geojson",
             what = "sp")
link <- "https://studentaid.gov/data-center/school/clery-act-reports"
link %>%
  read_html() %>%
  html_nodes("h3") %>%
  html_text() %>%
  view()

#### RSelenium ###
#Download java development kit
install.packages("RSelenium")
library(RSelenium)
#set up remote server using remoteServer function
#call it r
test <- rsDriver(port = 4444L,
                 browser = "firefox")

r <- remoteDriver(browserName = "firefox",
                  port = 4444L)
r$open()
r$navigate(link)
# test.h3 <- r$findElements(using = "tag name","h3")
# 
# test.h3.2 <- test.h3$getElements()
# 
source <- r$getPageSource()

source.h3 <- source %>%
  unlist() %>%
  read_html() %>%
  html_nodes("h3") %>%
  html_text() %>% as.data.frame()

write_csv(source.h3, "report.names.csv")

### programming the leaflet ### 
###this is basically all the information we need for making our leaflet###
library("readxl")
college.report.links <- read_excel("colleges.reports.xlsx")
college.report.links.edited <- college.report.links %>% mutate(capital.college = str_to_upper(NAME))

###check what schools don't have coordinates
college.report.links.edited %>% anti_join(college.shapes@data, by = c("capital.college" = "NAME"))


college.shapes.copy <- college.shapes
college.shapes.copy@data <- college.shapes@data %>% left_join(college.report.links.edited, by = c("NAME" = "capital.college"))
college.shapes.copy@data <- college.shapes.copy@data %>% filter(!is.na(NAME.y))
library(leaflet)
library(tidyverse)
college.shapes.copy %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = ~LATITUDE, 
             lng = ~LONGITUDE,
             label = ~NAME,
             popup = ~paste("Year:",Year, "<br>",
                            "<a href =\"",Hyperlink,"\", target=\"_blank\">",`Hyperlink name`,"</a>"),
             clusterOptions = markerClusterOptions())
  
