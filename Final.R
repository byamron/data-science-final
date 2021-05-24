

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

#new code 5/20/21

#reading in shape file 
#colleges.points.shape <- st_read("Colleges_and_Universities.shp")
#colleges.transform <- st_transform(colleges.points.shape, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#colleges.transform %>%
#  leaflet() %>%
#  addTiles() %>%
#  addMarkers()

#Trying to graph a leaflet is soooo slow, so we are going to try to use ggplot
#ggplot() + 
#  geom_sf(data = colleges.points.shape, size = 1, color = "black", fill = "cyan1") + 
#  coord_sf()


#read in geojson file
#college.shapes <- geojson_read("Colleges_and_Universities.geojson",
#             what = "sp")

#colleges.shapes.copy <- college.shapes
#colleges.shapes.copy@data <- college.shapes@data %>% filter(NAME == "MIDDLEBURY COLLEGE")
#
#
#colleges.shapes.copy %>%
#  leaflet() %>%
#  addTiles() %>%
#  addMarkers()


#college.shapes %>%
#  leaflet() %>%
#  addTiles() %>%
#  addMarkers()

#rape vs. pop
metrics.full %>%
  ggplot(aes(x = Total.pop,
             y = rate.rape,
             color = Sector_desc)) +
  geom_point() +
  theme_bw()

#statr vs. pop
metrics.full %>%
  ggplot(aes(x = Total.pop,
             y = rate.statr,
             color = Sector_desc)) +
  geom_point() +
  theme_bw()

#fondl vs. pop
metrics.full %>%
  ggplot(aes(x = Total.pop,
             y = rate.fondl,
             color = Sector_desc)) +
  geom_point() +
  theme_bw()

#vawa vs. pop
metrics.full %>%
  ggplot(aes(x = Total.pop,
             y = rate.vawa,
             color = Sector_desc)) +
  geom_point() +
  theme_bw()

#public vs private
metrics.full %>%
  filter(!(INSTNM == "Christian Life College")) %>%
  group_by(Sector_desc) %>%
  summarize(agg_rape_rate = mean(rate.rape)) %>%
  ggplot(aes(x = Sector_desc,
             y = agg_rape_rate,
             fill = Sector_desc)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual(values = c("darkred", "blue")) +
  theme_bw()

#### Divya's graph

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


#filter(Sector_desc %in% c("Public, 4-year or above", "Private nonprofit, 4-year or above")) %>%
#filter(!(is.na(State)))

#rename columns based on original data sets
colnames(total.joined.na.narrative) <- str_replace_all(colnames(total.joined.na.narrative),"x", "OFF")
colnames(total.joined.na.narrative) <- str_replace_all(colnames(total.joined.na.narrative),"y", "ON")

total.na.edited <- total.joined.na.narrative %>% select(RAPE16.ON, RAPE17.ON, RAPE18.ON,
                                                        RAPE16.OFF, RAPE17.OFF, RAPE18.OFF,
                                                        RAPE16, RAPE17, RAPE18, 
                                                        DOMEST16.OFF, DOMEST17.OFF, DOMEST18.OFF,
                                                        DOMEST16.ON, DOMEST17.ON, DOMEST18.ON,
                                                        DOMEST16, DOMEST17, DOMEST18) 
total.na.edited[!is.na(total.na.edited)] = 0
total.na.edited[is.na(total.na.edited)] = 1

total.na.scores <- total.na.edited %>% cbind(total.joined.na.narrative$Sector_desc) %>%
  mutate(on.campus.sex.offenses = RAPE16.ON + RAPE17.ON + RAPE18.ON,
         off.campus.sex.offenses = RAPE16.OFF + RAPE17.OFF + RAPE18.OFF,
         residential.hall.sex.offensies = RAPE16 + RAPE17 + RAPE18,
         on.campus.VAWA.crimes = DOMEST16.ON + DOMEST17.ON + DOMEST18.ON,
         off.campus.VAWA.crimes = DOMEST16.OFF, DOMEST17.OFF, DOMEST18.OFF,
         residential.hall.VAWA.crimes = DOMEST16, DOMEST17, DOMEST18) %>%
  select(19:25)
total.na.scores.long <- total.na.scores %>% pivot_longer(-`total.joined.na.narrative$Sector_desc`,
                                                         names_to = "location",
                                                         values_to = "frequency")

heat.map.na.values <- total.na.scores.long %>% count(`total.joined.na.narrative$Sector_desc`, location, frequency)  %>%
  filter(frequency == 1) 


### graphs to show missing values
heat.map.na.values %>%
  ggplot(aes(x = factor(location),
             y = factor(`total.joined.na.narrative$Sector_desc`))) +
  geom_tile(aes(fill = n)) +
  scale_fill_gradient(high = "green",
                      low = "black")
 
