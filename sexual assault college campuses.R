library(tidyverse)

oncampus.crime <- read_csv("oncampus_criminal_offenses.csv")

filtered.oncampus.crime <- oncampus.crime %>%
  mutate(total_rape = (RAPE16 + RAPE17 + RAPE18)) %>%
  mutate(total_fond = (FONDL16 + FONDL17 + FONDL18)) %>%
  mutate(total_stat_rape = (STATR16 + STATR17 + STATR18)) %>%
  select(1:3, 
         men_total, 
         women_total, 
         total_rape,
         total_fond, 
         total_stat_rape, 
         RAPE16, 
         RAPE17, 
         RAPE18, 
         FONDL16, 
         FONDL17, 
         FONDL18, 
         STATR16, 
         STATR17, 
         STATR18) %>%
  filter(!(is.na(RAPE17 | RAPE16 | RAPE18))) %>%
  filter(!(is.na(FONDL16 | FONDL17 | FONDL18))) %>%
  filter(!(is.na(STATR16 | STATR17 | STATR18))) 

#normalize values for rape, fondling, statutory rape based on school's population 
#3 new columns after subbing mean and dividing by st dev
#only want one total value for each of the 3 categories for all campuses at one school 


grouped.and.filtered.oncampus.crime <- filtered.oncampus.crime %>%
  mutate(total_population = men_total + women_total) %>%
  group_by(INSTNM) %>%
  summarize(men = mean(men_total),
            women = mean(women_total),
            total_population = mean(total_population), 
            total_rape = sum(total_rape),
            total_fond = sum(total_fond),
            total_stat_rape = sum(total_stat_rape))


#5820 schools

#want the number of offenses per thousand students 
add.rates <- grouped.and.filtered.oncampus.crime %>%
  group_by(INSTNM) %>%
  summarize(rape_rate = (total_rape/total_population)*100,
            fond_rate = (total_fond/total_population)*100,
            stat_rape_rate = (total_stat_rape/total_population)*100)
  
  

#only want the state schools and NESCACs




  
oncampus.vaca <- read_csv("offcampus_vawa.csv")

offcampus.crime <- read_csv("offcampus_crime.csv")

filtered.offcampus.crime <- offcampus.crime %>%
  mutate(total_rape = (RAPE16 + RAPE17 +RAPE18)) %>%
  mutate(total_fond = (FONDL16 + FONDL17 + FONDL18)) %>%
  mutate(total_stat_rape = (STATR16 + STATR17 + STATR18)) %>%
  select(1:3, 
         men_total, 
         women_total, 
         total_rape,
         total_fond, 
         total_stat_rape, 
         RAPE16, 
         RAPE17, 
         RAPE18, 
         FONDL16, 
         FONDL17, 
         FONDL18, 
         STATR16, 
         STATR17, 
         STATR18) %>%
  filter(!(is.na(RAPE17 | RAPE16 | RAPE18))) %>%
  filter(!(is.na(FONDL16 | FONDL17 | FONDL18))) %>%
  filter(!(is.na(STATR16 | STATR17 | STATR18))) 



offcampus.vawa <- read_csv("offcampus_vawa.csv")

reshall.crime <- read_csv("residencehall_crime.csv")
reshall.vawa <- read_csv("residencehall_vawa.csv")

