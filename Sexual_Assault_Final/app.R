#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



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
    filter(Sectr_desc != "Administrative Unit Only")


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
%>%
  ggplot(aes(x = factor(location),
             y = factor(`total.joined.na.narrative$Sector_desc`))) +
  geom_tile(aes(fill = n)) +
  scale_fill_gradient(high = "green",
                      low = "black")
  

  
  


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
