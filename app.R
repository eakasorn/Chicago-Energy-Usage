library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(DT)
library(mapview)
library(tigris)
library(leaflet)
library(readxl)

energy <- read.csv("modified-energy-usage-2010.csv")[-c(34:63)]
energy_nws <- subset(energy, COMMUNITY.AREA.NAME=='Near West Side')

community_area <- unique(as.factor(energy$COMMUNITY.AREA.NAME))

il_block_cook <- blocks(state='IL', county='Cook')
chicago_blocks <- subset(il_block_cook, GEOID10 %in% energy$CENSUS.BLOCK)

nws <- subset(chicago_blocks, GEOID10 %in% energy_nws$CENSUS.BLOCK)
join_nws_energy <- merge(x=nws, y=energy_nws, by.x='GEOID10', by.y='CENSUS.BLOCK')

filter_by <- c("Gas","Electricity","Building Age","Building Type","Building Height","Total Population")
month <- c("January","February","March","April","May","June","July","August","September","October","November","December","All")
building_type <- c("Residential","Commercial","Industrial", "All")

e_month <- c("KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010", "KWH.MAY.2010", "KWH.JUNE.2010",
               "KWH.JULY.2010", "KWH.AUGUST.2010", "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010", "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010", "TOTAL.KWH")
g_month <- c("THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "TERM.APRIL.2010", "THERM.MAY.2010", "THERM.JUNE.2010",
               "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010", "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010", "TOTAL.THERMS")
e_month.y <- c("KWH.JANUARY.2010.y", "KWH.FEBRUARY.2010.y", "KWH.MARCH.2010.y", "KWH.APRIL.2010.y", "KWH.MAY.2010.y", "KWH.JUNE.2010.y",
             "KWH.JULY.2010.y", "KWH.AUGUST.2010.y", "KWH.SEPTEMBER.2010.y", "KWH.OCTOBER.2010.y", "KWH.NOVEMBER.2010.y", "KWH.DECEMBER.2010.y", "TOTAL.KWH.y")
g_month.y <- c("THERM.JANUARY.2010.y", "THERM.FEBRUARY.2010.y", "THERM.MARCH.2010.y", "TERM.APRIL.2010.y", "THERM.MAY.2010.y", "THERM.JUNE.2010.y",
             "THERM.JULY.2010.y", "THERM.AUGUST.2010.y", "THERM.SEPTEMBER.2010.y", "THERM.OCTOBER.2010.y", "THERM.NOVEMBER.2010.y", "THERM.DECEMBER.2010.y", "TOTAL.THERMS.y")

energy_tracts <- energy
energy_tracts$TRACTS <- substr(energy$CENSUS.BLOCK,6,11)
chicago_tracts <- subset(il_block_cook, TRACTCE10 %in% energy_tracts$TRACTS)
join_energy_tracts <- merge(x=chicago_tracts, y=energy_tracts, by.x='TRACTCE10', by.y='TRACTS')


top10 <- c("Oldest Buildings", "Newest Buildings", "Tallest Buildings", "Most Electricity", "Most Gas", "Most Population", "Most Occupied Percentage", "Highest Percentage Renters")

t.agg <- aggregate(cbind(THERM.JANUARY.2010,THERM.FEBRUARY.2010,THERM.MARCH.2010,TERM.APRIL.2010,THERM.MAY.2010,THERM.JUNE.2010,
                         THERM.JULY.2010,THERM.AUGUST.2010,THERM.SEPTEMBER.2010,THERM.OCTOBER.2010,THERM.NOVEMBER.2010,THERM.DECEMBER.2010,TOTAL.THERMS,
                         KWH.JANUARY.2010,KWH.FEBRUARY.2010,KWH.MARCH.2010,KWH.APRIL.2010,KWH.MAY.2010,KWH.JUNE.2010,
                         KWH.JULY.2010,KWH.AUGUST.2010,KWH.SEPTEMBER.2010,KWH.OCTOBER.2010,KWH.NOVEMBER.2010,KWH.DECEMBER.2010,TOTAL.KWH,
                         AVERAGE.BUILDING.AGE,AVERAGE.STORIES,TOTAL.POPULATION,OCCUPIED.UNITS.PERCENTAGE,RENTER.OCCUPIED.HOUSING.PERCENTAGE)
                   ~TRACTCE10, FUN = mean, data=join_energy_tracts)
t.geo <- join_energy_tracts[!duplicated(join_energy_tracts$TRACTCE10),][,c(1,61)]
t.merge <- merge(x=t.geo, t.agg, by='TRACTCE10')

t.building.age <- t.merge[28]
t.building.height <- t.merge[29]
t.electricity <- t.merge[15:27]
t.gas <- t.merge[2:14]
t.population <- t.merge[30]
t.occupied <- t.merge[31]
t.highest.renters <- t.merge[32]
aoc_attr <- names(t.merge[c(28,29,27,14,30,31,32)])


# Census Data
energy_area <- energy[1:2]

census <- read_excel("CCASF12010CMAP.xlsx",skip=1)
census.m.18 <- census[,c(1,12:15)]
census.f.18 <- census[,c(1,35:38)]
census.m.18$Under18M <- census.m.18[2] + census.m.18[3] + census.m.18[4] + census.m.18[5]
census.m.18$Under18M <- census.m.18$Under18M$`Male: Under 5 years old`
census.f.18$Under18F <- census.f.18[2] + census.f.18[3] + census.f.18[4] + census.f.18[5]
census.f.18$Under18F <- census.f.18$Under18F$`Female: Under 5 years old`
gen <- merge(x=energy_area, y=census.m.18, by.x = 'COMMUNITY.AREA.NAME', by. = 'Geog')
gen <- merge(x=gen, y=census.f.18, by.x = 'COMMUNITY.AREA.NAME', by. = 'Geog')
gen <- gen[!duplicated(gen$CENSUS.BLOCK),]
gen <- merge(x=chicago_blocks, y=gen, by.x = 'GEOID10', by.y = 'CENSUS.BLOCK')
gen.sim <- gen[,c(23,28,29)]


# Shiny Interface
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 3"),
  
  # Sidebar start
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("Near West Side", tabName = "nws"),
                     menuItem("Area Comparison", tabName = "comparison"),
                     menuItem("All of Chicago", tabName = "all"),
                     menuItem("Data Integration", tabName = "integration"),
                     menuItem("About", tabName = "about")
                   )
  ),
  # Sidebar end
  
  # Body start
  dashboardBody(
    tabItems(
      # "About" Tab
      tabItem(tabName = "about",
              h2("About"),
              p("This project focuses on the visualization of data using the electricity and gas usage in Chicago dataset which was collected in 2010.
                Important information such as the community area, census block, building type, electricity and gas usage are recorded.
                By mapping the data according to the census block, it is possible to see how neighborhoods, types of buildings, age of buildings can affect the power usage throughout the year.
                The original data is available from",
                a("https://www.kaggle.com/chicago/chicago-energy-usage-2010"),
                "as well as ",
                a("https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp"),
                ". Another source of information about the demographic of the residents living in the city can be found at ",
                a("https://datahub.cmap.illinois.gov/dataset/5700ba1a-b173-4391-a26e-48b198e830c8/resource/b30b47bf-bb0d-46b6-853b-47270fb7f626/download/CCASF12010CMAP.xlsx"),
                ".",
                style = "font-family : sans-serif"),
              p("The app is written by Eakasorn Suraminitkul and uses the data from 2010.",
                style = "font-family : sans-serif"),
      ),
      
      # "Visualization" Tab
      tabItem(tabName = "nws",
              h2("Near West Side (2010)"),
              
              # Map
              column(4,
                fluidRow(
                  column(3,
                         selectInput("fb", "Filter by",       
                                     filter_by, selected="Electricity")
                  ),
                  column(3,
                         selectInput("mth", "Month",       
                                     month, selected="All")
                  ),
                  column(3,
                         selectInput("bt", "Building Type",       
                                     building_type, selected="All")
                  ),
                  column(1,
                         actionButton("reset", "Reset")
                  )
                ),
                fluidRow(
                  box(title = "Near West Side Map", solidHeader = TRUE, status = "primary", width = 12,
                      leafletOutput("map", height = 550)
                  )
                ),
                fluidRow(
                  textOutput("t")
                )
              ),

              # Electricity
              column(8,
                column(6,
                  fluidRow(
                    box(title = "Electricity Usage by Census Block", solidHeader = TRUE, status = "primary", width = 12,
                        plotOutput("eu_graph", height = 375)
                    )
                  )
                ),
                
                # Gas
                column(6,
                  fluidRow(
                    box(title = "Gas Usage by Census Block", solidHeader = TRUE, status = "primary", width = 12,
                        plotOutput("gu_graph", height = 375)
                    )
                  )
                ),
                
                # Table
                fluidRow(
                  box(title = "Table", solidHeader = TRUE, status = "primary", width = 12,
                      DT::dataTableOutput("table"),style = "height:350px; overflow-y: scroll;overflow-x: scroll;"
                  )
                )
              )
              
      ),
      
      # "Area Comparison" Tab
      tabItem(tabName = "comparison",
              h2("Comparison between community areas"),
              
              # Left map
              column(6,
                     fluidRow(
                       column(3,
                              selectInput("cal", "Community Area",       
                                          community_area, selected="Near West Side")
                       ),
                       column(3,
                              selectInput("fbl", "Filter by",       
                                          filter_by, selected="Electricity")
                       ),
                       column(2,
                              selectInput("mthl", "Month",       
                                          month, selected="All")
                       ),
                       column(2,
                              selectInput("btl", "Building Type",       
                                          building_type, selected="All")
                       ),
                       column(1,
                              selectInput("colorl", "Color",       
                                          c(1:3), selected=2)
                       ),
                       column(1,
                              actionButton("resetl", "Reset")
                       )
                     ),
                     fluidRow(
                       box(title = "Area 1", solidHeader = TRUE, status = "primary", width = 12,
                           leafletOutput("mapl", height = 550)
                       )
                     )
              ),
              
              # Right map
              column(6,
                     fluidRow(
                       column(3,
                              selectInput("car", "Community Area",       
                                          community_area, selected="Loop")
                       ),
                       column(3,
                              selectInput("fbr", "Filter by",       
                                          filter_by, selected="Electricity")
                       ),
                       column(2,
                              selectInput("mthr", "Month",       
                                          month, selected="All")
                       ),
                       column(2,
                              selectInput("btr", "Building Type",       
                                          building_type, selected="All")
                       ),
                       column(1,
                              selectInput("colorr", "Color",       
                                          c(1:3), selected=2)
                       ),
                       column(1,
                              actionButton("resetr", "Reset")
                       )
                     ),
                     fluidRow(
                       box(title = "Area 2", solidHeader = TRUE, status = "primary", width = 12,
                           leafletOutput("mapr", height = 550)
                       )
                     )
              )
      ),
      
      # "All" Tab
      tabItem(tabName = "all",
              h2("City of Chicago"),
              
              # Map
              fluidRow(
                column(3,
                       selectInput("top", "Top 10%",
                                   top10, selected="Most Electricity")
                ),
                column(1,
                      actionButton("resetaoc", "Reset")
                )
              ),
              
              fluidRow(
                 box(title = "Chicago Map", solidHeader = TRUE, status = "primary", width = 12,
                     leafletOutput("mapaoc", height = 550)
                 )
              )
      ),
      
      # "Integration" Tab
      tabItem(tabName = "integration",
              h2("Data Integration"),
              
              # Map
              fluidRow(
                column(1,
                       selectInput("gender", "Gender",
                                   c("Male","Female"), selected="Male")
                ),
                column(1,
                       actionButton("resetg", "Reset")
                )
              ),
              
              fluidRow(
                box(title = "Chicago Map", solidHeader = TRUE, status = "primary", width = 12,
                    leafletOutput("mapg", height = 550)
                )
              )
              
              
              
      )
      
    )
  )
)


server <- function(input, output) {
  # Near West Side tab
  re_filter <- reactive(switch(input$fb,
                                "Gas" = subset(join_nws_energy, is.na(join_nws_energy$TOTAL.THERMS)==FALSE),
                                "Electricity" = subset(join_nws_energy, is.na(join_nws_energy$TOTAL.KWH)==FALSE),
                                "Building Age" = join_nws_energy,
                                "Building Type" = join_nws_energy,
                                "Building Height" = join_nws_energy,
                                "Total Population" = subset(join_nws_energy, is.na(join_nws_energy$TOTAL.POPULATION)==FALSE)
                               ))
  
  re_month <- reactive(
    if (input$mth == 'All') {
      switch(input$fb,
              "Gas" = re_filter()[c(1:20,36:60)],
              "Electricity" = re_filter()[c(1:35,50:60)],
              "Building Age" = re_filter()[c(1:20,50:60)],
              "Building Type" = re_filter()[c(1:20,50:60)],
              "Building Height" = re_filter()[c(1:20,50:60)],
              "Total Population" = re_filter()[c(1:20,50:60)])
      }
    else {
      temp <- re_filter()[c(1:20, 20+match(input$mth, month.name), 34:35, 35+match(input$mth, month.name), 49:60)]
      switch(input$fb,
              "Gas" = temp[c(1:20,24,25:36)],
              "Electricity" = temp[c(1:20,21:23,26:36)],
              "Building Age" = temp[c(1:20,26:36)],
              "Building Type" = temp[c(1:20,26:36)],
              "Building Height" = temp[c(1:20,26:36)],
              "Total Population" = temp[c(1:20,26:36)])
    }
  )
  
  attr <- reactive(
    switch(input$fb,
           "Gas" = switch(input$mth,
                          "January"   = g_month.y[1],
                          "February"  = g_month.y[2],
                          "March"     = g_month.y[3],
                          "April"     = g_month.y[4],
                          "May"       = g_month.y[5],
                          "June"      = g_month.y[6],
                          "July"      = g_month.y[7],
                          "August"    = g_month.y[8],
                          "September" = g_month.y[9],
                          "October"   = g_month.y[10],
                          "November"  = g_month.y[11],
                          "December"  = g_month.y[12],
                          "All"       = g_month.y[13]  ),
           "Electricity" = switch(input$mth,
                                  "January"   = e_month.y[1],
                                  "February"  = e_month.y[2],
                                  "March"     = e_month.y[3],
                                  "April"     = e_month.y[4],
                                  "May"       = e_month.y[5],
                                  "June"      = e_month.y[6],
                                  "July"      = e_month.y[7],
                                  "August"    = e_month.y[8],
                                  "September" = e_month.y[9],
                                  "October"   = e_month.y[10],
                                  "November"  = e_month.y[11],
                                  "December"  = e_month.y[12],
                                  "All"       = e_month.y[13] ),
           "Building Age" = "AVERAGE.BUILDING.AGE.y",
           "Building Type" = "BUILDING.TYPE",
           "Building Height" = "AVERAGE.STORIES.y",
           "Total Population" = "TOTAL.POPULATION"
    )
  )
  
  # For 'All' months
  re_pre_plot <- reactive(
    switch(input$fb,
           "Gas" =  merge(x=re_month(), y=aggregate(cbind(THERM.JANUARY.2010,THERM.FEBRUARY.2010,THERM.MARCH.2010,TERM.APRIL.2010,THERM.MAY.2010,THERM.JUNE.2010,
                                                          THERM.JULY.2010,THERM.AUGUST.2010,THERM.SEPTEMBER.2010,THERM.OCTOBER.2010,THERM.NOVEMBER.2010,THERM.DECEMBER.2010,TOTAL.THERMS)
                                                    ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
           "Electricity" = merge(x=re_month(), y=aggregate(cbind(KWH.JANUARY.2010,KWH.FEBRUARY.2010,KWH.MARCH.2010,KWH.APRIL.2010,KWH.MAY.2010,KWH.JUNE.2010,
                                                                 KWH.JULY.2010,KWH.AUGUST.2010,KWH.SEPTEMBER.2010,KWH.OCTOBER.2010,KWH.NOVEMBER.2010,KWH.DECEMBER.2010,TOTAL.KWH)
                                                           ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
           "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
           "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
           "Building Type" = re_month(),
           "Total Population" = re_month()
    
    )
  )
  
  re_plot <- reactive(
    if (input$mth=='All') {
      re_pre_plot()
    }
    else {
      switch(input$mth,
           "January" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.JANUARY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.JANUARY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "February" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.FEBRUARY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.FEBRUARY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "March" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.MARCH.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.MARCH.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "April" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(TERM.APRIL.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.APRIL.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "May" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.MAY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.MAY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "June" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.JUNE.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.JUNE.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "July" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.JULY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.JULY.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "August" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.AUGUST.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.AUGUST.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "September" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.SEPTEMBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.SEPTEMBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "October" = switch(input$fb,
                              "Gas" =  merge(x=re_month(), y=aggregate(THERM.OCTOBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Electricity" = merge(x=re_month(), y=aggregate(KWH.OCTOBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                              "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                              "Building Type" = re_month(),
                              "Total Population" = re_month()),
           "November" = switch(input$fb,
                               "Gas" =  merge(x=re_month(), y=aggregate(THERM.NOVEMBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                               "Electricity" = merge(x=re_month(), y=aggregate(KWH.NOVEMBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                               "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                               "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                               "Building Type" = re_month(),
                               "Total Population" = re_month()),
           "December" = switch(input$fb,
                               "Gas" =  merge(x=re_month(), y=aggregate(THERM.DECEMBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                               "Electricity" = merge(x=re_month(), y=aggregate(KWH.DECEMBER.2010 ~ GEOID10, FUN = sum, data=re_month()), by='GEOID10'),
                               "Building Age" = merge(x=re_month(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                               "Building Height" = merge(x=re_month(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month()), by='GEOID10'),
                               "Building Type" = re_month(),
                               "Total Population" = re_month()
           )
           
      )
    }
  )
  
  re_plot_1 <- reactive(switch(input$bt,
                                "Residential" = subset(re_plot(), BUILDING.TYPE=='Residential'),
                                "Commercial" = subset(re_plot(), BUILDING.TYPE=='Commercial'),
                                "Industrial" = subset(re_plot(), BUILDING.TYPE=='Industrial'),
                                "All" = re_plot()
                              )  
  )
  
  
  mapviewOptions(basemaps = "CartoDB.Positron",
                 vector.palette = colorRampPalette(c("pink","red","grey10")))
  
  output$map <- renderLeaflet(
    # reset button
    if(input$reset) {
      mapview(re_plot_1(), zcol=attr(), alpha.regions=0.25, alpha=0.1, layer.name=attr())@map
    }
    else{
      mapview(re_plot_1(), zcol=attr(), alpha.regions=0.25, alpha=0.1, layer.name=attr())@map
    }
  )
  
  
  output$table <- renderDataTable(
    re_plot_1()
  )
  
  e_geoid <- reactive({
    temp <- re_plot_1()[1]
    temp$geometry <- NULL
    temp
  })
  e_geom <- reactive(re_plot_1()$geometry)
  e_table <- reactive(
    if(input$fb=='Electricity'){
      temp <- re_plot_1()[21:32]
      temp$geometry <- NULL
      temp <- melt(temp)
      temp$geoid <- e_geoid()$GEOID10[1:931]
      temp
    }
  )
  
  output$eu_graph <- renderPlot(
    ggplot(e_table(), aes(x=variable, y=value, colour=factor(geoid))) +
      stat_summary(aes(y = value, group=factor(geoid)), geom="line", show.legend = FALSE) +
      labs(x="Month", y="Amount (Kilowatthours)") +
      theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
  )
  
  g_geoid <- reactive({
    temp <- re_plot_1()[1]
    temp$geometry <- NULL
    temp
  })
  g_geom <- reactive(re_plot_1()$geometry)
  g_table <- reactive(
    if(input$fb=='Gas'){
      temp <- re_plot_1()[21:32]
      temp$geometry <- NULL
      temp <- melt(temp)
      temp$geoid <- g_geoid()$GEOID10[1:865]
      temp
    }
  )
  
  output$gu_graph <- renderPlot(
    ggplot(g_table(), aes(x=variable, y=value, colour=factor(geoid))) +
      stat_summary(aes(y = value, group=factor(geoid)), geom="line", show.legend = FALSE) +
      labs(x="Month", y="Amount (Therms)") +
      theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
  )
  
  
  ##############################################################################
  # Area Comparison tab
  energy_l <- reactive(subset(energy, COMMUNITY.AREA.NAME==renderText(input$cal)()))
  energy_r <- reactive(subset(energy, COMMUNITY.AREA.NAME==renderText(input$car)()))

  area_l <- reactive(subset(chicago_blocks, GEOID10 %in% energy_l()$CENSUS.BLOCK))
  area_r <- reactive(subset(chicago_blocks, GEOID10 %in% energy_r()$CENSUS.BLOCK))

  join_l <- reactive(merge(x=area_l(), y=energy_l(), by.x='GEOID10', by.y='CENSUS.BLOCK'))
  join_r <- reactive(merge(x=area_r(), y=energy_r(), by.x='GEOID10', by.y='CENSUS.BLOCK'))

  re_filter_l <- reactive(switch(input$fbl,
                               "Gas" = subset(join_l(), is.na(join_l()$TOTAL.THERMS)==FALSE),
                               "Electricity" = subset(join_l(), is.na(join_l()$TOTAL.KWH)==FALSE),
                               "Building Age" = join_l(),
                               "Building Type" = join_l(),
                               "Building Height" = join_l(),
                               "Total Population" = subset(join_l(), is.na(join_l()$TOTAL.POPULATION)==FALSE)
  ))

  re_month_l <- reactive(
    if (input$mthl == 'All') {
      switch(input$fbl,
             "Gas" = re_filter_l()[c(1:20,36:60)],
             "Electricity" = re_filter_l()[c(1:35,50:60)],
             "Building Age" = re_filter_l()[c(1:20,50:60)],
             "Building Type" = re_filter_l()[c(1:20,50:60)],
             "Building Height" = re_filter_l()[c(1:20,50:60)],
             "Total Population" = re_filter_l()[c(1:20,50:60)])
    }
    else {
      temp <- re_filter_l()[c(1:20, 20+match(input$mthl, month.name), 34:35, 35+match(input$mthl, month.name), 49:60)]
      switch(input$fbl,
             "Gas" = temp[c(1:20,24,25:36)],
             "Electricity" = temp[c(1:20,21:23,26:36)],
             "Building Age" = temp[c(1:20,26:36)],
             "Building Type" = temp[c(1:20,26:36)],
             "Building Height" = temp[c(1:20,26:36)],
             "Total Population" = temp[c(1:20,26:36)])
    }
  )

  attr_l <- reactive(
    switch(input$fbl,
           "Gas" = switch(input$mthl,
                          "January"   = g_month.y[1],
                          "February"  = g_month.y[2],
                          "March"     = g_month.y[3],
                          "April"     = g_month.y[4],
                          "May"       = g_month.y[5],
                          "June"      = g_month.y[6],
                          "July"      = g_month.y[7],
                          "August"    = g_month.y[8],
                          "September" = g_month.y[9],
                          "October"   = g_month.y[10],
                          "November"  = g_month.y[11],
                          "December"  = g_month.y[12],
                          "All"       = g_month.y[13]  ),
           "Electricity" = switch(input$mthl,
                                  "January"   = e_month.y[1],
                                  "February"  = e_month.y[2],
                                  "March"     = e_month.y[3],
                                  "April"     = e_month.y[4],
                                  "May"       = e_month.y[5],
                                  "June"      = e_month.y[6],
                                  "July"      = e_month.y[7],
                                  "August"    = e_month.y[8],
                                  "September" = e_month.y[9],
                                  "October"   = e_month.y[10],
                                  "November"  = e_month.y[11],
                                  "December"  = e_month.y[12],
                                  "All"       = e_month.y[13] ),
           "Building Age" = "AVERAGE.BUILDING.AGE.y",
           "Building Type" = "BUILDING.TYPE",
           "Building Height" = "AVERAGE.STORIES.y",
           "Total Population" = "TOTAL.POPULATION"
    )
  )

  # For 'All' months
  re_pre_plot_l <- reactive(
    switch(input$fbl,
           "Gas" =  merge(x=re_month_l(), y=aggregate(cbind(THERM.JANUARY.2010,THERM.FEBRUARY.2010,THERM.MARCH.2010,TERM.APRIL.2010,THERM.MAY.2010,THERM.JUNE.2010,
                                                          THERM.JULY.2010,THERM.AUGUST.2010,THERM.SEPTEMBER.2010,THERM.OCTOBER.2010,THERM.NOVEMBER.2010,THERM.DECEMBER.2010,TOTAL.THERMS)
                                                    ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
           "Electricity" = merge(x=re_month_l(), y=aggregate(cbind(KWH.JANUARY.2010,KWH.FEBRUARY.2010,KWH.MARCH.2010,KWH.APRIL.2010,KWH.MAY.2010,KWH.JUNE.2010,
                                                                 KWH.JULY.2010,KWH.AUGUST.2010,KWH.SEPTEMBER.2010,KWH.OCTOBER.2010,KWH.NOVEMBER.2010,KWH.DECEMBER.2010,TOTAL.KWH)
                                                           ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
           "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
           "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
           "Building Type" = re_month_l(),
           "Total Population" = re_month_l()

    )
  )

  re_plot_l <- reactive(
    if (input$mthl=='All') {
      re_pre_plot_l()
    }
    else {
      switch(input$mthl,
             "January" = switch(input$fbl,
                                "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.JANUARY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.JANUARY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                "Building Type" = re_month_l(),
                                "Total Population" = re_month_l()),
             "February" = switch(input$fbl,
                                 "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.FEBRUARY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                 "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.FEBRUARY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                 "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                 "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                 "Building Type" = re_month_l(),
                                 "Total Population" = re_month_l()),
             "March" = switch(input$fbl,
                              "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.MARCH.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                              "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.MARCH.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                              "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                              "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                              "Building Type" = re_month_l(),
                              "Total Population" = re_month_l()),
             "April" = switch(input$fbl,
                              "Gas" =  merge(x=re_month_l(), y=aggregate(TERM.APRIL.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                              "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.APRIL.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                              "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                              "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                              "Building Type" = re_month_l(),
                              "Total Population" = re_month_l()),
             "May" = switch(input$fbl,
                            "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.MAY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                            "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.MAY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                            "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                            "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                            "Building Type" = re_month_l(),
                            "Total Population" = re_month_l()),
             "June" = switch(input$fbl,
                             "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.JUNE.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                             "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.JUNE.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                             "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                             "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                             "Building Type" = re_month_l(),
                             "Total Population" = re_month_l()),
             "July" = switch(input$fbl,
                             "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.JULY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                             "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.JULY.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                             "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                             "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                             "Building Type" = re_month_l(),
                             "Total Population" = re_month_l()),
             "August" = switch(input$fbl,
                               "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.AUGUST.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                               "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.AUGUST.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                               "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                               "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                               "Building Type" = re_month_l(),
                               "Total Population" = re_month_l()),
             "September" = switch(input$fbl,
                                  "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.SEPTEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                  "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.SEPTEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                  "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                  "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                  "Building Type" = re_month_l(),
                                  "Total Population" = re_month_l()),
             "October" = switch(input$fbl,
                                "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.OCTOBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.OCTOBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                "Building Type" = re_month_l(),
                                "Total Population" = re_month_l()),
             "November" = switch(input$fbl,
                                 "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.NOVEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                 "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.NOVEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                 "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                 "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                 "Building Type" = re_month_l(),
                                 "Total Population" = re_month_l()),
             "December" = switch(input$fbl,
                                 "Gas" =  merge(x=re_month_l(), y=aggregate(THERM.DECEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                 "Electricity" = merge(x=re_month_l(), y=aggregate(KWH.DECEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_l()), by='GEOID10'),
                                 "Building Age" = merge(x=re_month_l(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                 "Building Height" = merge(x=re_month_l(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_l()), by='GEOID10'),
                                 "Building Type" = re_month_l(),
                                 "Total Population" = re_month_l()
      )

      )
    }
  )

  re_plot_l_1 <- reactive(switch(input$btl,
                               "Residential" = subset(re_plot_l(), BUILDING.TYPE=='Residential'),
                               "Commercial" = subset(re_plot_l(), BUILDING.TYPE=='Commercial'),
                               "Industrial" = subset(re_plot_l(), BUILDING.TYPE=='Industrial'),
                               "All" = re_plot_l()
                               )
  )


  observe(
    if (input$colorl == "1"){
      mapviewOptions(basemaps = "CartoDB.Positron",
                     vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")))
    }
    else if (input$colorl == "2") {
      mapviewOptions(basemaps = "CartoDB.Positron",
                     vector.palette = colorRampPalette(c("pink","red","grey10")))
    }
    else {
      mapviewOptions(basemaps = "CartoDB.Positron",
                     vector.palette = colorRampPalette(c("green","yellow","grey10")))
    }
  )


  output$mapl <- renderLeaflet(
    # reset button
    if(input$resetl) {
      mapview(re_plot_l_1(), zcol=attr_l(), alpha.regions=0.25, alpha=0.1, layer.name=attr_l())@map
    }
    else{
      mapview(re_plot_l_1(), zcol=attr_l(), alpha.regions=0.25, alpha=0.1, layer.name=attr_l())@map
    }

  )

  re_filter_r <- reactive(switch(input$fbr,
                                 "Gas" = subset(join_r(), is.na(join_r()$TOTAL.THERMS)==FALSE),
                                 "Electricity" = subset(join_r(), is.na(join_r()$TOTAL.KWH)==FALSE),
                                 "Building Age" = join_r(),
                                 "Building Type" = join_r(),
                                 "Building Height" = join_r(),
                                 "Total Population" = subset(join_r(), is.na(join_r()$TOTAL.POPULATION)==FALSE)
  ))

  re_month_r <- reactive(
    if (input$mthr == 'All') {
      switch(input$fbr,
             "Gas" = re_filter_r()[c(1:20,36:60)],
             "Electricity" = re_filter_r()[c(1:35,50:60)],
             "Building Age" = re_filter_r()[c(1:20,50:60)],
             "Building Type" = re_filter_r()[c(1:20,50:60)],
             "Building Height" = re_filter_r()[c(1:20,50:60)],
             "Total Population" = re_filter_r()[c(1:20,50:60)])
    }
    else {
      temp <- re_filter_r()[c(1:20, 20+match(input$mthr, month.name), 34:35, 35+match(input$mthr, month.name), 49:60)]
      switch(input$fbr,
             "Gas" = temp[c(1:20,24,25:36)],
             "Electricity" = temp[c(1:20,21:23,26:36)],
             "Building Age" = temp[c(1:20,26:36)],
             "Building Type" = temp[c(1:20,26:36)],
             "Building Height" = temp[c(1:20,26:36)],
             "Total Population" = temp[c(1:20,26:36)])
    }
  )

  attr_r <- reactive(
    switch(input$fbr,
           "Gas" = switch(input$mthr,
                          "January"   = g_month.y[1],
                          "February"  = g_month.y[2],
                          "March"     = g_month.y[3],
                          "April"     = g_month.y[4],
                          "May"       = g_month.y[5],
                          "June"      = g_month.y[6],
                          "July"      = g_month.y[7],
                          "August"    = g_month.y[8],
                          "September" = g_month.y[9],
                          "October"   = g_month.y[10],
                          "November"  = g_month.y[11],
                          "December"  = g_month.y[12],
                          "All"       = g_month.y[13]  ),
           "Electricity" = switch(input$mthr,
                                  "January"   = e_month.y[1],
                                  "February"  = e_month.y[2],
                                  "March"     = e_month.y[3],
                                  "April"     = e_month.y[4],
                                  "May"       = e_month.y[5],
                                  "June"      = e_month.y[6],
                                  "July"      = e_month.y[7],
                                  "August"    = e_month.y[8],
                                  "September" = e_month.y[9],
                                  "October"   = e_month.y[10],
                                  "November"  = e_month.y[11],
                                  "December"  = e_month.y[12],
                                  "All"       = e_month.y[13] ),
           "Building Age" = "AVERAGE.BUILDING.AGE.y",
           "Building Type" = "BUILDING.TYPE",
           "Building Height" = "AVERAGE.STORIES.y",
           "Total Population" = "TOTAL.POPULATION"
    )
  )

  # For 'All' months
  re_pre_plot_r <- reactive(
    switch(input$fbr,
           "Gas" =  merge(x=re_month_r(), y=aggregate(cbind(THERM.JANUARY.2010,THERM.FEBRUARY.2010,THERM.MARCH.2010,TERM.APRIL.2010,THERM.MAY.2010,THERM.JUNE.2010,
                                                            THERM.JULY.2010,THERM.AUGUST.2010,THERM.SEPTEMBER.2010,THERM.OCTOBER.2010,THERM.NOVEMBER.2010,THERM.DECEMBER.2010,TOTAL.THERMS)
                                                      ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
           "Electricity" = merge(x=re_month_r(), y=aggregate(cbind(KWH.JANUARY.2010,KWH.FEBRUARY.2010,KWH.MARCH.2010,KWH.APRIL.2010,KWH.MAY.2010,KWH.JUNE.2010,
                                                                   KWH.JULY.2010,KWH.AUGUST.2010,KWH.SEPTEMBER.2010,KWH.OCTOBER.2010,KWH.NOVEMBER.2010,KWH.DECEMBER.2010,TOTAL.KWH)
                                                             ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
           "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
           "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
           "Building Type" = re_month_r(),
           "Total Population" = re_month_r()

    )
  )

  re_plot_r <- reactive(
    if (input$mthr=='All') {
      re_pre_plot_r()
    }
    else {
      switch(input$mthr,
             "January" = switch(input$fbr,
                                "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.JANUARY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.JANUARY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                "Building Type" = re_month_r(),
                                "Total Population" = re_month_r()),
             "February" = switch(input$fbr,
                                 "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.FEBRUARY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                 "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.FEBRUARY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                 "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                 "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                 "Building Type" = re_month_r(),
                                 "Total Population" = re_month_r()),
             "March" = switch(input$fbr,
                              "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.MARCH.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                              "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.MARCH.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                              "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                              "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                              "Building Type" = re_month_r(),
                              "Total Population" = re_month_r()),
             "April" = switch(input$fbr,
                              "Gas" =  merge(x=re_month_r(), y=aggregate(TERM.APRIL.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                              "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.APRIL.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                              "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                              "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                              "Building Type" = re_month_r(),
                              "Total Population" = re_month_r()),
             "May" = switch(input$fbr,
                            "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.MAY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                            "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.MAY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                            "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                            "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                            "Building Type" = re_month_r(),
                            "Total Population" = re_month_r()),
             "June" = switch(input$fbr,
                             "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.JUNE.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                             "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.JUNE.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                             "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                             "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                             "Building Type" = re_month_r(),
                             "Total Population" = re_month_r()),
             "July" = switch(input$fbr,
                             "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.JULY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                             "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.JULY.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                             "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                             "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                             "Building Type" = re_month_r(),
                             "Total Population" = re_month_r()),
             "August" = switch(input$fbr,
                               "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.AUGUST.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                               "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.AUGUST.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                               "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                               "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                               "Building Type" = re_month_r(),
                               "Total Population" = re_month_r()),
             "September" = switch(input$fbr,
                                  "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.SEPTEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                  "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.SEPTEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                  "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                  "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                  "Building Type" = re_month_r(),
                                  "Total Population" = re_month_r()),
             "October" = switch(input$fbr,
                                "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.OCTOBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.OCTOBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                "Building Type" = re_month_r(),
                                "Total Population" = re_month_r()),
             "November" = switch(input$fbr,
                                 "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.NOVEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                 "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.NOVEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                 "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                 "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                 "Building Type" = re_month_r(),
                                 "Total Population" = re_month_r()),
             "December" = switch(input$fbr,
                                 "Gas" =  merge(x=re_month_r(), y=aggregate(THERM.DECEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                 "Electricity" = merge(x=re_month_r(), y=aggregate(KWH.DECEMBER.2010 ~ GEOID10, FUN = sum, data=re_month_r()), by='GEOID10'),
                                 "Building Age" = merge(x=re_month_r(), y=aggregate(AVERAGE.BUILDING.AGE ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                 "Building Height" = merge(x=re_month_r(), y=aggregate(AVERAGE.STORIES ~ GEOID10, FUN = mean, data=re_month_r()), by='GEOID10'),
                                 "Building Type" = re_month_r(),
                                 "Total Population" = re_month_r()
             )

      )
    }
  )

  re_plot_r_1 <- reactive(switch(input$btr,
                                 "Residential" = subset(re_plot_r(), BUILDING.TYPE=='Residential'),
                                 "Commercial" = subset(re_plot_r(), BUILDING.TYPE=='Commercial'),
                                 "Industrial" = subset(re_plot_r(), BUILDING.TYPE=='Industrial'),
                                 "All" = re_plot_r()
                                 )
  )


  observe(
    if (input$colorr == "1"){
      mapviewOptions(basemaps = "CartoDB.Positron",
                     vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")))
    }
    else if (input$colorr == "2") {
      mapviewOptions(basemaps = "CartoDB.Positron",
                     vector.palette = colorRampPalette(c("pink","red","grey10")))
    }
    else {
      mapviewOptions(basemaps = "CartoDB.Positron",
                     vector.palette = colorRampPalette(c("green","yellow","grey10")))
    }
  )


  output$mapr <- renderLeaflet(
    # reset button
    if(input$resetr) {
      mapview(re_plot_r_1(), zcol=attr_r(), alpha.regions=0.25, alpha=0.1, layer.name=attr_r())@map
    }
    else{
      mapview(re_plot_r_1(), zcol=attr_r(), alpha.regions=0.25, alpha=0.1, layer.name=attr_r())@map
    }
  )


  ##############################################################################
  # All of Chicago tab
  top_range <- round(nrow(t.merge) / 10)
  re_aoc <- reactive(
    switch(input$top,
           "Oldest Buildings" = t.building.age[with(t.building.age, order(-AVERAGE.BUILDING.AGE)), ][1:top_range,],
           "Newest Buildings" = t.building.age[with(t.building.age, order(AVERAGE.BUILDING.AGE)), ][1:top_range,],
           "Tallest Buildings" = t.building.height[with(t.building.height, order(-AVERAGE.STORIES)), ][1:top_range,],
           "Most Gas" = t.gas[with(t.gas, order(-TOTAL.THERMS)), ][1:top_range,],
           "Most Electricity" = t.electricity[with(t.electricity, order(-TOTAL.KWH)), ][1:top_range,],
           "Most Population" = t.population[with(t.population, order(-TOTAL.POPULATION)), ][1:top_range,],
           "Most Occupied Percentage" = t.occupied[with(t.occupied, order(-OCCUPIED.UNITS.PERCENTAGE)), ][1:top_range,],
           "Highest Percentage Renters" = t.highest.renters[with(t.highest.renters, order(-RENTER.OCCUPIED.HOUSING.PERCENTAGE)), ][1:top_range,]
    )
  )

  output$mapaoc <- renderLeaflet(
    # reset button
    if(input$resetaoc) {
      switch (input$top,
              "Oldest Buildings" = mapview(re_aoc(), zcol=aoc_attr[1], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[1])@map,
              "Newest Buildings" = mapview(re_aoc(), zcol=aoc_attr[1], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[1])@map,
              "Tallest Buildings" = mapview(re_aoc(), zcol=aoc_attr[2], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[2])@map,
              "Most Electricity" = mapview(re_aoc(), zcol=aoc_attr[3], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[3])@map,
              "Most Gas" =  mapview(re_aoc(), zcol=aoc_attr[4], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[4])@map,
              "Most Population" = mapview(re_aoc(), zcol=aoc_attr[5], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[5])@map,
              "Most Occupied Percentage" = mapview(re_aoc(), zcol=aoc_attr[6], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[6])@map,
              "Highest Percentage Renters" =mapview(re_aoc(), zcol=aoc_attr[7], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[7])@map
      )
    }
    else{
      switch (input$top,
              "Oldest Buildings" = mapview(re_aoc(), zcol=aoc_attr[1], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[1])@map,
              "Newest Buildings" = mapview(re_aoc(), zcol=aoc_attr[1], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[1])@map,
              "Tallest Buildings" = mapview(re_aoc(), zcol=aoc_attr[2], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[2])@map,
              "Most Electricity" = mapview(re_aoc(), zcol=aoc_attr[3], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[3])@map,
              "Most Gas" =  mapview(re_aoc(), zcol=aoc_attr[4], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[4])@map,
              "Most Population" = mapview(re_aoc(), zcol=aoc_attr[5], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[5])@map,
              "Most Occupied Percentage" = mapview(re_aoc(), zcol=aoc_attr[6], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[6])@map,
              "Highest Percentage Renters" =mapview(re_aoc(), zcol=aoc_attr[7], alpha.regions=0.25, alpha=0.5, layer.name=aoc_attr[7])@map
      )
    }
  )

  ##############################################################################
  # Data Integration tab
  output$mapg <- renderLeaflet(
    # reset button
    if(input$resetg) {
      switch (input$gender,
              "Male" = mapview(gen.sim, zcol='Under18M', alpha.regions=0.25, alpha=0.1, layer.name='Male under 18')@map,
              "Female" = mapview(gen.sim, zcol='Under18F', alpha.regions=0.25, alpha=0.1, layer.name='Female under 18')@map
      )
    }
    else{
      switch (input$gender,
              "Male" = mapview(gen.sim, zcol='Under18M', alpha.regions=0.25, alpha=0.1, layer.name='Male under 18')@map,
              "Female" = mapview(gen.sim, zcol='Under18F', alpha.regions=0.25, alpha=0.1, layer.name='Female under 18')@map
      )
    }
  )
}

shinyApp(ui = ui, server = server)