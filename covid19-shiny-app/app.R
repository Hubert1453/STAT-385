#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(usmap)
library(ggplot2)
library(shinyWidgets)
library(scales)
library(rnaturalearthdata)
library(rnaturalearth)
library(rworldmap)
library(Cairo)
library(RColorBrewer)
library(dplyr)
library(readr)
library(ggmap)
library(purrr)
library(geosphere)

# importing datasets
NYTimes_US_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
NYTimes_US_States_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
NYTimes_US_Counties_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
COVID_Tracking_Project_US_Historical_Data <- read_csv("https://covidtracking.com/data/download/national-history.csv")
#importing international COVID data for comparison with domestic
WHO_COVID_19_Situation_Report_Data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv")
#up-to-date global covid19 data from WHO
WHO_Global_Historical_Data = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
#estimates of mask usage by county from a nationwide survey
NYTIMES_US_mask_use_by_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv")

#sorting the data to see new cases and new deaths in the data
Champaign_data <- NYTimes_US_Counties_Historical_Data %>% 
  filter(county == "Champaign" & state == "Illinois") %>%
  arrange(date) %>%
  mutate(new_cases = 0, new_deaths = 0, new_cases = cases - lag(cases,default = 0), new_deaths = deaths - lag(deaths,default = 0)) %>%
  select(-c(state,fips))



# Define UI for application that draws a histogram
ui <- navbarPage("STAT385 Fall 2020 Covid-19 App", theme = shinytheme("flatly"),
                 tabPanel("Home", tags$head(tags$style(HTML(".tab-content {margin: 20px;}"))),
                          "This app is specifically designed to give you the latest information on Covid-19, including graphs of the number of cases in each month by state, accompanied by the number of deaths. Please remember to follow CDC guidelines by wearing a mask and staying 6 feet apart to limit transmission.",
                          tags$a(href = "https://my.castlighthealth.com/corona-virus-testing-sites/", "Click here if you are experiencing any of the followding Covid-19 symptoms to find the nearest testing location to you."),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/duration-isolation.html", "Click here for precautionary recommendations and CDC isolation guidlines."),
                          
                          # Add Virus Photo as a Visual Aid for the site
                          img(src="https://www.statnews.com/wp-content/uploads/2020/02/Coronavirus-CDC-1600x900.jpg", height= 55, width=95),
                          
                          h3("Fever, Cough, Chills, Fatigue, Muscle Soreness, Sore Throat, Loss of Taste or Smell "),
                          
                          # Symptoms
                          checkboxGroupInput("symptoms", h3("Symptoms"), 
                                             choices = list("Fever or chills",
                                                            "Cough", 
                                                            "Shortness of breath or difficulty breathing",
                                                            "Fatigue",
                                                            "Muscle or body aches",
                                                            "Headache",
                                                            "New loss of taste or smell",
                                                            "Sore throat",
                                                            "Congestion or runny nose",
                                                            "Nausea or vomiting",
                                                            "Diarrhea")
                          ),
                          htmlOutput("symptom_choice"),
                          tags$a(href = "https://landing.google.com/screener/covid19?source=google", "Do a self-assessment here"),
                          tags$a(href = "https://my.castlighthealth.com/corona-virus-testing-sites/", "or find your testing site here"),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html#seek-medical-attention", "or distinguish whether flu or COVID here"),
                          
                          h4("CDC Information about Symptoms:"),
                          tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/images/social/covid19-symptoms-fb.png", height = 400, width=600),
                          
                          
    h5("Illinois Current Phases by Region"),
    
    # Illinois Current Phases by Region
    radioButtons("regions", h5("Regions"), 
                       choices = list("Region 1", 
                                      "Region 2",
                                      "Region 3",
                                      "Region 4",
                                      "Region 5",
                                      "Region 6",
                                      "Region 7",
                                      "Region 8",
                                      "Region 9",
                                      "Region 10",
                                      "Region 11")
                      
    ),
    htmlOutput("region_choice"),
    
# Add Quarantine vs Isolation Infographic as a Visual Aid for the site
h6("CDC Information about Quarantine vs Isolation:"),
tags$img(src="https://www.co.lincoln.or.us/sites/default/files/styles/gallery500/public/imageattachments/hhs/page/7501/covid-19-quarantine-vs-isolation.png?itok=yDWeXaEg", height= 600, width=600),

                          #Ways to remain safe during the pandemic 
                          
                          h1("Ways to remain safe during the pandemic"),
                          img(src="https://www.mainlineart.org/wp-content/uploads/2020/06/COVID-ICONS-1024x724.jpg", height= 375, width= 750),
                              #add good habit checkbox
    tags$br(),
    tags$div(h6("Check your great prevention habits"),
      checkboxGroupInput("habits", label = NULL, 
                       choices = list("Wear maskes",
                                      "Use hand sanitizer", 
                                      "Wear gloves",
                                      "Keep social distance",
                                      "Wash hands often"))),
    htmlOutput("habit_check"),
                          tags$br(),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html", "Click here to see the CDC's covid safety guidelines."),
                          tags$br(),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/community/organizations/business-employers/bars-restaurants.html", "Click here to see the CDC's covid recommendations for restaurant owners."),
                          tags$br(),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/essential-goods-services.html", "Click here to see the CDC's recommendations for staying safe while running essential errands"),
    
                          #Other credible inter-governmental agencies sources
                          tags$div(
                            h1("Other Inter-governmental Agencies' Sources"), 
                            "This list contains internet URLs for various official inter-governmental agencies' websites",
                            tags$br(),
                            tags$a(href = "https://covid19.who.int", "WHO Coronavirus Disease (COVID-19) Overview."),
                            tags$br(),
                            tags$a(href = "https://covid19.who.int/table", "WHO Coronavirus Disease (COVID-19) Data Table."),
                            tags$br(),
                            tags$a(href = "https://www.who.int/health-topics/coronavirus#tab=tab_1", "Coronavirus disease (COVID-19)."),
                            tags$br(),
                            tags$a(href = "https://www.who.int/csr/don/en/", "Disease Outbreak News (DONs)."),
                            tags$br(),
                            tags$a(href = "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases", "COVID-19 Data, News, and Reports from the European Center for Disease Prevention and Control"),
                            tags$br(),
                            tags$a(href = "https://www.worldometers.info/coronavirus/", "COVID-19 Data, News, and Reports from the WorldoMeters."),
                            tags$br(),
                            tags$a(href = "https://covid.ourworldindata.org/data/ecdc/total_cases.csv", "COVID-19 Data, News, and Reports from OurWorldinData")
                          ),
                          
                          #Highly-rated nonprofits providing relief to those impacted by the pandemic
                          tags$div(
                            h1("Looking to Help?"), 
                            "This list contains internet URLs for highly rated nonprofit organizations providing relief and recovery to communities impacted by the pandemic",
                            tags$br(),
                            tags$a(href = "https://www.hearttoheart.org/", "Heart to Heart International: Improving global health through humanitarian initiatives that connect people and resources to a world in need."),
                            tags$br(),
                            tags$a(href = "https://icfdn.org/", "International Community Foundation: Seeks to increase charitable giving and volunteerism to benefit communities and nonprofit organizations."),
                            tags$br(),
                            tags$a(href = "https://www.healthcorps.org/", "HealthCorps: Strengthening communities with the most innovative approaches to health and wellness."),
                            tags$br(),
                            tags$a(href = "https://www.crisisaid.org/", "Crisis Aid International: Bringing necessary foods, materials, and medicines to people in times of crisis."),
                          )
                 ),
                 
                 tabPanel("UIUC", tags$head(tags$style(HTML(".tab-content {margin: 20px;}"))),
                          #add UIUC covid-19 dashboard
                          tags$a(href = "https://go.illinois.edu/COVIDTestingData", "Click here for UIUC Covid-19 Dashboard."),
                          #add volunteer signup link in UIUC panel
                          tags$br(),
                          tags$a(href = "https://union.illinois.edu/get-involved/office-of-volunteer-programs", "Click here to become a volunteer in UIUC."),
                          
                          tags$h3("Covid Data in Champaign"),
                          
                          fluidRow(
                            column(3,
                                   dateRangeInput(inputId = "date_range_covid",
                                                  label = "Date Range",
                                                  start = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                  end =  as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                  min = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                  max = as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                  separator = 'to')),
                            
                            column(3,
                                   selectInput(inputId = "Graph_Type", 
                                               label = "Types", 
                                               c("New Cases", "Total Cases","New Deaths","Total Deaths")))
                          ),
                          
                          plotOutput("lol"),
                          
                          h1("Cumulative Cases in Champaign County"),
                          fluidRow(
                            column(width = 12,
                                   plotOutput("champaign_cases", height = 350,hover = hoverOpts(id ="plot_hover"))
                            )
                          ),
                          fluidRow(
                            column(width = 5,
                                   verbatimTextOutput("hover_info")
                            )
                          ),
                          h2("Cumulative Deaths in Champaign County"),
                          plotOutput("champaign_deaths"),
                          h2("Cumulative Recoveries in Champaign County"),
                          plotOutput("champaign_recoveries"),
                          
                          h1("Closest Testing Site Near You"),
                          h4("Enter your longitude and latitude of your address using the link below."),
                          uiOutput("tab"),
                          numericInput(inputId = "long", label = "Enter Longitude", value = 0),
                          numericInput(inputId = "lat", label = "Enter the Latitude", value = 0),
                          textOutput("testing_site"),
                          
                          #add the Growth Rate of Cases plot in Champaign County
                          h1("Daily Growth Rate of Cases in Champaign County"),
                          sliderInput(
                            "champaign_growth_date",
                            "Select the range of date from the first case appeared",
                            min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                              NYTimes_US_Counties_Historical_Data$county == "Champaign",
                              NYTimes_US_Counties_Historical_Data$state == "Illinois"
                            ),]$date[1], "%Y-%m-%d"),
                            max = as.Date(
                              tail(NYTimes_US_Counties_Historical_Data[which(
                                NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                NYTimes_US_Counties_Historical_Data$state == "Illinois"
                              ),]$date, 1),
                              "%Y-%m-%d"
                            ),
                            value = as.Date("2020-04-22", "%Y-%m-%d")
                          ),
                          
                          plotOutput("champaign_growth"),
                          
                          # add precise search for cases around Champaign
                          h1("Seach for number of cases"),
                          dateInput(
                            "champaignCasesSearch_Input",
                            "Please select a date",
                            value = as.Date(NYTimes_US_Counties_Historical_Data[which(
                              NYTimes_US_Counties_Historical_Data$county == "Champaign",
                              NYTimes_US_Counties_Historical_Data$state == "Illinois"
                            ), ]$date[1], "%Y-%m-%d"),
                            min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                              NYTimes_US_Counties_Historical_Data$county == "Champaign",
                              NYTimes_US_Counties_Historical_Data$state == "Illinois"
                            ), ]$date[1], "%Y-%m-%d"),
                            max = as.Date(
                              tail(NYTimes_US_Counties_Historical_Data[which(
                                NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                NYTimes_US_Counties_Historical_Data$state == "Illinois"
                              ), ]$date, 1),
                              "%Y-%m-%d"
                            )
                          ),
                          textOutput("champaign_cases_search")
                 ),
                
    
                 
                 tabPanel("HOW TO HELP",
                          
                          fluidPage(fluidRow(column(12,align="center",tags$img(src="https://pbs.twimg.com/media/EUdWR4qXkAEwlJ2.jpg",width="50%"))),
                                    fluidRow(column(4,tags$br(),align="center",tags$img(src="https://www.charities.org/sites/default/files/styles/large/public/AmericasCharities_COVID19-Fund_300pxAd_4-2-20.jpg?itok=f4MozTVN",width="51%")),
                                             column(4,tags$br(),align="center",tags$img(src="https://covid19responsefund.org/assets/share-social.png",width="75%")),
                                             column(4,tags$br(),align="center",tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/images/hhs/Donate-Plasma-Navy.jpg", width="75%"))),
                                    fluidRow(column(4,tags$br(),
                                                    "Nonprofits all across America are playing a vital role in supporting the people and communities that have been impacted by the COVID-19 pandemic",
                                                    tags$br(),
                                                    h4("Find Out How You Can Help"),
                                                    tags$a(href = "https://www.charities.org/coronavirus-covid-19-donations-funds-and-resources?gclid=CjwKCAiA_Kz-BRAJEiwAhJNY71h4gb5n8UlD5eDwUK_WhK1JSVdwvWiFsqnVZNoTkcthCBpSdMQwfRoCT3QQAvD_BwE","List of Nonprofits and Charities"),
                                                    tags$br(),
                                                    tags$a(href = "https://www.feedingamerica.org/take-action/coronavirus","Feeding America's Coronavirus Response Fund"),
                                                    tags$br(),
                                                    tags$a(href = "https://www.powerof.org/volunteer","Sign Up to Become a Volunteer!")
                                    ),
                                    column(4,tags$br(),
                                           h4("World Health Organization"),
                                           "The World Health Organization is leading and coordinating the global effort to combat the COVID-19 pandemic by helping countries prevent, detect, and respond to the virus.",
                                           tags$br(),
                                           tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/donate",
                                                  "Click Here to Learn More About the COVID-19 Solidarity Response Fund."),
                                           tags$br(),
                                           tags$a(href = "https://covid19responsefund.org/en/",
                                                  "Click here to Donate to the COVID-19 Solidarity Response Fund."),
                                           h4("National Voluntary Organizations Active in Disaster"),
                                           "While the WHO is spearheading the international effort, the NVOAD is helping to assist in the US's response to COVID-19.",
                                           tags$br(),
                                           tags$a(href = "https://www.nvoad.org/covid-19-response/", "Click here to Donate to the National Voluntary Organizations Active in Disaster.")
                                    ),
                                    column(4,tags$br(),
                                           "Because you fought the infection, your plasma now contains COVID-19 antibodies. These antibodies provided one way for your immune system to fight the virus when you were sick, so your plasma may be able to be used to help others fight off the disease.",
                                           tags$h4("Plasma Donations"),
                                           tags$a(href = "https://www.fda.gov/emergency-preparedness-and-response/coronavirus-disease-2019-covid-19/donate-covid-19-plasma",
                                                  "Click Here to Learn More About how Donating Plasma Saves Lives."),
                                           tags$br(),
                                           tags$a(href = "https://thefightisinus.org/en-US/home#home",
                                                  "Click Here if You Are Interested in Donating Plasma."),
                                           tags$h4("Blood Donations"),
                                           tags$a(href = "https://www.redcrossblood.org/", "Click Here if You Are Interested in Donating Blood.")   
                                    ))
                          )
                 ),
                 
                 tabPanel("U.S",
                          h1("Total Cumulative Cases and Deaths in United States"),
                          plotOutput("cm_plot"),
                          sliderInput("date", "Choose a date",
                                      min = as.Date(min(NYTimes_US_States_Historical_Data$date),"%Y-%m-%d"), 
                                      max = as.Date(max(NYTimes_US_States_Historical_Data$date),"%Y-%m-%d"),
                                      value = c(as.Date(min(NYTimes_US_States_Historical_Data$date)), 
                                                as.Date(max(NYTimes_US_States_Historical_Data$date))),timeFormat="%d %b", 
                                      dragRange = TRUE,
                                      width = "100%"),
                          h2(textOutput("case_count"), align = "left"),
                          h2(textOutput("death_count"), align = "left"),
                          selectInput(inputId = "state1", label = "Choose a state", choices = sort(unique(NYTimes_US_States_Historical_Data$state))),
                          plotOutput(outputId = "stateplot", click = "plot_click",dblclick = "plot_dblclick",hover="plot_hover",brush = "plot_brush"),
                          verbatimTextOutput("info1"),
                          plotOutput(outputId = "stateplot2", click = "plot_click2",dblclick = "plot_dblclick",hover="plot_hover",brush = "plot_brush"),
                          verbatimTextOutput("info2"),
                          h1("Covid-19 Deaths per Month"),
                          plotOutput("deaths_per_month"),
                          h1("Cases by states"),
                          plotOutput("caseplot"),
                          h1("Covid-19 Cases per County"),
                          selectInput(inputId = "state_map", label = "Choose a state",
                                      choices = c("All states", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                      selected = "All states"),
                          #plotOutput to be completed later in server function.
                          h1("Covid-19 Deaths per County"),
                          selectInput(inputId = "state_map", label = "Choose a state",
                                      choices = c("All states", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                      selected = "All states"),
                          plotOutput("mapplot"),
                          h1("Latest Deaths/Cases by State"),
                          plotOutput("latestplot"),
                          h1("Monthly Increase of Recovered Number"),
                          plotOutput("recoveredplot"),
                          h1("Recovered Rate and Death Rate and Positive rate (US) per day", align = "left"),
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          radioGroupButtons(
                                            inputId = "type_rate",
                                            label = "",
                                            choices = c("recovered_rate",
                                                        "death_rate", 
                                                        "positive_rate",
                                                        "deathvsrecover",
                                                        "all"
                                            ),
                                            justified = FALSE
                                          ),
                                          
                                        ),
                                        
                                        
                                        
                                        mainPanel(dateRangeInput(inputId = "Rate",
                                                                 label = "Select a date range",
                                                                 start = as.Date(min(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                 end =  as.Date(max(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                 min = as.Date(min(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                 max = as.Date(max(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                 separator = 'to'),
                                                  plotOutput("recovered_death_rate")
                                        )
                          ),
                          
                          
                          
                          
                          sliderInput(inputId = "date1",
                                      label = "From the date of",
                                      value = c(min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                      min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                      max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                      dragRange = TRUE,
                                      width = "100%"),
                          plotOutput("testplot"),
                          h1("Estimated Current Cases By County"),
                          selectInput(inputId = "state.input", label = "Select a State",
                                      choices = c("All States", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                      selected = "All States"),
                          plotOutput("county"),
                          
                          #Section for Links specifically for United States Information including advisories, mandates, and travel bans
                          h1("Useful Links for United States Information"),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html", "Click here for the latest information regarding COVID-19 from the CDC."),
                          tags$br(),
                          tags$a(href = "https://www.nytimes.com/interactive/2020/us/states-reopen-map-coronavirus.html", "Click here for the latest information regarding COVID-19 Advisories, Mandates, and Travel Restrictions"),
                          tags$br(),
                          tags$a(href = "https://www.usatoday.com/storytelling/coronavirus-reopening-america-map/#restrictions", "Click here for the latest information on COVID-19 State Policy Trends"),
                          tags$br(),
                          tags$a(href = "https://www.usatoday.com/storytelling/coronavirus-reopening-america-map/", "Click here for the COVID-19 restrictions among each states"),
                          tags$br(),
                          h1("Current Hospitalize Patients"),
                          plotOutput("hos"),
                          
                          h1("Covid-19 In US States - Cases VS Deaths. Contains Regression Line For Toggle Points"),
                          fluidRow(
                            column(width = 8,
                                   plotOutput("plot", height = 300,
                                              click = "plot_click",
                                              brush = brushOpts(
                                                id = "plot_brush"
                                              )
                                   ),
                                   actionButton("exclude_toggle", "Toggle points"),
                                   actionButton("exclude_reset", "Reset")
                            )
                          ),
                          
                          #added what percent of state has covid
                          h1("Percent of Population Covid-19 Positive by State"),
                          selectInput(inputId = "state0", label = "Choose a state",
                                      choices = c("All states", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                      selected = "All states"),
                          textOutput("percent_pos_state"),
                          
                          
                 ),
                 
                 
                 
                 tabPanel("Global",
                          h1("Global COVID-19 Situation by Region"),
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          radioGroupButtons(
                                            inputId = "type",
                                            label = "",
                                            choices = c("New_cases",
                                                        "Cumulative_cases",
                                                        "New_deaths",
                                                        "Cumulative_deaths"),
                                            justified = FALSE
                                            
                                          ),
                                          htmlOutput("cases_count_region")
                                        ),
                                        mainPanel(sliderInput(inputId = "date2",
                                                              label = "From the date of",
                                                              value = c(min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                                              min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                              max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                              dragRange = TRUE,
                                                              width = "100%"),
                                                  plotOutput("plot_region")
                                        )
                          ),
                          h1("World Map Representation By Case"),
                          plotOutput("plot_world"),
                          em("*Grayed out country indicates data is missing"),
                          h1("World Map Representation By New Cases"),
                          plotOutput("new_cases_world"),
                          em("*Grayed out country indicates data is missing"),
                          h1("World Map Representation By Death"),
                          plotOutput("death_plot_world"),
                          em("*Grayed out country indicates data is missing"), 
                          
                          
                          #Commit 1- Graph detailing R^2 between ventilator, hospitalization, and positive test result and death over time with an adjustable delay for COVID (measuring death latency essentially)
                          
                          
                          h1("Death Latency", align = "left"),
                          h4("\nAdjust the delay to understand the relationship between COVID stages and death"),
                          h4("\nHigher R Squared values indicate better correlation"),
                          h4("\nUse the date range to explore various periods in the pandemic"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(inputId = "drange",
                                          label = "Select Date Range",
                                          value = c(as.Date("2020-5-26"),max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                          min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                          max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                          dragRange = TRUE),
                              sliderInput(inputId = "Lag", label = "Select the Delay Amount (Days)", min=0, max=21, value=14),
                              radioGroupButtons(inputId = "Curve", label = "", direction = "vertical", selected="hosp",
                                                choices = c("Positive Test"="pos", "Hospitalization"="hosp", "Ventilator"="vent")),
                              htmlOutput("Rsquared")),
                            mainPanel(plotOutput("Death_Latency"))),
                          
                          #Commit 2 Graph detailing ventilator usage as a percentage of hospitalized COVID patients over time
                          
                          h1("Ventilator Usage as a Percentage of Hospitalized COVID patients", align = "left"),
                          sliderInput(inputId = "Ventilator",
                                      label = "Select Date Range",
                                      value = c(min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                      min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                      max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                      dragRange = TRUE),
                          plotOutput("Ventilator_Usage"),
                          
                          h1("Zoom Plot on Global Covid 19 Cases Cumulative Cases VS. Death"),
                          fluidRow(
                            column(width = 4, class = "well",
                                   h4("Brush and double-click to zoom"),
                                   plotOutput("plot1", height = 300,
                                              dblclick = "plot1_dblclick",
                                              brush = brushOpts(
                                                id = "plot1_brush",
                                                resetOnNew = TRUE
                                              )
                                   )
                            ),
                            column(width = 8, class = "well",
                                   h4("Left plot controls right plot"),
                                   fluidRow(
                                     column(width = 6,
                                            plotOutput("plot2", height = 300,
                                                       brush = brushOpts(
                                                         id = "plot2_brush",
                                                         resetOnNew = TRUE
                                                       )
                                            )
                                     ),
                                     column(width = 6,
                                            plotOutput("plot3", height = 300)
                                     )
                                   )
                            )
                            
                          )
                          
                          
                 ),
    
    tabPanel("VACCINE",
             #add link about information on Covid 19 vaccine
             tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/vaccines/index.html", "Click here for more information on COVID-19 Vaccine."),
             tags$br(),
             tags$a(href = "https://www.nytimes.com/interactive/2020/12/03/opinion/covid-19-vaccine-timeline.html", "Click here to fill out a survey to find your place in the COVID-19 Vaccine Line")
    ))


#Plotting Positive Cases Increase Per Month In Correlation to Number of Deaths
positivecases <- COVID_Tracking_Project_US_Historical_Data

boxplot(death ~ positiveIncrease, data = positivecases,
        xlab = "Positive Increase in Case", 
        ylab = "Number of Deaths", 
        main = "Positive Increase vs Number of Deaths",
        pch = 20, 
        cex = 2, 
        col = "dark orange",
        border = "black")


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(NYTimes_US_States_Historical_Data))
  )
  
  output$plot <- renderPlot({
    
    keep    <- NYTimes_US_States_Historical_Data[ vals$keeprows, , drop = FALSE]
    exclude <- NYTimes_US_States_Historical_Data[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(cases, deaths)) + geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "blue") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(0,20000), ylim = c(0,9000))
  })
  
  
  observeEvent(input$plot_click, {
    res <- nearPoints(NYTimes_US_States_Historical_Data, input$plot_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(NYTimes_US_States_Historical_Data, input$plot_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(NYTimes_US_States_Historical_Data))
  })
  
  
  
  # Symptoms
  output$symptom_choice <- renderText({
    symptoms <- paste(input$symptoms, collapse = ", ")
    paste(strong("Since you think you may have:"), symptoms)
  })
    #habits 
  output$habit_check <- renderText({
    habits <- paste(input$habits, collapse = ", ")
    paste(strong("Good habits you have:"), habits)})
  
  output$region_choice <- renderText({
    if (input$regions == "Region 1") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=1", "Jo Davies, Stephenson, et al.")
    } else if (input$regions == "Region 2") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=2", "Rock Island, Henry, Bureau, Putnam, et al.")
    } else if (input$regions == "Region 3") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=3", "Hancock, Adams, Pike, et al.")                    
    } else if (input$regions == "Region 4") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=4", "Bond, Madison, St. Clair, et al.")
    } else if (input$regions == "Region 5") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=5", "Marion, Jefferson, Wayne, et al.")
    } else if (input$regions == "Region 6") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=6", "Iroquois, Ford, Dewitt, et al.")
    } else if (input$regions == "Region 7") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=7", "Will, Kankakee")
    } else if (input$regions == "Region 8") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=8", "Kane, Dupage")
    } else if (input$regions == "Region 9") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=9", "McHenry, Lake")
    } else if (input$regions == "Region 10") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=10", "Cook")
    } else {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=11", "Chicago")
    } 
    paste(strong("You are in the region: "), regions)
  })
  
  reac_hist_date <- reactive({
    NYTimes_US_Historical_Data %>% filter(date == input$date)
  })
  reac_cm_date <- reactive({
    NYTimes_US_Historical_Data %>% filter(date <= input$date)
  })
  output$cm_plot <- renderPlot({
    par(mfrow = c(2, 1))
    plot(x = reac_cm_date()$date, y = reac_cm_date()$cases, ylab = "Cumulative Cases", xlab = "Date", pch = 20, col = "#0455A4")
    plot(x = reac_cm_date()$date, y = reac_cm_date()$deaths, ylab = "Cumulative Deaths", xlab = "Date", pch = 20, col = "#E84A27")
    plot(x = reac_cm_date()$date, y = reac_cm_date()$deaths/reac_cm_date()$cases, ylab = "Cumulative Deaths Ratio", xlab = "Date", pch = 20, col = "black")
  })
  output$case_count <- renderText({
    paste0(prettyNum(reac_hist_date()$cases, big.mark=","), " cases in the US as of ",input$date)
  })
  output$death_count <- renderText({
    paste0(prettyNum(reac_hist_date()$deaths, big.mark=","), " deaths in the US as of ",input$date)
  })
  output$stateplot = renderPlot({
    plot(x = as.Date(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)[2:length(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)], y = diff(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$cases), ylab = "New Cases", xlab = "Date", main = paste("Number of New COVID-19 Cases Per Day in ", as.character(input$state1), sep = ""), type = "l", col="#048732")
  })
  
  output$info1 <- renderText({
    xy_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0("New Cases = ",round(as.numeric(e$y),1),"\n")
    }
    xy_range_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0(" The Lowest cases over that period = ", round(as.numeric(e$ymin), 1), " The highest cases over that period = ", round(as.numeric(e$ymax), 1))
    }
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
    
  })
  output$stateplot2 = renderPlot({
    plot(x = as.Date(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)[2:length(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)], y = diff(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$deaths), ylab = "New Death", xlab = "Date", main = paste("Number of New COVID-19 Deaths Per Day in ", as.character(input$state1), sep = ""), type = "l", col="#ddb3ff")
  })
  
  output$info2 <-renderText({
    xy_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0("New Death=",round(as.numeric(e$y),1),"\n")
    }
    xy_range_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0(" The Lowest new death over that period=", round(as.numeric(e$ymin), 1), " The highest new death over that period=", round(as.numeric(e$ymax), 1))
    }
    paste0(
      "click: ", xy_str(input$plot_click2),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
    
  })
  #calculating average increase in cases globally and storing values in growth_rate_global
  confirmed_cases_global <- c(WHO_COVID_19_Situation_Report_Data[1,])
  growth_rate_global <- c()
  for (i in 5:154) {
    growth_rate_global <- c(growth_rate_global,(abs(as.numeric(confirmed_cases_global[i+1]) - as.numeric(confirmed_cases_global[i]))))
  }
 
  # Plotting deaths per cases by county
  data_county <- as.data.frame(NYTimes_US_Counties_Historical_Data)
  
  output$deaths_by_county <- renderPlot ({
    
    our_data <- data_county()
    
    barplot(colSums(our_data[,c("deaths","county")]),
            ylab="Deaths",
            xlab="County",
            names.arg = c("Deaths", "County"),
            col = color)
  })
  
  #order the number of cases for each state by date and calculate death/cases ratio by day,
  #stored in new$ratio. highest ratio is found by state and sorted in peak_sorted
  usstates <- NYTimes_US_States_Historical_Data
  new <- usstates[order(usstates$state),]
  new$ratio <- new$deaths/new$cases
  peak_sorted <- new %>% group_by(state) %>% filter(ratio == max(ratio))
  
  #national deaths/cases
  new_national <- NYTimes_US_Historical_Data
  new_national$ratio <- new_national$deaths/new_national$cases
  
  #comparing latest ratio of deaths/cases by state and in the entire country
  latest_ratio_states <- new %>% filter(date==max(date))
  latest_ratio_national <- new_national %>% filter(date==max(date))
  comparison_latest_by_states <- ifelse(latest_ratio_states$ratio >= latest_ratio_national$ratio, 1, 0)
  combined_latest_national <- cbind(latest_ratio_states, comparison_latest_by_states)
  
  
  #plot the states' latest deaths/cases ratio
  output$latestplot <- renderPlot({
    plot_latest <- plot_usmap("states", data = combined_latest_national, values = "ratio", color = "black", labels = TRUE) +
      scale_fill_continuous(low = "grey", high = "#E84A27", name = "Latest Deaths/Cases Ratio") +
      labs(title = "Latest Deaths/Cases Ratio") + theme(legend.position = "left", panel.background = element_rect(color = "black", fill = "white")) +
      scale_fill_continuous(low = "grey", high = "#E84A27")
    return(plot_latest)
  })
  
  #plot the states' total cases
  output$caseplot <- renderPlot({
    data1 =  NYTimes_US_States_Historical_Data %>% filter(date == input$date)
    plot_case <- plot_usmap("states", data = data1[, c(3,4)], values = "cases", color = "black", labels = TRUE) +
      scale_fill_gradient(low = "white", high = "#0455A4", na.value = NA, name = "# of cases by state") +
      labs(title = "Cases by states") + theme(legend.position = "left") +
      guides(color = guide_legend(order = 1)) +
      labs(fill = "# of cases by state")
    return(plot_case)
  })
  
  #Plotting by county
  output$mapplot <- renderPlot({
    if(input$state_map == "All states"){
      data = NYTimes_US_Counties_Historical_Data %>% filter(date == input$date)
    }
    else{
      data = NYTimes_US_Counties_Historical_Data %>% filter(date == input$date & state == input$state_map)
    }
    vc_states <- unique(state.abb[match(data$state, state.name)])
    p <- plot_usmap(regions = "counties",
                    include = vc_states,
                    data = data[, c(4, 6)], values = "deaths") +
      labs(title = ifelse(input$state_map == "All states", "US Counties", vc_states),
           subtitle = paste0("Shows all counties in ", input$state_map)) +
      theme(panel.background = element_rect(color = "black", fill = "white")) +
      scale_fill_continuous(low = "yellow", high = "#0000FF", na.value = "#FFFFFF")
    return(p)
  })
  
  # Plotting deaths per cases by county
  
  # Preparing Dataset
  new_counties <- NYTimes_US_Counties_Historical_Data
  new_counties$ratio <- new_counties$deaths/new_counties$cases
  
  #Plotting deaths per month
  output$deaths_per_month <- renderPlot({
    new <- ggplot(data(), aes(y=deaths, x=month))
    new + geom_bar(stat = "sum", fill = "#E84A27") +  labs(x = "Month", y = "Deaths") + theme(text = element_text(size = 25), legend.position = "none") + 
      geom_text(aes(label=deaths), vjust = 1, color = "black", size = 4, position = position_dodge(.8)) + 
      scale_x_date(date_labels = "%b", breaks = date_breaks("1 month"))
    #paste("New Cases=", round(as.numeric(input$deaths_per_month$y)))
  })
  
  
  
  #Plotting total cases, new cases rate and death cases rate by WHO Region
  region_data = WHO_Global_Historical_Data %>%
    mutate(Region = case_when(WHO_region == "AFRO" ~ "Africa",
                              WHO_region == "EURO" ~ "Europe",
                              WHO_region == "EMRO" ~ "Eastern Mediterranean",
                              WHO_region == "WPRO" ~ "Western Pacific",
                              WHO_region == "AMRO" ~ "America",
                              WHO_region == "SEARO" ~ "South-East Asia",
                              TRUE ~ "Other")) %>%
    filter(Region != "Other") %>%
    group_by(Region, Date_reported) %>%
    summarise(New_cases = sum(New_cases),
              Cumulative_cases = sum(Cumulative_cases),
              New_deaths = sum(New_deaths),
              Cumulative_deaths = sum(Cumulative_deaths),
              New_cases_rate = sum(New_cases) / sum(Cumulative_cases),
              New_deaths_rate = sum(New_deaths) / sum(Cumulative_deaths))
  
  options(scipen=10000) #changing scaling from scientific to standard form
  output$plot_region = renderPlot({
    ggplot(data = region_data, aes(x = Date_reported)) +
      geom_vline(xintercept = input$date2, color = "Blue", size = 2) +
      geom_smooth(method = "loess",se = FALSE, aes(y = eval(parse(text = input$type))), color = "#E84A27") +
      facet_wrap( ~ Region) +
      labs(x = "Date", y = "Number of Cases") +
      theme(text = element_text(size=20), axis.text.x=element_text(angle=45, hjust=1))
  })
  
  
  #global tab world colored maps
  output$plot_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    mapParams <- mapCountryData(malMap, nameColumnToPlot="Cumulative_cases", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE)
    #updated more specific legend
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    #label above legend
    mtext("Cases in Thousands",side=1,line=1.5)
  })
  
  
  output$new_cases_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    colourPalette <- brewer.pal(5,'YlGn')
    
    mapParams <- mapCountryData(malMap, nameColumnToPlot="New_cases", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE,colourPalette=colourPalette)
    
    #updated more specific legend
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    #label above legend
    mtext("Cases in Thousands",side=1,line=1.5)
  })
  
  output$new_cases_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    mapParams <- mapCountryData(malMap, nameColumnToPlot="New_deaths", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE)
    
    #updated more specific legend
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    #label above legend
    mtext("Cases in Thousands",side=1,line=1.5)
  })
  
  output$death_plot_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    colourPalette <- brewer.pal(6,'RdPu')
    mapParams <- mapCountryData(malMap, nameColumnToPlot="Cumulative_deaths", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE,colourPalette=colourPalette )
    #updated more specific legend
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    #label above legend
    mtext("Deaths in Thousands",side=1,line=1.5)
  })
  
  #global tab text box at top left explaining top right graphs  
  output$cases_count_region = renderUI({
    text = NULL
    for (i in 1:6) {
      region = paste(group_data(region_data)[i,1])
      new = paste0(region, ": <strong>", region_data[which(region_data$Date_reported == input$date[2] & region_data$Region == region), input$type], "</strong> cases", "<br />")
      text = paste(text, new)
    }
    HTML(text)
  })
  
  
  
  
  
  
  # Plotting Recovered_rate and Death_rate and Positive rate per day (US)
  
  
  output$recovered_death_rate = renderPlot({
    
    df2 = COVID_Tracking_Project_US_Historical_Data %>% select(c('date', 'recovered', 'positive','death', 'totalTestResults','totalTestResultsIncrease','positiveIncrease'))
    df2$recovered_rate = df2$recovered/df2$positive
    df2$date = df2$date
    df2$death_rate = df2$death/df2$positive
    df2$date = df2$date
    temp_data = df2 %>% 
      select(c("date", "recovered_rate","death_rate")) %>%
      filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
      gather(key = "variable", value = "value", -date) 
    
    ggplot(temp_data, aes(x=date, y=value)) + 
      geom_line(aes(color = variable, linetype = variable)) + 
      scale_color_manual(values = c("blue", "red")) +
      theme(plot.background = element_blank(), 
            legend.position = "bottom") 
    df2$deathvsrecover = df2$death/df2$recovered
    df2$date = df2$date
    
    df2$positive_rate = df2$positiveIncrease / df2$totalTestResultsIncrease
    
    ## assign color accordingly to different input lines
    
    if (input$type_rate == "recovered_rate") {
      temp_data = df2 %>%
        select(c("date", "recovered_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("#E84A27")
    }
    
    if (input$type_rate == "death_rate") {
      temp_data = df2 %>%
        select(c("date", "death_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("#0455A4")
    }
    
    if (input$type_rate == "deathvsrecover") {
      temp_data = df2 %>%
        select(c("date","deathvsrecover")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("red")
    }
    
    if (input$type_rate == "positive_rate") {
      temp_data = df2 %>%
        select(c("date", "positive_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("green")
    }
    
    if (input$type_rate == "all") {
      temp_data = df2 %>%
        select(c("date", "recovered_rate","death_rate", "deathvsrecover", "positive_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("#E84A27", "#0455A4", "red", "green")
    }
    
    ggplot(temp_data, aes(x=date, y=value)) +
      geom_line(aes(color = variable, linetype = variable)) +
      scale_color_manual(values = colorlines) +
      scale_y_continuous(labels = scales::percent)+
      theme(plot.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = '#E84A27',size=rel(1.5)),
            panel.grid.major.y = element_line(colour="aliceblue"),
            panel.background = element_blank())+
      labs(x = "Date", y = "",
           title = paste("Recovered Rate & Death Rate (US) between",
                         as.character(input$Rate[1]),
                         "and",
                         as.character(input$Rate[2]),
                         sep = " ")) +
      scale_x_date(breaks = date_breaks("months"),
                   labels = date_format("%Y-%m-%d"))
    
  })
  

  
  
  output$testplot <- renderPlot({
    rec_t <- COVID_Tracking_Project_US_Historical_Data %>%
      select(date, positiveIncrease, totalTestResultsIncrease) %>%
      filter(date >= input$date1) %>%
      gather(key = "variables", value = "value", -date)
    
    ggplot(rec_t, aes(x = date, y = value)) +
      geom_area(aes(color = variables, fill = variables),
                alpha = 0.5, position = position_dodge(0.8)) +
      scale_color_manual(values = c("#FF6600", "#E7B800")) +
      scale_fill_manual(values = c("#FF6600", "#E7B800"))+
      theme(plot.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text=element_text(size=rel(1.5)),
            panel.grid.major.y = element_line(colour="cornsilk3"),
            panel.background = element_blank())+
      ylab("Cases")+
      labs(title = "US Daily Increase of Total test Result Cases & Positive Cases")
  })
  
  #Create a new variable counting the increase of recovered number per day
  national <- COVID_Tracking_Project_US_Historical_Data
  national <- arrange(national, date) 
  national <- mutate(national, recovered_increase = -(lag(national$recovered,1) - national$recovered))
  #Add a new variable of month
  national <- mutate(national, month = format(as.Date(date),"%Y-%m")) 
  
  sum <- as.data.frame(summarise(group_by(national,month),sum(recovered_increase)))
  sum$`sum(recovered_increase)` <- ifelse(is.na(sum$`sum(recovered_increase)`) == TRUE, 0, sum$`sum(recovered_increase)`)
  
  #plot the monthly increase of recovered number
  output$recoveredplot <- renderPlot({ggplot(sum,aes(x = month, y = `sum(recovered_increase)`,group = 1)) +
      geom_line(col = "dodgerblue") + 
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="Number of Recovered", title = "Monthly Increase of Recovered Number (Up to 2020-12-02)")
  })
  
  #plot negativeIncrease per day 
  output$negativeIncrease <- renderPlot({
    datanegative = COVID_Tracking_Project_US_Historical_Data %>% filter(date == input$date)
    plot_negative <- plot(negativeIncrease,date)
    return(plot_negative)
  })
  
  output$Rsquared = renderUI({
    drop_list = case_when(input$Curve == "pos" ~ c("hospitalizedCurrently","onVentilatorCurrently"),
                          input$Curve == "hosp" ~ c("positiveIncrease","onVentilatorCurrently"),
                          input$Curve == "vent" ~ c("hospitalizedCurrently","positiveIncrease"))
    
    df3 = COVID_Tracking_Project_US_Historical_Data %>%
      select(c('date', 'deathIncrease','hospitalizedCurrently', 'onVentilatorCurrently','positiveIncrease')) %>%
      na.omit() %>%
      filter((date >= input$drange[1]) & (date <= input$drange[2])) %>%
      mutate(date2=date+input$Lag) %>%
      select(-(drop_list))
    
    df_a <- df3[1:2]                                                          #Split then recombine with aligned dates
    df_a[2] <- scale(df_a[2])                                                 #Scale (normalize) variables
    df_b <- df3[3:4]                                                          #Columns with the numeric data only
    df_b[1] <- scale(df_b[1])
    df3 <- inner_join(df_a, df_b, by=c("date"="date2"))
    
    R2 <- cor(df3[2],df3[3])^2                                                #Calculate the R2 = corr^2
    
    text = "The R squared for your choices is"
    text = paste(text, "<strong>", round(R2,3), "</strong>")
    HTML(text)
  })
  
  #Champaign County Data
  NYTimes_US_Counties_Historical_Data$recoveries = NYTimes_US_Counties_Historical_Data$cases - NYTimes_US_Counties_Historical_Data$deaths
  output$champaign_cases = renderPlot({
    ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
    
    ggplot(ccd2, aes(x=date, y=cases))+
      geom_line() +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="Cumulative Cases", title = "Champaign County Cumulative Covid Cases")
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      cat("Cumulative Covid Cases :\n")
      str(input$plot_hover$y)
    }
  })

  
  
  output$champaign_deaths = renderPlot({
    ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
    
    ggplot(ccd2, aes(x=date, y=deaths))+
      geom_line() +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="Cumulative Deaths", title = "Champaign County Cumulative Covid Deaths")
  })
  
  output$champaign_recoveries = renderPlot({
    ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
    
    ggplot(ccd2, aes(x=date, y=recoveries))+
      geom_line() +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="Cumulative Cases", title = "Champaign County Cumulative Covid Cases")
  })
  
  
  
  
  
  output$Ventilator_Usage = renderPlot({
    
    df3 = COVID_Tracking_Project_US_Historical_Data %>% 
      select(c('date', 'hospitalizedCurrently', 'onVentilatorCurrently')) %>%
      na.omit() %>%
      mutate(ventilator_rate = onVentilatorCurrently/hospitalizedCurrently)  %>%
      filter((date >= input$Ventilator[1]) & (date <= input$Ventilator[2])) 
    
    colors <- c("Max Ventilator Rate" = "firebrick2")
    
    ggplot(data = df3, mapping = aes(x = date, y = ventilator_rate)) + 
      theme_bw() +
      theme(legend.title=element_blank(), legend.position = c(0.9, 0.9)) +
      theme(legend.background = element_rect(size=0.5, linetype="solid", color="gray50")) +
      
      labs(x="Date", y="Ventilator Usage Rate") +
      
      geom_line(color="darkblue",size=1.5) + 
      scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      geom_point(data=slice_max(df3,df3$ventilator_rate),aes(color="Max Ventilator Rate"), size=5) +      #Max point
      annotate("label", x = df3$date[which.max(df3$ventilator_rate)]+3, y=df3$ventilator_rate[which.max(df3$ventilator_rate)], 
               label = round(df3$ventilator_rate[which.max(df3$ventilator_rate)],2), hjust = 0, 
               color="gray50") +
      
      geom_hline(yintercept=mean(df3$ventilator_rate),linetype="dashed", color="gray50") +                #Mean line
      annotate("label", x = df3$date[which.min(df3$date)], 
               y = mean(df3$ventilator_rate),label = "Average Ventilator Rate", hjust = 0, 
               color="gray50", fill="white", label.size=NA) 
    
  })
  
  
  ranges <-reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
  })
  
  observeEvent(input$plot1_dblclick,{
    brush <-input$plot1_brush
    if (!is.null(brush)){
      ranges$x <-c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
      geom_point()
  })
  
  output$plot3 <- renderPlot({
    ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  
  output$lol <- renderPlot({
    
    date_range = input$date_range_covid
    data = Champaign_data %>% filter(date_range[1] < date & date < date_range[2])
    type = data$new_cases
    color = "new_cases"
    
    if(input$Graph_Type == "New Cases") {
      type = data$new_cases
      color = "new_cases"
    }
    else if (input$Graph_Type == "Total Cases"){
      type = data$cases
      color = "cases"
    }
    else if (input$Graph_Type == "New Deaths"){
      type = data$new_deaths
      color = "new_deaths"
    }
    else {
      type = data$deaths
      color = "deaths"
    }
    
    ggplot(data, aes(x = `date`)) +
      geom_line(aes(y = type, color = color), size = 1)+
      scale_color_manual("",values = "deepskyblue4")+
      labs(y= paste("Number of ",color,sep=""), x = "Date")+
      theme(legend.position = "none",
            panel.background = element_rect(fill="black",colour = "Black"),
            panel.grid.major = element_line(colour = "sienna3",size = .5),
            panel.grid.minor = element_blank(),
            plot.subtitle = element_text(size = 10))
  }  
  )
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  #plotting estimated number of current cases
  output$county <- renderPlot({
    max.cases = NYTimes_US_Counties_Historical_Data %>% group_by(county, state) %>% slice(which.max(cases))
    cases.2weeks = filter(NYTimes_US_Counties_Historical_Data, date < today() - 14)
    max.cases.2weeks = cases.2weeks %>% group_by(county, state) %>% slice(which.max(cases))
    twoweeks = inner_join(max.cases, max.cases.2weeks, by = c("county", "state"))
    twoweeks$CASES = twoweeks$cases.x - twoweeks$cases.y
    names(twoweeks)[names(twoweeks) == "fips.x"] = "fips"
    if(input$state.input == "All States"){
      countycases = twoweeks
    }
    else{
      countycases = twoweeks %>% filter(state == input$state.input)
    }
    states <- unique(state.abb[match(countycases$state, state.name)])
    plot <- plot_usmap(regions = "counties",
                       include = states, labels = ifelse(input$state.input == "All States", FALSE, TRUE),
                       data = countycases[, c(4, 11)], values = "CASES") +
      theme(panel.background = element_rect(color = "black", fill = "white")) +
      scale_fill_continuous(low = "yellow1", high = "firebrick4", na.value = "snow2") +
      labs(title = ifelse(input$state.input == "All States", "All Counties", states),
           subtitle = paste0("Number of Cases in Past 14 Days in ", input$state.input), fill = "Estimated Current Cases") 
    plot$layers[[2]]$aes_params$size <- 3
    return(plot)
  })
  
  #output the daily growth rate of cases in Champaign area with the selected time range
  output$champaign_growth <- renderPlot({
    datas = NYTimes_US_Counties_Historical_Data %>%
      filter(
        county == "Champaign",
        state == "Illinois",
        as.Date(date) < input$champaign_growth_date
      ) %>%
      select(cases, date)
    datas$date = as.Date(datas$date)
    temp = c(0)
    
    datas$date = as.Date(datas$date)
    
    temp = c()
    
    for (i in 1:length(datas$cases)) {
      temp[i] = (datas$cases[i + 1] - datas$cases[i]) / datas$cases[i]
    }
    
    datas$rate = temp
    ggplot(data = datas) +
      geom_line(mapping = aes(x = date, y = rate)) +
      scale_x_continuous(breaks = seq(min(datas$date), max(datas$date), by = "1 weeks"), name = "Date") +
      scale_y_continuous(
        breaks = seq(0, 1, by = 0.25),
        labels = scales::percent_format(accuracy = 0.1),
        name = "Daily Growth Rate"
      )
    
    
  })
  
  #output the cases of Champaign at selected date
  output$champaign_cases_search <- renderText({
    cas = NYTimes_US_Counties_Historical_Data %>% filter(county == "Champaign", state == "Illinois") %>% select(cases, date)
    out <-
      paste("At",
            input$champaignCasesSearch_Input,
            "there are",
            cas[which(cas$date == input$champaignCasesSearch_Input), ]$cases,
            "cases in Champaign area")
    
  })
  #plot current hospitalize
  output$hos <- renderPlot({
    ggplot(data = COVID_Tracking_Project_US_Historical_Data, aes(x=date, y=hospitalizedCurrently))+
      geom_line() +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="hospitalizedCurrently", title = "Current Hospitalize patients")
  })
  
  output$percent_pos_state = renderText({
    population = read.csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-popchg2010_2019.csv")
    state.pop = population %>% slice(which(NAME %in% state.name))
    
    covid = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
    statecases = covid %>% group_by(state) %>% slice(which.max(cases)) %>% slice(which(state %in% state.name))
    which(statecases$state %in% state.name)
    state.percent = data.frame(statecases$state, statecases$cases / state.pop$POPESTIMATE2019 * 100)
    names(state.percent)[names(state.percent) == "statecases.cases.state.pop.POPESTIMATE2019...100"] = "percent"
    
    
    percentpop = function(state){
      states = which(state.name == state)
      a = statecases[states, 4]
      b =  state.pop[states, 16]
      paste(round(a[[1]] / b, 2) * 100, "% of", state, "has tested positive for COVID-19")
    }
    
    
    percentpop(input$state0)
  })
  
  output$testing_site = renderText({
    
    get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
      loadNamespace("purrr")
      loadNamespace("geosphere")
      longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
      longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
      distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
      distance_m = sapply(distance_list, function(col) { col[1] })
      
      if (units == "km") {
        distance = distance_m / 1000.0;
      }
      else if (units == "miles") {
        distance = distance_m / 1609.344
      }
      else {
        distance = distance_m
      }
      distance
    }
    # https://www.latlong.net/
    
    lat = input$lat
    long = 	input$long
    lat_long = c(lat,long)
    
    testing_sites = data.frame(site = c("CRCE", "Illini Union", "State Farm Center", "SDRP", "Vetinary Medicine"),
                               long = c(40.104141, 40.10939235	, 40.0962421, 40.10409, 40.101877),
                               lat = c(-88.221538, -88.2272187093397, -88.2359287628109, -88.2394624, -88.219142))
    
    
    
    distances = map2_dbl(testing_sites$long, testing_sites$lat, ~get_geo_distance(.x, .y, lat_long[1], lat_long[2]))
    
    paste(testing_sites[which.min(distances), 1], "is the closest testing site to your address.")
  })
  
  
  url = a("Click Here", href= "https://stevemorse.org/jcal/latlon.php")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  
  
}

shinyApp(ui = ui, server = server)
