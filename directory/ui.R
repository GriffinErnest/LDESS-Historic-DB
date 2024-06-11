library(curl)
library(jsonlite)
library(tidyr)
library(readxl)
library(shinydashboard)
library(shiny)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(RSQLite)
library(plotly)
library(shinyauthr)
library(shinycssloaders)
library(openxlsx)
library(DT)


#this line alligns your working directory with the file path but can't be used for an application hosted on shinyapps
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####for testing
ui <- dashboardPage(
  
  dashboardHeader(title="EXTEND LDESS 1 Data"
  ),
  dashboardSidebar(width = 300,
                    
                    sidebarMenu(menuItem(" Hot Water Battery", tabName = "HWB", icon = icon("shower")),
                                menuItem(" Space Heating Battery", tabName = "SHB", icon = icon("house-fire")),
                                menuItem(" Heat Load", tabName = "Heat_Load", icon = icon("gauge")),
                                menuItem(" Operation Mode", tabName = "OP_MO", icon = icon("table-list")),
                                
                                
                                id="selectedTab"),
                 
                    # Add a logout button
                    
                    #id = "contenta",
                    selectInput(
                      inputId = "HoursBack",
                      label = "Hours:",
                      selected = "5",
                      choices = c(2:24)
                    ),
                    
                    # selectInput(
                    #   inputId = "YearMerge",
                    #   label = "Year:",
                    #   selected = "2024",
                    #   choices = c("2024")
                    # ),
                    # selectInput(
                    #   inputId = "MonthsMerge",
                    #   label = "Month",
                    #   selected = "1",
                    #   choices = c(1:12)),
                    # fluidRow(
                    #   column(5,
                    #          actionButton("Month_back", "Back Month",width = 120)),
                    #   column(5,
                    #          actionButton("Month_forward", "Forward Month",width = 120))),
                   
                      selectInput(
                      inputId = "EXTEND_ID",
                      label = "EXTEND_ID:",
                      choices = c("LDESS_1_Macmerry")
                      
                    ),
                    sliderInput( inputId = "CyclesMerge",
                                 label = "Time filter (1=include all)",
                                 min = 0,
                                 max = 1,
                                 value= c(0,1),
                                 step =.01), 
                   actionButton("refreshButton", "Refresh", icon = icon("refresh")),
                   downloadButton("downloadXLS", "Download XLS", icon = icon("download"))
                    
  ),
  
  dashboardBody(
    
    tabItems(
      #HOT WATER BATTERY
      tabItem(tabName = "HWB",
              fluidRow(box(
                column(
                  selectInput("metric", "Metrics for 24h plot:", choices = c("HWB_TSB","HWB_TSM","HWB_TST","HWB_SOC_pct"),
                              selected =  c("HWB_TSB","HWB_TSM","HWB_TST","HWB_SOC_pct"),
                              multiple = TRUE),width = 5),
                # column(
                #   selectInput("HWB_TSFS", "Metrics for Hydronics plot:", choices = c("HS_TS4","HS_TS5","FS3_FS"),
                #               selected =  c("HS_TS4","HS_TS5","FS3_FS"),
                #               multiple = TRUE), 
                  # width = 5 ),
                column(
                    selectInput("HWB_PM", "Metrics for Power usage plot:", choices = c("PM1_PF","PM1_I","PM1_V","PM1_PWH","PM1_PVA","PM1_W","PM1_FR"),
                                selected =  c("PM1_W"),
                                multiple = TRUE),
                  width = 5 ),
                height =100, width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("HWB_top", height = 350)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("HWB_bottom", height = 350)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("HWB_bottomid", height = 350)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("HWB_bottomer", height = 350)),width = 12))
              
      ),
      
      #SPACE HEATING BATTERY
      tabItem(tabName = "SHB",
              fluidRow(box(
                column(
                  selectInput("SHB_TM", "SOC Metrics:", choices = c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG","CHB_SOC_pct"),
                              selected =  c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG","CHB_SOC_pct"),
                              multiple = TRUE),width = 5),
                column(
                  selectInput("SHB_BM", "Metrics for Hydronics plot:", choices = c("HS_TS10","HS_TS11","HS_TS12","FS5_FS"),
                              selected =   c("HS_TS10","HS_TS11","HS_TS12","FS5_FS"),
                              multiple = TRUE), 
                  width = 5 ),height =100, width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("SHB_top", height = 350)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("SHB_bottom", height = 350)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("SHB_bottomer", height = 350)),width = 12))
      ),
      #OPERATIONAL MODE
      tabItem(tabName = "OP_MO",
              box(title = "Operation Mode", status = "primary", height = "1200",width = "12",solidHeader = T, 
                  DT::dataTableOutput("OP_mode_table"),style = "height:1000px; overflow-y: scroll; overflow-x: scroll;")),
      #Heat load
      tabItem(tabName = "Heat_Load",
              fluidRow(box(withSpinner(plotlyOutput("HeatLoad", height = 450)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("PowerPlot", height = 250)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("SOCPlot", height = 250)),width = 12))
              )
    
  )
  )
  )
