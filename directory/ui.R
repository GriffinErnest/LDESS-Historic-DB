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

library(DBI) #connecting to sql database


#this line alligns your working directory with the file path but can't be used for an application hosted on shinyapps
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####for testing
ui <- dashboardPage(
  
  dashboardHeader(title="EXTEND Historic Data"
                  
  ),
  dashboardSidebar(width = 300,
                    selectInput(
                      inputId = "EXTEND_ID",
                      label = "EXTEND_ID:",
                      selected = "EXT0007",
                      choices = c("EXT0007",
                                  "EXT0008")
                      
                    ),
                   sidebarMenu(menuItem(" Hot Water Battery", tabName = "HWB", icon = icon("shower")),
                                menuItem(" Space Heating Battery", tabName = "SHB", icon = icon("house-fire")),
                                menuItem(" Heat Load", tabName = "Heat_Load", icon = icon("gauge")),
                                menuItem(" Operation Mode", tabName = "OP_MO", icon = icon("table-list")),
                                id="selectedTab"),
                   dateRangeInput("dates",
                                  "Date range",
                                  start = Sys.Date()-days(7),
                                  end = Sys.Date()#,
                                  # min = Sys.Date() - 7,  # Minimum date (7 days ago)
                                  # max = Sys.Date()
                                  ),
                   #BOOKMARK FOR NOW textOutput("DateRange"),
                   #week toggle
                   fluidRow(
                     column(5,
                            actionButton("Week_back", "Back Week",width = 120)),
                     column(5,
                            actionButton("Week_forward", "Forward Week",width = 120))),
                   #day toggle
                   fluidRow(
                     column(5,
                            actionButton("Day_back", "Back Day",width = 120)),
                     column(5,
                            actionButton("Day_forward", "Forward Day",width = 120))),
                   fluidRow(
                    sliderInput( inputId = "CyclesMerge",
                                 label = "Time filter (1=include all)",
                                 min = 0,
                                 max = 1,
                                 value= c(0,1),
                                 step =.01)), 
                   # fluidRow(
                   #   column(5,
                   #          actionButton("Refresh_call", "Refresh", icon = icon("refresh"), width = 120)
                   #          )
                   #   ),
                   
                   
                  
                  
                   fluidRow(
                     column(5,
                            downloadButton("downloadXLS", "Download XLS", icon = icon("download"), width = 120, style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 15px 15px 15px 15px; ")
                     ))
                    
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
                    selectInput("HWB_PM", "Metrics for Power usage plot:", choices = c("PM3_PF","PM3_I","PM3_V","PM3_PWH","PM3_PVA","PM3_FR"),
                                selected =  c("PM3_PVA"),
                                multiple = TRUE),
                  width = 5 ),
                height =100, width = 12)),
              conditionalPanel(condition= "input.metric.length > 0",
              fluidRow(box(withSpinner(plotlyOutput("HWB_top", height = 350)),width = 12))
              ),
              fluidRow(box(withSpinner(plotlyOutput("HWB_bottom", height = 350)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("HWB_bottomid", height = 350)),width = 12)),
              conditionalPanel(condition= "input.HWB_PM.length > 0",
                fluidRow(box(withSpinner(plotlyOutput("HWB_bottomer", height = 350)),width = 12))               
              )
              
              
      ),
      
      #SPACE HEATING BATTERY
      tabItem(tabName = "SHB",
              fluidRow(box(
                column(5,
                  fluidRow(
                  selectInput("SHB_TM", "SOC Metrics:", choices = c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG","CHB_SOC_pct"),
                              selected =  c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG","CHB_SOC_pct"),
                              multiple = TRUE)),
                              
                  fluidRow(
                    selectInput("SHB_BMid", "Space Heating Metrics:", choices = c("HS_TS6","HS_TS7","HS_TS8","HS_TS9","ROOM_TS19"),
                                selected =  c("HS_TS6","HS_TS7","HS_TS8","HS_TS9"),
                                multiple = TRUE)),
                    ),
                column(5,
                  fluidRow(
                  selectInput("SHB_BM", "Metrics for Hydronics plot:", choices = c("HS_TS10","HS_TS11","HS_TS12","FS5_FS"),
                              selected =   c("HS_TS10","HS_TS11","HS_TS12","FS5_FS"),
                              multiple = TRUE)), 
                  fluidRow(
                    selectInput("SHB_BB", "Metrics for Power usage plot:", choices = c("PM2_I","PM2_PW","PM2_PVA","PM2_PWH","PM2_FR","PM2_PF","PM2_V"),
                                selected =   c("PM2_PVA"),
                                multiple = TRUE)),
                  ),
                  
                  height =150, width = 12)),
              conditionalPanel(condition= "input.SHB_TM.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_top", height = 350)),width = 12))),
              conditionalPanel(condition= "input.SHB_BM.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_bottom", height = 350)),width = 12))),
              conditionalPanel(condition= "input.SHB_BMid.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_bottomid", height = 350)),width = 12))),
              conditionalPanel(condition= "input.SHB_BB.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_bottomer", height = 350)),width = 12)))
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
