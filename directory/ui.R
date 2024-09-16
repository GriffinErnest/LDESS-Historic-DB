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
library(numbers) #for mod function

library(DBI) #connecting to sql database



#this line alligns your working directory with the file path but can't be used for an application hosted on shinyapps
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####for testing
ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title=tags$img(src = "Extend.png", width = "200px", height = "auto")
                  # , #adds the sunamp logo in the top RHS
                  # tags$li(a(href = 'https://sunamp.com/en-gb/',
                  #           img(src = 'Sunamp.png',
                  #               title = "Company Home", height = "30px"),
                  #           style = "padding-top:10px; padding-bottom:10px;"),
                  #         class = "dropdown")
  ),
  dashboardSidebar(width = 300, 
                   
                   
                   
                   
                   
                    selectInput(
                      inputId = "EXTEND_ID",
                      label = "EXTEND_ID:",
                      selected = "EXT0006",
                      choices = c("EXT0006",
                                  "EXT0007"#,
                                  #"EXT0008"
                                  )
                      
                    ),
                   
                   conditionalPanel(
                     condition = ("input.EXTEND_ID == 'EXT0006'"),
                     
                     sidebarMenu(menuItem(" Hot Water Battery", tabName = "HWB", icon = icon("shower")),
                                 menuItem(" Space Heating Battery", tabName = "SHB", icon = icon("house-fire")),
                                 menuItem(" Heat Load", tabName = "Heat_Load", icon = icon("gauge")),
                                 menuItem(" Operation Mode", tabName = "OP_MO", icon = icon("table-list")),
                                 menuItem(" Operating Schedule", tabName = "TOU", icon = icon("money-bill-trend-up")),
                                 menuItem(" Hydronics", tabName = "Hydronics", icon = icon("wrench")),
                                 menuItem(" Master Graph", tabName = "Mastergraph", icon = icon("compass-drafting")),
                                 id="selectedTab")
                   ),
                   conditionalPanel(
                     condition = ("input.EXTEND_ID == 'EXT0007'"),
                     
                     sidebarMenu(menuItem(" Hot Water Battery", tabName = "HWB", icon = icon("shower")),
                                 menuItem(" Space Heating Battery", tabName = "SHB", icon = icon("house-fire")),
                                 menuItem(" Heat Load", tabName = "Heat_Load", icon = icon("gauge")),
                                 menuItem(" Operation Mode", tabName = "OP_MO", icon = icon("table-list")),
                                # menuItem(" Operating Schedule", tabName = "TOU", icon = icon("money-bill-trend-up")),
                                 id="selectedTab")
                   ),
                   # sidebarMenu(menuItem(" Hot Water Battery", tabName = "HWB", icon = icon("shower")),
                   #              menuItem(" Space Heating Battery", tabName = "SHB", icon = icon("house-fire")),
                   #              menuItem(" Heat Load", tabName = "Heat_Load", icon = icon("gauge")),
                   #              menuItem(" Schedule", tabName = "TOU", icon = icon("money-bill-trend-up")),
                   #             menuItem(" Hydronics", tabName = "Hydronics", icon = icon("wrench")),
                   #              menuItem(" Master Graph", tabName = "Mastergraph", icon = icon("compass-drafting")),
                   #              menuItem(" Operation Mode", tabName = "OP_MO", icon = icon("table-list")),
                   #              id="selectedTab"),
                   dateRangeInput("dates",
                                  "Date range",
                                  start = Sys.Date()-days(1),
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
                     column(10,
                      actionButton("datapull","Run Query:",icon = icon("refresh") )
                      
                            )
                   ),
                   fluidRow(
                    sliderInput( inputId = "CyclesMerge",
                                 label = "Time filter (Use this for zooming into all the data at once)",
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
                     )),
                   #overide
                   p("When queries are for data larger then 24 hours, data is removed from the plots to speed up their loading"),
                   fluidRow( 
                     column(10,
                            checkboxInput("trimmer_overide",label = "Overide Speed up",value = FALSE)
                     )
                     
                   )
                    
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
                    selectInput("HWB_PM", "Metrics for Power usage plot:", choices = c("PM3_I","PM3_PW","PM3_PVA","PM3_PWH","PM3_FR","PM3_PF","PM3_V"),
                                selected =  c("PM3_PVA"),
                                multiple = TRUE),
                  width = 5 ),
                height =100, width = 12)),
              conditionalPanel(condition= "input.metric.length > 0",
              fluidRow(box(withSpinner(plotlyOutput("HWB_top", height = 350)),width = 12))
              ),
              # fluidRow(box(withSpinner(plotlyOutput("HWB_bottom", height = 350)),width = 12)),
              # fluidRow(box(withSpinner(plotlyOutput("HWB_bottomid", height = 350)),width = 12)),
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
                  conditionalPanel(
                    condition= ("input.EXTEND_ID == 'EXT0007'"),
                             
                  fluidRow(
                    selectInput("SHB_BMid", "Space Heating Metrics:", choices = c("HS_TS6","HS_TS7","HS_TS8","HS_TS9","ROOM_TS19"),
                                selected =  c("HS_TS6","HS_TS7","HS_TS8","HS_TS9"),
                                multiple = TRUE)),
                    )),
                column(5,
                       conditionalPanel(
                         condition= ("input.EXTEND_ID == 'EXT0007'"), 
                  fluidRow(
                  selectInput("SHB_BM", "Metrics for Hydronics plot:", choices = c("HS_TS10","HS_TS11","HS_TS12","FS5_FS"),
                              selected =   c("HS_TS10","HS_TS11","HS_TS12","FS5_FS"),
                              multiple = TRUE))), 
                  fluidRow(
                    selectInput("SHB_BB", "Metrics for Power usage plot:", choices = c("PM2_I","PM2_PW","PM2_PVA","PM2_PWH","PM2_FR","PM2_PF","PM2_V"),
                                selected =   c("PM2_PVA"),
                                multiple = TRUE)),
                  ),
                  
                  height =150, width = 12)),
              conditionalPanel(condition= "input.SHB_TM.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_top", height = 350)),width = 12))),
              conditionalPanel(
                condition= ("input.EXTEND_ID == 'EXT0007'"),
              conditionalPanel(condition= "input.SHB_BM.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_bottom", height = 350)),width = 12))),
              conditionalPanel(condition= "input.SHB_BMid.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_bottomid", height = 350)),width = 12)))),
              conditionalPanel(condition= "input.SHB_BB.length > 0",
                               fluidRow(box(withSpinner(plotlyOutput("SHB_bottomer", height = 350)),width = 12)))
      ),
      #OPERATIONAL MODE
      tabItem(tabName = "OP_MO",
              box(title = "Operation Mode", status = "primary", height = "1200",width = "12",solidHeader = T, 
                  DT::dataTableOutput("OP_mode_table"),style = "height:1000px; overflow-y: scroll; overflow-x: scroll;")),
      #TOU
      tabItem(tabName = "TOU",
              fluidPage(
                # fluidRow(box(withSpinner(plotlyOutput("TOU_plot", height = 450)),width = 12)),
                fluidRow(box(withSpinner(plotlyOutput("OP_Schedule_plot3", height = 450)),width = 12)),
                fluidRow(box(withSpinner(plotlyOutput("OP_Schedule_plot",height = 450)),width = 12)),
                fluidRow(box(withSpinner(plotlyOutput("OP_Schedule_plot2",height = 450)),width = 12)),
                fluidRow(box(withSpinner(plotlyOutput("OP_Schedule_plot4", height = 450)),width = 12)),
                fluidRow(box(withSpinner(plotlyOutput("OP_Schedule_plot5", height = 450)),width = 12))
                
              )),
      #Heat load
      tabItem(tabName = "Heat_Load",
              fluidRow(box(withSpinner(plotlyOutput("HeatLoad", height = 450)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("PowerPlot", height = 250)),width = 12)),
              fluidRow(box(withSpinner(plotlyOutput("SOCPlot", height = 250)),width = 12))
              ),
      #hydroncis
      tabItem(tabName = "Hydronics",
              conditionalPanel(condition= ("input.EXTEND_ID == 'EXT0006'"),
                               selectInput("Hydronics_im","Metrics to plot:",choices = c("HP_TS16",	
                                                                                         "HP_TS17",	
                                                                                         "HP_TS18", 
                                                                                         "ROOM_TS19",
                                                                                         "HS_TS1",
                                                                                         "HS_TS2",	
                                                                                         "HS_TS3",	
                                                                                         "HS_TS4"	,
                                                                                         "HS_TS5"	,
                                                                                         "HS_TS6"	,
                                                                                         "HS_TS7" ,
                                                                                         "FS1_FS"	,
                                                                                         "FS2_FS", 
                                                                                         "PUMP_1"),selected = c("ROOM_TS19",
                                                                                                                "HS_TS1",
                                                                                                                "HS_TS2",	
                                                                                                                "HS_TS3",	
                                                                                                                "HS_TS4"	,
                                                                                                                "HS_TS5"	,
                                                                                                                "HS_TS6"	,
                                                                                                                "HS_TS7"),
                                           multiple = TRUE, width = 2000
                                           
                               )
              ),
              conditionalPanel(condition= "input.Hydronics_im.length > 0",
                               
                               fluidRow(box(withSpinner(plotlyOutput("Hydronicsplot", height = 450)),width = 12))),
              fluidRow(box(withSpinner(plotlyOutput("OPM_Plot", height = 250)),width = 12)),
              conditionalPanel(condition= ("input.EXTEND_ID == 'EXT0006'"),
                               fluidRow(box(withSpinner(plotlyOutput("valveplot", height = 250)),width = 12))
              ),
              
              conditionalPanel(condition= ("input.EXTEND_ID == 'EXT0006'"),
                               fluidRow(img(src="LDESS_2.png",height = '500px')))
              
      ),
      #master plot
      tabItem(tabName = "Mastergraph",
              fluidPage(fluidRow(p("This page is only compatable with LDESS2 systems for now")),
                                 column(6,
                                        fluidRow(selectInput("Master_Y1_LDESS_2","variables",choices = c(
                                          
                                          "HWB_TSB",	"HWB_TSM",	"HWB_TST",	"CHB_TS1_1",	"CHB_TS1_2",	"CHB_TS1_3",	"CHB_TS1_4",
                                          "CHB_TS1_5","CHB_TS2_1",	"CHB_TS2_2",	"CHB_TS2_3",	"CHB_TS2_4",	"CHB_TS2_5",	"CHB_TS3_1",	"CHB_TS3_2",	"CHB_TS3_3"	,
                                          "CHB_TS3_4",	"CHB_TS3_5",	"CHB_TS4_1",	"CHB_TS4_2",	"CHB_TS4_3",	"CHB_TS4_4",	"CHB_TS4_5",	"CHB_TS5_1",	"CHB_TS5_2",	
                                          "CHB_TS5_3",	"CHB_TS5_4",	"CHB_TS5_5",	"CHB_TS6_1",	"CHB_TS6_2",	"CHB_TS6_3",	"CHB_TS6_4",	"CHB_TS6_5",	"DG1", "Setpoint",
                                          "COP",	"HP_TS16",	"HP_TS17","HP_TS18",	"ROOM_TS19"	,"HS_TS1",	"HS_TS2",	"HS_TS3",	"HS_TS4",	"HS_TS5",	"HS_TS6",	"HS_TS7"	,
                                          "HWB_SOC_pct",	"HWB_SOC_KJ",	"HWB_SOC_KJ_LO"	,"CHB_SOC_pct",	"CHB_SOC_KJ",	"CHB_SOC_KJ_LO",	"FS1_FS",	"FS2_FS",	"ZV"	,
                                          "MV1",	"MV2"	,"PUMP_1", "PM1_I",	"PM1_I_LO",	"PM1_W",	"PM1_W_LO",	"PM1_PVA",	"PM1_PVA_LO",	"PM1_PWH",	"PM1_PWH_LO",	"PM1_FR",	
                                          "PM1_PF",	"PM1_V",	"PM2_I",	"PM2_I_LO",	"PM2_W",	"PM2_W_LO",	"PM2_PVA",	"PM2_PVA_LO",	"PM2_PWH",	"PM2_PWH_LO",	"PM2_FR"	,
                                          "PM2_PF",	"PM2_V",	"PM3_I",	"PM3_I_LO",	"PM3_W",	"PM3_W_LO",	"PM3_PVA",	"PM3_PVA_LO",	"PM3_PWH",	"PM3_PWH_LO",	"PM3_FR"	,
                                          "PM3_PF",	"PM3_V"	,"PM4_I",	"PM4_I_LO",	"PM4_W",	"PM4_W_LO",	"PM4_PVA",	"PM4_PVA_LO",	"PM4_PWH",	"PM4_PWH_LO",	"PM4_FR"	,
                                          "PM4_PF",	"PM4_V",	"Heat_ meter_Power_in_KWH",	"Heat_ meter_Power_in_KWH_LO"	,"Flow_V1",	"Flow_V1_LO",	"Flow_V2"	,
                                          "Flow_V2_LO",	"T1_HI",	"T1_LO"	,"T2_HI",	"T2_LO"	,"SYS_CURR_OPM"	,"SSC_CORE_TYPE",	"CHB_Size",	"ERROR_ZV",	"ERROR_MV"	,
                                          "ERROR_PUMP",	"ERROR_SYS",	"ERROR_COMMS",	"PS1",	"Safety_limit_lower",	"Safety_limit_higher"	,"OPM_limit_lower"	,
                                          "OPM_limit_higher",	"Hex_mode",	"OP_Mode"	,"State_Explination",	"CHB_TS5_AVG",	"CHB_TS4_AVG"	,"CHB_TS3_AVG"	,
                                          "CHB_TS2_AVG",	"CHB_TS1_AVG",	"SHB_Charging_Heat_load",	"SHB_Discharging_Heat_load"	,"HP_Heat_load"	,
                                          "HW_Heat_load",	"SHB_Direct",	"space_heating_user_schedule",	"hot_water_user_schedule"	,
                                          "Agreed_Consumption_schedule"	,"consumption_Schedule"	,"SH Setpoint", "HWB Recharge SOC",	"SHB_max_IN_KW"	,
                                          "SHB_min_IN_KW"	,"HP_Source_Battery",	"HP_IN_KW",	"HP_OUT_KW",	"HP_HWB_Priority",	"HP_SHB_Priority","HP_SH_Priority"	,
                                          "HWB_Elec_IN_KW_max",	"HWB_Elec_IN_KW_min" , "MV1_return_value", "MV2_return_value"
                                          
                                        ), multiple= TRUE,width = 1000)),
                                        
                                        
                                        
                                        
                                        
                                        
                                        fluidRow(selectInput("Master_Y1_mult_LDESS_2","Multiplier variables (Y1 axis)",choices = c(
                                          
                                        	"HWB_TSB",	"HWB_TSM",	"HWB_TST",	"CHB_TS1_1",	"CHB_TS1_2",	"CHB_TS1_3",	"CHB_TS1_4",
                                          "CHB_TS1_5","CHB_TS2_1",	"CHB_TS2_2",	"CHB_TS2_3",	"CHB_TS2_4",	"CHB_TS2_5",	"CHB_TS3_1",	"CHB_TS3_2",	"CHB_TS3_3"	,
                                          "CHB_TS3_4",	"CHB_TS3_5",	"CHB_TS4_1",	"CHB_TS4_2",	"CHB_TS4_3",	"CHB_TS4_4",	"CHB_TS4_5",	"CHB_TS5_1",	"CHB_TS5_2",	
                                          "CHB_TS5_3",	"CHB_TS5_4",	"CHB_TS5_5",	"CHB_TS6_1",	"CHB_TS6_2",	"CHB_TS6_3",	"CHB_TS6_4",	"CHB_TS6_5",	"DG1", "Setpoint",
                                          "COP",	"HP_TS16",	"HP_TS17","HP_TS18",	"ROOM_TS19"	,"HS_TS1",	"HS_TS2",	"HS_TS3",	"HS_TS4",	"HS_TS5",	"HS_TS6",	"HS_TS7"	,
                                          "HWB_SOC_pct",	"HWB_SOC_KJ",	"HWB_SOC_KJ_LO"	,"CHB_SOC_pct",	"CHB_SOC_KJ",	"CHB_SOC_KJ_LO",	"FS1_FS",	"FS2_FS",	"ZV"	,
                                          "MV1",	"MV2"	,"PUMP_1", "PM1_I",	"PM1_I_LO",	"PM1_W",	"PM1_W_LO",	"PM1_PVA",	"PM1_PVA_LO",	"PM1_PWH",	"PM1_PWH_LO",	"PM1_FR",	
                                          "PM1_PF",	"PM1_V",	"PM2_I",	"PM2_I_LO",	"PM2_W",	"PM2_W_LO",	"PM2_PVA",	"PM2_PVA_LO",	"PM2_PWH",	"PM2_PWH_LO",	"PM2_FR"	,
                                          "PM2_PF",	"PM2_V",	"PM3_I",	"PM3_I_LO",	"PM3_W",	"PM3_W_LO",	"PM3_PVA",	"PM3_PVA_LO",	"PM3_PWH",	"PM3_PWH_LO",	"PM3_FR"	,
                                          "PM3_PF",	"PM3_V"	,"PM4_I",	"PM4_I_LO",	"PM4_W",	"PM4_W_LO",	"PM4_PVA",	"PM4_PVA_LO",	"PM4_PWH",	"PM4_PWH_LO",	"PM4_FR"	,
                                          "PM4_PF",	"PM4_V",	"Heat_ meter_Power_in_KWH",	"Heat_ meter_Power_in_KWH_LO"	,"Flow_V1",	"Flow_V1_LO",	"Flow_V2"	,
                                          "Flow_V2_LO",	"T1_HI",	"T1_LO"	,"T2_HI",	"T2_LO"	,"SYS_CURR_OPM"	,"SSC_CORE_TYPE",	"CHB_Size",	"ERROR_ZV",	"ERROR_MV"	,
                                          "ERROR_PUMP",	"ERROR_SYS",	"ERROR_COMMS",	"PS1",	"Safety_limit_lower",	"Safety_limit_higher"	,"OPM_limit_lower"	,
                                          "OPM_limit_higher",	"Hex_mode",	"OP_Mode"	,"State_Explination",	"CHB_TS5_AVG",	"CHB_TS4_AVG"	,"CHB_TS3_AVG"	,
                                          "CHB_TS2_AVG",	"CHB_TS1_AVG",	"SHB_Charging_Heat_load",	"SHB_Discharging_Heat_load"	,"HP_Heat_load"	,
                                          "HW_Heat_load",	"SHB_Direct",	"space_heating_user_schedule",	"hot_water_user_schedule"	,
                                          "Agreed_Consumption_schedule"	,"consumption_Schedule"	,"SH Setpoint", "HWB Recharge SOC",	"SHB_max_IN_KW"	,
                                          "SHB_min_IN_KW"	,"HP_Source_Battery",	"HP_IN_KW",	"HP_OUT_KW",	"HP_HWB_Priority",	"HP_SHB_Priority","HP_SH_Priority"	,
                                          "HWB_Elec_IN_KW_max",	"HWB_Elec_IN_KW_min", "MV1_return_value", "MV2_return_value"
                                          
                                        ), multiple= TRUE,width = 1000))),
                                 column(5,
                                        fluidRow(selectInput("Master_Y2_LDESS_2","variables for second y axis:",choices = c(
                                          
                                          "HWB_TSB",	"HWB_TSM",	"HWB_TST",	"CHB_TS1_1",	"CHB_TS1_2",	"CHB_TS1_3",	"CHB_TS1_4",
                                          "CHB_TS1_5","CHB_TS2_1",	"CHB_TS2_2",	"CHB_TS2_3",	"CHB_TS2_4",	"CHB_TS2_5",	"CHB_TS3_1",	"CHB_TS3_2",	"CHB_TS3_3"	,
                                          "CHB_TS3_4",	"CHB_TS3_5",	"CHB_TS4_1",	"CHB_TS4_2",	"CHB_TS4_3",	"CHB_TS4_4",	"CHB_TS4_5",	"CHB_TS5_1",	"CHB_TS5_2",	
                                          "CHB_TS5_3",	"CHB_TS5_4",	"CHB_TS5_5",	"CHB_TS6_1",	"CHB_TS6_2",	"CHB_TS6_3",	"CHB_TS6_4",	"CHB_TS6_5",	"DG1", "Setpoint",
                                          "COP",	"HP_TS16",	"HP_TS17","HP_TS18",	"ROOM_TS19"	,"HS_TS1",	"HS_TS2",	"HS_TS3",	"HS_TS4",	"HS_TS5",	"HS_TS6",	"HS_TS7"	,
                                          "HWB_SOC_pct",	"HWB_SOC_KJ",	"HWB_SOC_KJ_LO"	,"CHB_SOC_pct",	"CHB_SOC_KJ",	"CHB_SOC_KJ_LO",	"FS1_FS",	"FS2_FS",	"ZV"	,
                                          "MV1",	"MV2"	,"PUMP_1", "PM1_I",	"PM1_I_LO",	"PM1_W",	"PM1_W_LO",	"PM1_PVA",	"PM1_PVA_LO",	"PM1_PWH",	"PM1_PWH_LO",	"PM1_FR",	
                                          "PM1_PF",	"PM1_V",	"PM2_I",	"PM2_I_LO",	"PM2_W",	"PM2_W_LO",	"PM2_PVA",	"PM2_PVA_LO",	"PM2_PWH",	"PM2_PWH_LO",	"PM2_FR"	,
                                          "PM2_PF",	"PM2_V",	"PM3_I",	"PM3_I_LO",	"PM3_W",	"PM3_W_LO",	"PM3_PVA",	"PM3_PVA_LO",	"PM3_PWH",	"PM3_PWH_LO",	"PM3_FR"	,
                                          "PM3_PF",	"PM3_V"	,"PM4_I",	"PM4_I_LO",	"PM4_W",	"PM4_W_LO",	"PM4_PVA",	"PM4_PVA_LO",	"PM4_PWH",	"PM4_PWH_LO",	"PM4_FR"	,
                                          "PM4_PF",	"PM4_V",	"Heat_ meter_Power_in_KWH",	"Heat_ meter_Power_in_KWH_LO"	,"Flow_V1",	"Flow_V1_LO",	"Flow_V2"	,
                                          "Flow_V2_LO",	"T1_HI",	"T1_LO"	,"T2_HI",	"T2_LO"	,"SYS_CURR_OPM"	,"SSC_CORE_TYPE",	"CHB_Size",	"ERROR_ZV",	"ERROR_MV"	,
                                          "ERROR_PUMP",	"ERROR_SYS",	"ERROR_COMMS",	"PS1",	"Safety_limit_lower",	"Safety_limit_higher"	,"OPM_limit_lower"	,
                                          "OPM_limit_higher",	"Hex_mode",	"OP_Mode"	,"State_Explination",	"CHB_TS5_AVG",	"CHB_TS4_AVG"	,"CHB_TS3_AVG"	,
                                          "CHB_TS2_AVG",	"CHB_TS1_AVG",	"SHB_Charging_Heat_load",	"SHB_Discharging_Heat_load"	,"HP_Heat_load"	,
                                          "HW_Heat_load",	"SHB_Direct",	"space_heating_user_schedule",	"hot_water_user_schedule"	,
                                          "Agreed_Consumption_schedule"	,"consumption_Schedule"	,"SH Setpoint", "HWB Recharge SOC",	"SHB_max_IN_KW"	,
                                          "SHB_min_IN_KW"	,"HP_Source_Battery",	"HP_IN_KW",	"HP_OUT_KW",	"HP_HWB_Priority",	"HP_SHB_Priority","HP_SH_Priority"	,
                                          "HWB_Elec_IN_KW_max",	"HWB_Elec_IN_KW_min" , "MV1_return_value", "MV2_return_value"
                                          
                                        ), multiple= TRUE,width = 1000)),
                                        fluidRow(
                                          column(5,textInput("MasterMultiplier","Multiplier",value = "1")),
                                        column(5,actionButton("plotButton", "Generate Plot")))
                                        
                                 ),
                                 fluidRow(box(withSpinner(plotlyOutput("Master_Plot",height = 650)),width = 12))
                                 
                
                
              )      
              
      )#end tabitem
    
  )
  )
  )
