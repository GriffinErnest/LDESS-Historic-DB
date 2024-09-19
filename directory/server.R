#Write server output
server <- function(input, output, session){
  #####
  #buttons
  #####
  #week forward
  observeEvent(input$Week_forward, {
    updateDateRangeInput(session, 'dates', 
                         start = input$dates[1] + 7 ,
                         end = input$dates[2] + 7)})
  #week back
  observeEvent(input$Week_back, {
    updateDateRangeInput(session, 'dates', 
                         start = input$dates[1] - 7 ,
                         end = input$dates[2] - 7)})
  #day forward
  observeEvent(input$Day_forward, {
    updateDateRangeInput(session, 'dates', 
                         start = input$dates[1] + 1 ,
                         end = input$dates[2] + 1)})
  #day back
  observeEvent(input$Day_back, {
    updateDateRangeInput(session, 'dates', 
                         start = input$dates[1] - 1 ,
                         end = input$dates[2] - 1)})
  
#####
# Functions
#####
  #Retrieves LDESS type from EXTEND ID
  #WIP: THIS WILLL BE UPDATED TO USE A EXTERNALLY DEFINED LOOKUP TABLE
  LDESS <- reactive({
    ID <- input$EXTEND_ID
    if(input$EXTEND_ID  %in% "EXT0007"){
      output <- 1
    }else{
      output <- 2
    }
    return(output)
  })
 #CALCULATES HEAT LOAD
  Heat_load_calculator <- function(op_mode,op_mode_string,flow,heat1,heat2,specific_heat){
    if(any(op_mode %in% op_mode_string) & !(flow == 0)){
      value <- as.numeric(flow) * as.numeric(specific_heat) * abs(as.numeric(heat1) - as.numeric(heat2))
    }else{
      value <- 0
    }
    return(value)
  }

  #data pull functions
  sql_table_pull <- function(table_name,startdate,enddate,schema_name){
    dotenv::load_dot_env()
    #db information:
    db <- 'ldess_db'
    host_db <- Sys.getenv("DB_HOST") 
    db_port <- Sys.getenv("DB_PORT") 
    db_user <- Sys.getenv("DB_USER") 
    db_password <- Sys.getenv("DB_PASS") 
    
    #connect, define and run query   
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = db,
                     host = host_db,
                     port = db_port,
                     user = db_user,
                     password = db_password)
    
    query <- paste0("SELECT * FROM ",schema_name,".",table_name," WHERE \"DateTime\" >= \'",startdate,"\'::date AND \"DateTime\" < \'",enddate,"\'::date ;")
    result <- dbGetQuery(con, query)
    
    dbDisconnect(con) #disconnect
    return(result)
    
  }
  ######
  #Data Extraction  
  #####
  #Here we are creating a reacive value with 2 points, enough to trigger a refresh for each query when needed.
  #every time the button is pushed 2 more points are given to the rv value and the refreshes will run again.
  #we are preventing one refresh from taking all of the points by giving each command a unique decimal number it will take the points to
  
  rv <- reactiveVal(2) #reactive value starts at 2 upon innitialization
  
  stored_opp <- reactiveVal() #storage for operating table
  
  Operating_DF_pre <- eventReactive(rv(),{
    if(rv() == 2|rv() == 1){
      
      result <- sql_table_pull(table_name = "opschedule",
                               startdate = input$dates[1] %>% as.character(),
                               enddate = input$dates[2] %>% as.character(),
                               schema_name = input$EXTEND_ID %>% tolower())
      # rename
      #name mapping
      rename_dict <- c("shBatteryDirectMode" ="SHB_Direct",
                       "spaceHeatingSetpoint" = "SH_Setpoint",
                       "therminoStartRechargeSoc" = "HWB Recharge SOC",
                       "extendElements.maxElecInputKw" = "SHB_max_IN_KW",
                       "extendElements.minElecInputKw" = "SHB_min_IN_KW",
                       "heatPumpControl.hpSourceBattery" = "HP_Source_Battery",
                       "heatPumpControl.hpElectricInputKw" = "HP_IN_KW",
                       "heatPumpControl.hpThermalOutputKw" ="HP_OUT_KW",
                       "heatPumpControl.hwBatteryPriority" = "HP_HWB_Priority",
                       "heatPumpControl.shBatteryPriority" = "HP_SHB_Priority",
                       "heatPumpControl.spaceHeatingPriority" ="HP_SH_Priority",
                       "therminoElements.maxElecInputKw" = "HWB_Elec_IN_KW_max",
                       "therminoElements.minElecInputKw" = "HWB_Elec_IN_KW_min",
                       "DateTime" = "DateTime",
                       "space_heating_user_schedule" = "space_heating_user_schedule",
                       "hot_water_user_schedule" = "hot_water_user_schedule",
                       "consumption_Schedule" = "consumption_Schedule",
                       "Agreed_Consumption_schedule" = "Agreed_Consumption_schedule",
                       "TOU_NoVAT" = "TOU_NoVAT",
                       "TOU_VAT" = "TOU_VAT" ,
                       "outside_temperature_forecast" = "outside_temperature_forecast"
      )
      names(result) <- rename_dict[names(result)]
      
      result$SHB_Direct <- ifelse(result$SHB_Direct == TRUE,1,0) 
      
      #force integer columns to be numeric
      forcenumeric <- c( "HP_HWB_Priority", "HP_SHB_Priority","HP_SH_Priority")
      
      for(i in forcenumeric){
        result[,colnames(result) == i] <-  result[,colnames(result) == i] %>% as.numeric()
      }
      
      #correcting the heat schedule decimil place
      result$space_heating_user_schedule <- as.numeric(result$space_heating_user_schedule) /10
      
      #result$DateTime <- lubridate::with_tz(result$DateTime, tzone = "Europe/London") #converts to from UTC to BST
      
      stored_opp(result) 
      print("opp query run, rv should now be 1.5 or 0")
      if(rv() ==1){
        rv(0)
      }else if(rv() == 2){
        rv(1.5)
      }
      
    }
      
     
    
    return(stored_opp())
    
  })
  
  
  

  stored_data <- reactiveVal() #storage for maintanece log 
  
  Master_DF <- eventReactive(rv(),{
    if(rv() == 2|rv() == 1.5){
      
      result <- sql_table_pull(table_name = "sensor_logs",
                     startdate = input$dates[1] %>% as.character(),
                     enddate = input$dates[2] %>% as.character(),
                     schema_name = input$EXTEND_ID %>% tolower())
      print("master query has run, rv value shoudl be 1 or 0")
      
      stored_data(result)
      if(rv() ==1.5){
        rv(0)
      }else if(rv() == 2){
        rv(1)
      }
      
    }
    return(stored_data())
    
  })
  
  #add 2 to RV when datapull button is pushed
  observeEvent(input$datapull,{
    rv(2) 
    print("button pushed")
    })
  
  #filter out based on slider filter
  Master_DF_2_pre <- reactive({  
    DF <- Master_DF()
    CyclesMerge <- input$CyclesMerge
    
    #define actual maximum and minimum values in the data  
    min <- DF$DateTime %>% min() #corresponds to 0
    max <- DF$DateTime %>% max() #corressponds to 1
    #grab the difference
    diff <- max - min
    #grab the minimum andmaximum selected inputs and get the distance between those values and 0 and 1 respectivly
    minselect <- CyclesMerge %>% min()
    propmingap <- minselect
    maxselect <- CyclesMerge %>% max()
    propmaxgap <- 1 - maxselect
    #calculate from these the actual min and max value
    minval <- minselect*diff + min
    maxval <- max - propmaxgap*diff
    #filter out based on these
    DF <- DF[DF$DateTime >= minval & DF$DateTime <= maxval, ]
   
    #####
    #pull out data if too much is requested
    if(input$trimmer_overide == TRUE){
      DF <- DF[order(DF$DateTime,decreasing = TRUE),]
      if(difftime(DF$DateTime %>% max(),DF$DateTime %>% min()) <= hours(12)){
        DF <- DF
      }else if(difftime(DF$DateTime %>% max(),DF$DateTime %>% min()) <= hours(24)){
        DF <- DF[-seq(3,nrow(DF), by = 3),]
      }else if(difftime(DF$DateTime %>% max(),DF$DateTime %>% min()) <= hours(48)){
        DF <- DF[seq(1,nrow(DF), by = 2),]
      }else if(difftime(DF$DateTime %>% max(),DF$DateTime %>% min()) <= hours(72)){
        rows_to_keep <- seq(1, nrow(DF), by = 3)
        DF <- DF[rows_to_keep, ]
      }else if(difftime(DF$DateTime %>% max(),DF$DateTime %>% min()) <= hours(100)){
        rows_to_keep <- seq(1, nrow(DF), by = 5)
        DF <- DF[rows_to_keep, ]
      }
    }
    return(DF) 
    
    
  })
  #adjust opearting table based on slider selection
  Operating_DF <- reactive({  
    DF <- Operating_DF_pre()
    CyclesMerge <- input$CyclesMerge
    
    #define actual maximum and minimum values in the data  
    min <- DF$DateTime %>% min() #corresponds to 0
    max <- DF$DateTime %>% max() #corressponds to 1
    #grab the difference
    diff <- max - min
    #grab the minimum andmaximum selected inputs and get the distance between those values and 0 and 1 respectivly
    minselect <- CyclesMerge %>% min()
    propmingap <- minselect
    maxselect <- CyclesMerge %>% max()
    propmaxgap <- 1 - maxselect
    #calculate from these the actual min and max value
    minval <- minselect*diff + min
    maxval <- max - propmaxgap*diff
    #filter out based on these
    DF <- DF[DF$DateTime >= minval & DF$DateTime <= maxval, ]
    
    
    return(DF) 
    
    
  })
  
  
  #calculated columns
  Master_DF_2 <- reactive({  
    DF <- Master_DF_2_pre()
    
    #DF$DateTime <- lubridate::with_tz(DF$DateTime, tzone = "Europe/London") #converts to from UTC to BST
    
    #correct temperatures
    temperature_columns <- c("HS_TS1",
                             "HS_TS2",
                             "HS_TS3",
                             "HS_TS4",                           
                             "HS_TS5"      ,                     
                             "HS_TS6",
                             "HS_TS7",
                             "HS_TS8",
                             "HS_TS9" ,
                             "HS_TS10",
                             "HS_TS11",
                             "HS_TS12",
                             "HS_TS13" ,                        
                             "HP_TS14",
                             "HP_TS15" ,                         
                             "HP_TS16"  ,                        
                             "HP_TS17"   ,                       
                             "HP_TS18"    ,  
                             "ROOM_TS19"   ,                     
                             "HWB_TSB"      ,                    
                             "HWB_TSM"      ,                   
                             "HWB_TST"  ,
                             "CHB_TS1_1"  ,                      
                             "CHB_TS1_2"   ,                     
                             "CHB_TS1_3"    ,                    
                             "CHB_TS1_4"     ,
                             "CHB_TS1_5"      ,                  
                             "CHB_TS2_1"       ,                 
                             "CHB_TS2_2"        ,                
                             "CHB_TS2_3" ,
                             "CHB_TS2_4"  ,                      
                             "CHB_TS2_5"   ,                     
                             "CHB_TS3_1"    ,                    
                             "CHB_TS3_2" ,
                             "CHB_TS3_3"  ,                      
                             "CHB_TS3_4"   ,                     
                             "CHB_TS3_5"    ,                    
                             "CHB_TS4_1"    ,
                             "CHB_TS4_2"     ,                  
                             "CHB_TS4_3"      ,                  
                             "CHB_TS4_4"       ,                 
                             "CHB_TS4_5"        ,  
                             "CHB_TS5_1"         ,               
                             "CHB_TS5_2"          ,              
                             "CHB_TS5_3"           ,             
                             "CHB_TS5_4"           ,
                             "CHB_TS5_5"            ,            
                             "CHB_TS6_1"             ,           
                             "CHB_TS6_2"              ,          
                             "CHB_TS6_3"             ,
                             "CHB_TS6_4"              ,          
                             "CHB_TS6_5"               ,         
                             "CHB_TS7_1"                ,        
                             "CHB_TS7_2"     ,
                             "CHB_TS7_3"      ,                  
                             "CHB_TS7_4"       ,                 
                             "CHB_TS7_5"        ,                
                             "FS1_TS" ,
                             "FS2_TS"  ,                        
                             "FS3_TS"   ,                       
                             "FS4_TS"    ,                      
                             "FS5_TS"   ,
                             "FS6_TS"    ,                      
                             "FS7_TS"     ,                     
                             "FS8_TS",
                             "FS1_FS",
                             "FS2_FS",
                             "FS3_FS",
                             "FS4_FS",
                             "FS5_FS",
                             "FS6_FS"
    )
    for(i in temperature_columns){
      DF[,colnames(DF) %in% i] <- DF[,colnames(DF) %in% i] / 100
    }
    #current columns
    current <- c("PM1_I",
                 "PM2_I",
                 "PM3_I",
                 "PM4_I"
    )
    for(i in current){
      DF[,colnames(DF) %in% i] <- DF[,colnames(DF) %in% i] / 1000
    }
    #other power columns
    pwer_cols <- c("PM1_W",
                   "PM2_PW",
                   "PM3_PW",
                   "PM4_PW",
                   "PM1_PVA",
                   "PM2_PVA",
                   "PM3_PVA",
                   "PM4_PVA",
                   "PM1_PWH",
                   "PM2_PWH",
                   "PM3_PWH",
                   "PM4_PWH",
                   "PM1_FR",
                   "PM2_FR",
                   "PM3_FR",
                   "PM4_FR",
                   "PM1_PF",
                   "PM2_PF",
                   "PM3_PF",
                   "PM4_PF",
                   "PM1_V",
                   "PM2_V",
                   "PM3_V",
                   "PM4_V")
    for(i in pwer_cols){
      DF[,colnames(DF) %in% i] <- ifelse(is.na(as.numeric(DF[,colnames(DF) %in% i])), 0, as.numeric(DF[,colnames(DF) %in% i])/ 10) 
    }
    #change SOC variables to numeric
    DF$HWB_SOC_pct <- DF$HWB_SOC_pct %>% as.numeric()
    DF$CHB_SOC_pct <- DF$CHB_SOC_pct %>% as.numeric()
    
    #clean operating mode 
    Mode_mapping <- read_excel("register_mapping.xlsx",sheet = "Op_mode")
    
    DF$Hex_mode <- as.hexmode(DF$SYS_CURR_OPM) 
    #DF$Hex_mode <- gsub("^0+", "", DF$Hex_mode)
    DF$OP_Mode <-  Mode_mapping$State[match(tolower(DF$Hex_mode), tolower(Mode_mapping$Reading))]
    DF$State_Explination <-  Mode_mapping$Explination[match(tolower(DF$Hex_mode), tolower(Mode_mapping$Reading))]
    
    #clean up zone valve mapping. position value is = x mod 256 and the power value is x/256 rounded down 
    mv_cols <- c("MV1","MV2")
    for(i in mv_cols){
      DF[,colnames(DF) == i] <- mod(DF[,colnames(DF) == i] %>% as.numeric(),256)
      column_title <- paste0(i,"_return_value")
      DF[[column_title]] <- DF[,colnames(DF) == i] %>% as.numeric() %>% round(digits = 0)
    }
    
    
    #create averages for SHB
    #Create a mean TS reading for Extend battery
    #level 5
    DF$CHB_TS5_AVG <- 
      (DF$CHB_TS1_5 +
         DF$CHB_TS2_5 +
         DF$CHB_TS3_5 +
         DF$CHB_TS4_5 +
         DF$CHB_TS5_5 +
         DF$CHB_TS6_5)/ 6
    #level 4
    DF$CHB_TS4_AVG <- 
      (DF$CHB_TS1_4 +
         DF$CHB_TS2_4 +
         DF$CHB_TS3_4 +
         DF$CHB_TS4_4 +
         DF$CHB_TS5_4 +
         DF$CHB_TS6_4) / 6
    #level 3
    DF$CHB_TS3_AVG <- 
      (DF$CHB_TS1_3 +
         DF$CHB_TS2_3 +
         DF$CHB_TS3_3 +
         DF$CHB_TS4_3 +
         DF$CHB_TS5_3 +
         DF$CHB_TS6_3) / 6
    #level 2
    DF$CHB_TS2_AVG <- 
      (DF$CHB_TS1_2 +
         DF$CHB_TS2_2 +
         DF$CHB_TS3_2 +
         DF$CHB_TS4_2 +
         DF$CHB_TS5_2 +
         DF$CHB_TS6_2) / 6
    #level 1
    DF$CHB_TS1_AVG <- 
      (DF$CHB_TS1_1 +
         DF$CHB_TS2_1 +
         DF$CHB_TS3_1 +
         DF$CHB_TS4_1 +
         DF$CHB_TS5_1 +
         DF$CHB_TS6_1) / 6
    #water specific heat
    specific_heat_water <- 4.187
    #Potassium Formate Specific Heat approximation
    specific_heat_PF <- 2.55 # at 20 degrees from file:///C:/Users/griffin.ernest/OneDrive%20-%20Sunamp/BEIS%20-%20Long%20Duration%20Storage/Hydronic%20System/12%20-%20Working%20fluid/coolflow-LVF50-v1.01-tds.pdf
      #can change this when we have better data
  #calculate heat load 
    if(LDESS() == 1){
      DF$SHB_Electrical_Heat_load <- DF$FS5_FS * specific_heat_PF * abs(DF$HS_TS11  - DF$HS_TS10)
      DF$HP_Brine_Heat_load <- DF$FS6_FS * specific_heat_PF * abs(DF$HP_TS14 - DF$HP_TS15)
      DF$HP_output_Heat_load <- DF$FS6_FS * specific_heat_PF * abs(DF$HP_TS17 - DF$HP_TS16)
      DF$SH_toHX_Heat_load <- DF$FS2_FS * specific_heat_PF * abs(DF$HS_TS7 - DF$HS_TS6)
      DF$SH_fromHX_Heat_load <- DF$FS4_FS * specific_heat_water * abs(DF$HS_TS8 - DF$HS_TS9)
      DF$HP_toThermino_Heat_load <- DF$FS2_FS * specific_heat_PF * abs(DF$HS_TS3 - DF$HS_TS2)
    }else if(LDESS() == 2){
      DF$SHB_Charging_Heat_load <- apply(DF, 1, function(row) Heat_load_calculator(op_mode = row['OP_Mode'], op_mode_string = c("A2","D2A2","D1","D2D1"), flow = row['FS1_FS'],heat1 = row['HS_TS6'],heat2 = row['HS_TS3'],specific_heat = 4.187 ) ) %>% as.numeric()
      DF$SHB_Discharging_Heat_load <-  apply(DF, 1, function(row) Heat_load_calculator(op_mode = row['OP_Mode'], op_mode_string = c("S1", "D2S1"), flow = row['FS2_FS'],heat1 = row['HS_TS6'],heat2 = row['HS_TS3'],specific_heat = 4.187 ) ) %>% as.numeric()
      DF$HP_Heat_load <- apply(DF, 1, function(row) Heat_load_calculator(op_mode = row['OP_Mode'], op_mode_string = c("A1", "D2A1", "A3", "D2A3","D2"), flow = row['FS1_FS'],heat1 = row['HS_TS4'],heat2 = row['HS_TS1'],specific_heat = 4.187 ) ) %>% as.numeric()
      DF$HW_Heat_load <- apply(DF, 1, function(row) Heat_load_calculator(op_mode = row['OP_Mode'], op_mode_string = c("A3", "D2A3"), flow = row['FS2_FS'],heat1 = row['HS_TS3'],heat2 = row['HS_TS2'],specific_heat = 4.187 ) ) %>% as.numeric()
    }
    return(DF)
  })
  
  Master_DF_Mergerd <- reactive({
    DF_main <- Master_DF_2()
    DF_OP <- Operating_DF()
    DF <- full_join(DF_main,DF_OP, by = join_by(DateTime == DateTime))
    DF <- DF[order(DF$DateTime,decreasing = TRUE),]
    return(DF)
  })
  
  Master_DF_plots <- reactive({  
    DF <- Master_DF_2()
    #fill empty time index
    full_datetime_range <- seq.POSIXt(from = min(DF$DateTime), to = max(DF$DateTime), by = "min") #create list of times
    #find times where there is data using minutes only
    newtimes <- setdiff(substr(full_datetime_range,1,15), #ALL TIMES BY ten MINUTE
                          substr(DF$DateTime,1,15) #DATA TIMES BY ten MINUTE
                          )
    #include only those times
    full_datetime_range <- full_datetime_range[substr(full_datetime_range,1,15) %in% newtimes]
    # creat blank df with all the times
    blank_df <- data.frame(DateTime = full_datetime_range) 
    #merge with master table
    DF <- merge(DF, blank_df, by = "DateTime", all = TRUE) 
    return(DF)
  })
    
  
  #####
  #TABLE
  #####
  output$OP_mode_table <- DT::renderDT({
    DF <- Master_DF_2()
    if(LDESS() == 1){
      colreq <- c("DateTime","OP_Mode","State_Explination","DV1","DV2","DV3" ,"DV4","DV5","DV6","DV7","DV8","FS1_FS","FS2_FS","FS3_FS","FS4_FS", "FS5_FS", "FS6_FS")
    }else if(LDESS() == 2){
      colreq <- c("DateTime","OP_Mode","State_Explination","ZV",	"MV1",	"MV2", "FS1_FS","FS2_FS")
    }
    temp <- DF[,colnames(DF) %in% colreq]
    temp <- temp[order(temp$DateTime,decreasing = TRUE),]
    temp$DateTime <- as.POSIXct(temp$DateTime) %>% as.character()
    return(temp)
  },options = list(
    lengthMenu = list(c(10,25, 50, -1), c('25','50','100','All')),
    pageLength = 50))
  
  #####
  #Plots  
  #####   
  #HeatLoad Plots
  ##############
  output$HeatLoad <- renderPlotly({
    DF <- Master_DF_plots()
   
    reqCols <- c("DateTime")
    
    wantedCols <- c("SHB_Electrical_Heat_load", "HP_Brine_Heat_load", "HP_output_Heat_load", "SH_toHX_Heat_load", "SH_fromHX_Heat_load", "HP_toThermino_Heat_load",
                    "SHB_Charging_Heat_load",
                    "SHB_Discharging_Heat_load",
                    "HP_Heat_load",
                    "HW_Heat_load")
    
    thesecolumns <- append(reqCols,wantedCols)
    
    # fill empty time index
    full_datetime_range <- seq.POSIXt(from = min(DF$DateTime), to = max(DF$DateTime), by = "min") #create list of times
    blank_df <- data.frame(DateTime = full_datetime_range) # creat blank df with all the times
    DF <- merge(DF, blank_df, by = "DateTime", all = TRUE) #merge with master table
    

    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    
    fig <- plot_ly()
    if(LDESS() == 1){
      fig <- fig %>% add_trace(x = data[data$variable == "SHB_Electrical_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SHB_Electrical_Heat_load",colnames(data) == "value"],  name = "SHB_Electrical_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "HP_Brine_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HP_Brine_Heat_load",colnames(data) == "value"], name = "HP_Brine_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "HP_output_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HP_output_Heat_load",colnames(data) == "value"], name = "HP_output_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "SH_toHX_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SH_toHX_Heat_load",colnames(data) == "value"], name = "SH_toHX_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "SH_fromHX_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SH_fromHX_Heat_load",colnames(data) == "value"], name = "SH_fromHX_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "HP_toThermino_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HP_toThermino_Heat_load",colnames(data) == "value"], name = "HP_toThermino_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
    }else if(LDESS() == 2){
      fig <- fig %>% add_trace(x = data[data$variable == "SHB_Charging_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SHB_Charging_Heat_load",colnames(data) == "value"], name = "SHB_Charging_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "SHB_Discharging_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SHB_Discharging_Heat_load",colnames(data) == "value"], name = "SHB_Discharging_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "HP_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HP_Heat_load",colnames(data) == "value"], name = "HP_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
      fig <- fig %>% add_trace(x = data[data$variable == "HW_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HW_Heat_load",colnames(data) == "value"], name = "HW_Heat_load", mode = "lines+markers", type = "scatter")   # Add traces
    }
    
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>EXTEND system Heat load<b>", x=0.1, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="Datetime"),
      yaxis = list(title="<b>Heat Load</b>(flow x Specific Heat x ΔHWT)")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
    
  })
  
  output$PowerPlot <- renderPlotly({
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- c("PM4_I",
                      "PM3_I",
                      "PM2_I",
                      "PM1_I")
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "PM1_I",colnames(data) == "DateTime"], y = data[data$variable == "PM1_I",colnames(data) == "value"], name = "Heat Pump", mode = "lines+markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "PM2_I",colnames(data) == "DateTime"], y = data[data$variable == "PM2_I",colnames(data) == "value"], name = "Extend Element", mode = "lines+markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "PM3_I",colnames(data) == "DateTime"], y = data[data$variable == "PM3_I",colnames(data) == "value"], name = "Thermino Element", mode = "lines+markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "PM4_I",colnames(data) == "DateTime"], y = data[data$variable == "PM4_I",colnames(data) == "value"], name = "Ballance of Plant", mode = "lines+markers", type = "scatter")   # Add traces
      
    
    
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>EXTEND system power usage<b>", x=0.1, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="Datetime"),
      yaxis = list(title="<b>Power Readings</b> Current in A")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
    
  })
  
  output$SOCPlot <- renderPlotly({
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- c("HWB_SOC_pct",
                    "CHB_SOC_pct")
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "HWB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "HWB_SOC_pct",colnames(data) == "value"], name = "HWB SOC %", mode = "lines+markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "CHB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "CHB_SOC_pct",colnames(data) == "value"], name = "CHB SOC %", mode = "lines+markers", type = "scatter")   # Add traces
   
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>Battery State of Charge<b>", x=0.1, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="Datetime"),
      yaxis = list(title="<b>Battery SOC</b>")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
    
  })
  
  
  ########
  #HWB Plots
  ########
  output$HWB_top <- renderPlotly({
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- c("HWB_TSB","HWB_TSM","HWB_TST","HWB_SOC_pct")
    wantedCols <- input$metric
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
   
    
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    
    if(any(wantedCols %in% c("HWB_TSB","HWB_TSM","HWB_TST"))){
      if(wantedCols %in% c("HWB_SOC_pct") %>% any){ #two axis plot
        fig <- plot_ly()
        fig <- fig %>% add_trace(x = data[data$variable == "HWB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "HWB_SOC_pct",colnames(data) == "value"],yaxis = "y2", name = "SOC", mode = "lines+markers", type = "scatter") %>% layout(yaxis2 = list(range = c(0,125)))   # Add traces
        if(wantedCols %in% c("HWB_TSB")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSB",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSB",colnames(data) == "value"], name = "TSB", mode = "lines+markers", type = "scatter") %>% layout(yaxis = list(range = c(0,105)))
        }
        if(wantedCols %in% c("HWB_TST")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TST",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TST",colnames(data) == "value"], name = "TST", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("HWB_TSM")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSM",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSM",colnames(data) == "value"], name = "TSM", mode = "lines+markers", type = "scatter")
        }
        
        
        ay <- list(
          tickfont = list(color = "red"),
          overlaying = "y",
          side = "right",
          title = "<b>State of Charge</b> Thermino")
        
        
        
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>Thermino SOC<b>", x=.2, y=1.1), 
          margin = list(l = 30, r = 50, b = 10, t = 40),
          yaxis2 = ay,
          xaxis = list(title="Datetime"),
          yaxis = list(title="<b>Temperature</b> Thermino")
        )%>%
          layout(plot_bgcolor='#e5ecf6',
                 legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
                 xaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
                 yaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'))
        
        
        return(fig)
      }else{ #just one axis(TS)
        fig <- plot_ly()
        if(wantedCols %in% c("HWB_TSB")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSB",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSB",colnames(data) == "value"], name = "TSB", mode = "lines+markers", type = "scatter") %>% layout(yaxis = list(range = c(0,105)))
        }
        if(wantedCols %in% c("HWB_TST")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TST",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TST",colnames(data) == "value"], name = "TST", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("HWB_TSM")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSM",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSM",colnames(data) == "value"], name = "TSM", mode = "lines+markers", type = "scatter")
        }
        
        
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>Thermino SOC<b>", x=.2, y=1.1), 
          margin = list(l = 30, r = 50, b = 10, t = 40),
          xaxis = list(title="Datetime"),
          yaxis = list(title="<b>Temperature</b> Thermino")
        )%>%
          layout(plot_bgcolor='#e5ecf6',
                 legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
                 xaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
                 yaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'))
        
        
        return(fig)
      }
    }else if(wantedCols %in% c("HWB_SOC_pct")){ #just one axis (SOC)
      fig <- plot_ly()
      fig <- fig %>% add_trace(x = data[data$variable == "HWB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "HWB_SOC_pct",colnames(data) == "value"], name = "SOC", mode = "lines+markers", type = "scatter")   %>% layout(yaxis = list(range = c(0,125)))# Add traces
      
      
      
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = list(text = "<b>Thermino SOC<b>", x=.2, y=1.1), 
        margin = list(l = 30, r = 50, b = 10, t = 40),
        xaxis = list(title="Datetime"),
        yaxis = list(title="<b>State of Charge</b> Thermino")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
      
      
      return(fig)
    }
    
  })
  
  output$HWB_bottomer <- renderPlotly({
    DF <- Master_DF_plots()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- input$HWB_PM
  
    
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    fig <- plot_ly()
    if(wantedCols %in% c("PM3_PF")  %>% any()){
        fig <- fig %>% add_trace(x = data[data$variable == "PM3_PF",colnames(data) == "DateTime"], y = data[data$variable == "PM3_PF",colnames(data) == "value"], name = "PM3_PF", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM3_I")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM3_I",colnames(data) == "DateTime"], y = data[data$variable == "PM3_I",colnames(data) == "value"], name = "PM3_I", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM3_V")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM3_V",colnames(data) == "DateTime"], y = data[data$variable == "PM3_V",colnames(data) == "value"], name = "PM3_V", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM3_PWH")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM3_PWH",colnames(data) == "DateTime"], y = data[data$variable == "PM3_PWH",colnames(data) == "value"], name = "PM3_PWH", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM3_PVA")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM3_PVA",colnames(data) == "DateTime"], y = data[data$variable == "PM3_PVA",colnames(data) == "value"], name = "PM3_PVA", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM3_W")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM3_W",colnames(data) == "DateTime"], y = data[data$variable == "PM3_W",colnames(data) == "value"], name = "PM3_W", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM3_FR")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM3_FR",colnames(data) == "DateTime"], y = data[data$variable == "PM3_FR",colnames(data) == "value"], name = "PM3_FR", mode = "lines+markers", type = "scatter")
    }

        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>Thermino Electrical Element<b>", x=.2, y=1.1), 
          margin = list(l = 30, r = 50, b = 10, t = 40),
          xaxis = list(title="Datetime"),
          yaxis = list(title="<b>Power usage</b>")
        )%>%
          layout(plot_bgcolor='#e5ecf6',
                 legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
                 xaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
                 yaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'))
        
        
        return(fig)
      
    
    
  })
  #SHB Plots
  ##########
  output$SHB_top <- renderPlotly({
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG","CHB_SOC_pct")
    wantedCols <- input$SHB_TM
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    
    if(any(wantedCols %in% c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG"))){
      if(wantedCols %in% c("CHB_SOC_pct") %>% any){ #two axis plot
        fig <- plot_ly()
        fig <- fig %>% add_trace(x = data[data$variable == "CHB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "CHB_SOC_pct",colnames(data) == "value"],yaxis = "y2", name = "SOC", mode = "lines+markers", type = "scatter") %>% layout(yaxis2 = list(range = c(0,125)))  # Add traces
        if(wantedCols %in% c("CHB_TS1_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS1_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS1_AVG",colnames(data) == "value"], name = "CHB_TS1_AVG", mode = "lines+markers", type = "scatter") %>% layout(yaxis = list(range = c(0,105)))
        }
        if(wantedCols %in% c("CHB_TS2_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS2_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS2_AVG",colnames(data) == "value"], name = "CHB_TS2_AVG", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS3_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS3_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS3_AVG",colnames(data) == "value"], name = "CHB_TS3_AVG", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS5_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS5_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS5_AVG",colnames(data) == "value"], name = "CHB_TS5_AVG", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS4_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS4_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS4_AVG",colnames(data) == "value"], name = "CHB_TS4_AVG", mode = "lines+markers", type = "scatter")
        }
        
        
        ay <- list(
          tickfont = list(color = "red"),
          overlaying = "y",
          side = "right",
          title = "<b>State of Charge</b> EXTEND")
        
        
        
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>EXTEND SOC<b>", x=.2, y=1.1), 
          margin = list(l = 30, r = 50, b = 10, t = 40),
          yaxis2 = ay,
          xaxis = list(title="Datetime"),
          yaxis = list(title="<b>Temperature</b> EXTEND")
        )%>%
          layout(plot_bgcolor='#e5ecf6',
                 legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
                 xaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
                 yaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'))
        
        
        return(fig)
      }else{ #just one axis(TS)
        fig <- plot_ly()
        if(wantedCols %in% c("CHB_TS1_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS1_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS1_AVG",colnames(data) == "value"], name = "CHB_TS1_AVG", mode = "lines+markers", type = "scatter") %>% layout(yaxis = list(range = c(0,105)))
        }
        if(wantedCols %in% c("CHB_TS2_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS2_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS2_AVG",colnames(data) == "value"], name = "CHB_TS2_AVG", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS3_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS3_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS3_AVG",colnames(data) == "value"], name = "CHB_TS3_AVG", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS5_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS5_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS5_AVG",colnames(data) == "value"], name = "CHB_TS5_AVG", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS4_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS4_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS4_AVG",colnames(data) == "value"], name = "CHB_TS4_AVG", mode = "lines+markers", type = "scatter")
        }
        
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>EXTEND SOC<b>", x=.2, y=1.1), 
          margin = list(l = 30, r = 50, b = 10, t = 40),
          xaxis = list(title="Datetime"),
          yaxis = list(title="<b>Temperature</b> EXTEND")
        )%>%
          layout(plot_bgcolor='#e5ecf6',
                 legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
                 xaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
                 yaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'))
        
        
        return(fig)
      }
    }else if(wantedCols %in% c("CHB_SOC_pct")){ #just one axis (SOC)
      fig <- plot_ly()
      fig <- fig %>% add_trace(x = data[data$variable == "CHB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "CHB_SOC_pct",colnames(data) == "value"], name = "SOC", mode = "lines+markers", type = "scatter")  %>% layout(yaxis = list(range = c(0,125))) # Add traces
      
      
      
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = list(text = "<b>EXTEND SOC<b>", x=.2, y=1.1), 
        margin = list(l = 30, r = 50, b = 10, t = 40),
        xaxis = list(title="Datetime"),
        yaxis = list(title="<b>State of Charge</b> EXTEND")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
      
      
      return(fig)
    }
    
  })
  
  output$SHB_bottom <- renderPlotly({
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- input$SHB_BM
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
     
    if(any(wantedCols %in% c("HS_TS10","HS_TS11","HS_TS12"))){
      if(wantedCols %in% c("FS5_FS") %>% any){ #two axis plot
        fig <- plot_ly()
        fig <- fig %>% add_trace(x = data[data$variable == "FS5_FS",colnames(data) == "DateTime"], y = data[data$variable == "FS5_FS",colnames(data) == "value"],yaxis = "y2", name = "FS5", mode = "lines+markers", type = "scatter")   # Add traces
        if(wantedCols %in% c("HS_TS10")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS10",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS10",colnames(data) == "value"], name = "HS_TS10", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS11")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS11",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS11",colnames(data) == "value"], name = "HS_TS11", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS12")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS12",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS12",colnames(data) == "value"], name = "HS_TS12", mode = "lines+markers", type = "scatter")
        }
        
        
        ay <- list(
          tickfont = list(color = "red"),
          overlaying = "y",
          side = "right",
          title = "<b>Flow</b> EXTEND")
        
        
        
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>EXTEND Hydronics<b>", x=.2, y=1.1), 
          margin = list(l = 30, r = 50, b = 10, t = 40),
          yaxis2 = ay,
          xaxis = list(title="Datetime"),
          yaxis = list(title="<b>Temperature</b> EXTEND Hydronics")
        )%>%
          layout(plot_bgcolor='#e5ecf6',
                 legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
                 xaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
                 yaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'))
        
        
        return(fig)
      }else{ #just one axis(TS)
        fig <- plot_ly()
        if(wantedCols %in% c("HS_TS10")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS10",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS10",colnames(data) == "value"], name = "HS_TS10", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS11")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS11",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS11",colnames(data) == "value"], name = "HS_TS11", mode = "lines+markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS12")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS12",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS12",colnames(data) == "value"], name = "HS_TS12", mode = "lines+markers", type = "scatter")
        }
        
        
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>EXTEND Hydronics<b>", x=.2, y=1.1), 
          margin = list(l = 30, r = 50, b = 10, t = 40),
          xaxis = list(title="Datetime"),
          yaxis = list(title="<b>Temperature</b> EXTEND Hydronics")
        )%>%
          layout(plot_bgcolor='#e5ecf6',
                 legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
                 xaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
                 yaxis = list(
                   zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'))
        
        return(fig)
      }
    }else if(wantedCols %in% c("FS5_FS") %>% any()){ #just one axis (SOC)
      fig <- plot_ly()
      fig <- fig %>% add_trace(x = data[data$variable == "FS5_FS",colnames(data) == "DateTime"], y = data[data$variable == "FS5_FS",colnames(data) == "value"], name = "FS5", mode = "lines+markers", type = "scatter")   # Add traces
      
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = list(text = "<b>EXTEND Hydronics<b>", x=.2, y=1.1), 
        margin = list(l = 30, r = 50, b = 10, t = 40),
        xaxis = list(title="Datetime"),
        yaxis = list(title="<b>Flow</b> EXTEND")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
      return(fig)
    }
    
  })
  
  output$SHB_bottomid <- renderPlotly({
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- input$SHB_BMid
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    fig <- plot_ly()
    
    if(wantedCols %in% c("HS_TS6")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "HS_TS6",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS6",colnames(data) == "value"], name = "TS6", mode = "lines+markers", type = "scatter")   # Add traces
      }
    if(wantedCols %in% c("HS_TS7")  %>% any()){
        fig <- fig %>% add_trace(x = data[data$variable == "HS_TS7",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS7",colnames(data) == "value"], name = "TS7", mode = "lines+markers", type = "scatter")   # Add traces
    }
    if(wantedCols %in% c("HS_TS8")  %>% any()){
        fig <- fig %>% add_trace(x = data[data$variable == "HS_TS8",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS8",colnames(data) == "value"], name = "TS8", mode = "lines+markers", type = "scatter")   # Add traces
    }
    if(wantedCols %in% c("HS_TS9")  %>% any()){
        fig <- fig %>% add_trace(x = data[data$variable == "HS_TS9",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS9",colnames(data) == "value"], name = "TS9", mode = "lines+markers", type = "scatter")   # Add traces
    }
    if(wantedCols %in% c("ROOM_TS19")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "ROOM_TS19",colnames(data) == "DateTime"], y = data[data$variable == "ROOM_TS19",colnames(data) == "value"], name = "TS19", mode = "lines+markers", type = "scatter")   # Add traces
    }
    
    
    
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>Space Heating HX<b> Temperatures", x=.2, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="Datetime"),
      yaxis = list(title="<b>Temperature</b> in C")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
    
  })
  
  output$SHB_bottomer <- renderPlotly({
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- input$SHB_BB
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    fig <- plot_ly()
    if(wantedCols %in% c("PM2_PF")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM2_PF",colnames(data) == "DateTime"], y = data[data$variable == "PM2_PF",colnames(data) == "value"], name = "PM2_PF", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM2_I")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM2_I",colnames(data) == "DateTime"], y = data[data$variable == "PM2_I",colnames(data) == "value"], name = "PM2_I", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM2_V")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM2_V",colnames(data) == "DateTime"], y = data[data$variable == "PM2_V",colnames(data) == "value"], name = "PM2_V", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM2_PWH")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM2_PWH",colnames(data) == "DateTime"], y = data[data$variable == "PM2_PWH",colnames(data) == "value"], name = "PM2_PWH", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM2_PVA")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM2_PVA",colnames(data) == "DateTime"], y = data[data$variable == "PM2_PVA",colnames(data) == "value"], name = "PM2_PVA", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM2_FR")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM2_FR",colnames(data) == "DateTime"], y = data[data$variable == "PM2_FR",colnames(data) == "value"], name = "PM2_FR", mode = "lines+markers", type = "scatter")
    }
    if(wantedCols %in% c("PM2_PW")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM2_PW",colnames(data) == "DateTime"], y = data[data$variable == "PM2_PW",colnames(data) == "value"], name = "PM2_PW", mode = "lines+markers", type = "scatter")
    }
    
    
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>EXTEND Electrical Element<b>", x=.2, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="Datetime"),
      yaxis = list(title="<b>Power usage</b> ")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    

  })
  #####
  #Schedule plots
  #####
  output$OP_Schedule_plot <- renderPlotly({
    DF <- Operating_DF()
    
    y1 <- c(
        "HP_SH_Priority",
        "HP_SHB_Priority",
        "HP_HWB_Priority",
        "HP_Source_Battery",
        "SHB_Direct"
      
    )
    
    DF <- DF[,colnames(DF) %in% append(y1,"DateTime")]
    
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #two axis plot
    
    fig <- plot_ly()
    for(i in y1){
      fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "markers", type = "scatter")   # Add traces
    }
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>Operating Schedule<b> Heat Pump and Space Heating", x=.01, y=1.4),
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="DateTime"),
       yaxis = list(title="<b>Priority</b>",tickvals = c(1:3), range =c(.5,3.5))
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
  })
  
  
  
  output$OP_Schedule_plot2 <- renderPlotly({
    DF <-  Operating_DF()
    
    y1 <-c("HWB_Elec_IN_KW_min",
           "HWB_Elec_IN_KW_max",
           "HP_OUT_KW",
           "HP_IN_KW",
           "SHB_min_IN_KW",
           "SHB_max_IN_KW"
    )
    
    
    DF <- DF[order(DF$DateTime),]
    DF <- DF[,colnames(DF) %in% append(y1,"DateTime")]

    
    
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #two axis plot
    
    fig <- plot_ly()
    for(i in y1){
      fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "lines+markers", type = "scatter")   # Add traces
    }
    
    fig <- fig %>% layout(
      title = list(text = "<b>Operating Schedule<b> SHB and HWB", x=.01, y=1.1),
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="DateTime"),
      # yaxis2 = ay,
      yaxis = list(title="<b>Power</b> kW")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
  })
  
  output$OP_Schedule_plot3 <- renderPlotly({ 
    DF <-  Operating_DF()
  
  
  
  y1 <-c("consumption_Schedule",
          "Agreed_Consumption_schedule"
         
         )
  
  
  DF <- DF[order(DF$DateTime),]
  
  
  
  
  data <- reshape2::melt(DF,id.var = c("DateTime"))
  #two axis plot
  
  fig <- plot_ly()
  for(i in y1){
    fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "lines+markers", type = "scatter")   # Add traces
  }
  
  fig <- fig %>% layout(
    title = list(text = "<b>Operating Schedule<b> SHB and HWB", x=.01, y=1.1),
    margin = list(l = 30, r = 50, b = 10, t = 40),
    xaxis = list(title="DateTime"),
    yaxis = list(title="<b>Power</b> kW")
  )%>%
    layout(plot_bgcolor='#e5ecf6',
           legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'))
  
  
  return(fig)
  
    
  })
  output$OP_Schedule_plot4 <- renderPlotly({ 
    DF <-  Operating_DF()
    
    
    
    y1 <-c("SH_Setpoint",
           "hot_water_user_schedule" ,
           "space_heating_user_schedule"
           
    )
    
    y2 <- c("HWB Recharge SOC")
    reqcol <- append(y1,y2)
    DF <- DF[,colnames(DF) %in% append("DateTime",reqcol)]
    DF <- DF[order(DF$DateTime),]
    
    
    DF$hot_water_user_schedule <- DF$hot_water_user_schedule * 10
    
    
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #two axis plot
    
    fig <- plot_ly()
    for(i in y1){
      fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "lines+markers", type = "scatter")   # Add traces
    }
    for(i in y2){
      fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], yaxis = "y2", name = i, mode = "lines+markers", type = "scatter")   # Add traces
    }
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "SOC")
    #Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>Heating Schedule<b> Hot water and Space Heating", x=.01, y=1.1),
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="DateTime"),
      yaxis2 = ay,
      yaxis = list(title="<b>Power</b> kW")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
  })
  output$OP_Schedule_plot5 <- renderPlotly({ 
    DF <-  Operating_DF()
    
    
    
    y1 <-c("TOU_VAT","TOU_NoVAT")
    
    y2 <- c("outside_temperature_forecast")
    reqcol <- append(y1,y2)
    DF <- DF[,colnames(DF) %in% append("DateTime",reqcol)]
    DF <- DF[order(DF$DateTime),]
    
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #two axis plot
    
    fig <- plot_ly()
    for(i in y1){
      fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "lines+markers", type = "scatter")   # Add traces
    }
    for(i in y2){
      fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], yaxis = "y2", name = i, mode = "lines+markers", type = "scatter")   # Add traces
    }
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Temperature ")
    #Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>Heating Schedule<b> Hot water and Space Heating", x=.01, y=1.1),
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="DateTime"),
      yaxis2 = ay,
      yaxis = list(title="<b>TOU</b>")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
    
    
  })
  #######
  #Hydronics plot
  #######
  output$valveplot <- renderPlotly({
    DF <- Master_DF_plots()
    
    DF <- DF[,colnames(DF) %in% c("DateTime","ZV","MV1","MV2")]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    
    
    fig <- plot_ly()
    
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "ZV",colnames(data) == "DateTime"], y = data[data$variable == "ZV",colnames(data) == "value"], yaxis= "y2", name = "ZV", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "MV1",colnames(data) == "DateTime"], y = data[data$variable == "MV1",colnames(data) == "value"], name = "MV1", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "MV2",colnames(data) == "DateTime"], y = data[data$variable == "MV2",colnames(data) == "value"], name = "MV2", mode = "markers", type = "scatter")   # Add traces
    
    # Set figure title, x and y-axes titles
    
    ay <- list(
      tickfont = list(color = "red"),
      type = "linear",
      overlaying = "y",
      side = "right",
      title = "<b>Zone Valve Status")
    
    fig <- fig %>% layout(
      title = list(text = "<b>MV and ZV reading<b>", x=.2, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      yaxis2 = ay,
      xaxis = list(title="DateTime"),
      yaxis = list(title="<b>MV Status</b> ")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    
    return(fig)
  })
  
  output$Hydronicsplot <- renderPlotly({
    imputs <- input$Hydronics_im
    temper_inputs <- imputs[imputs %in% c("HP_TS16",	
                                          "HP_TS17",	
                                          "HP_TS18", 
                                          "ROOM_TS19",
                                          "HS_TS1",
                                          "HS_TS2",	
                                          "HS_TS3",	
                                          "HS_TS4"	,
                                          "HS_TS5"	,
                                          "HS_TS6"	,
                                          "HS_TS7")]
    other_inputs <- imputs[imputs %in% c("FS1_FS"	,"FS2_FS","PUMP_1")]
    
    DF <- Master_DF_plots()
    reqCols <- c("DateTime")
    wantedCols <- c(imputs)
    
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    
    
    fig <- plot_ly()
    if(length(other_inputs)>=1){
      for(i in other_inputs){
        fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], yaxis= "y2", name = i, mode = "lines+markers", type = "scatter")
      }
      if(length(temper_inputs) >=1){
        for(i in temper_inputs){
          fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "lines+markers", type = "scatter")
        }
      }else{ #just put someting on the LHS axis
        fig <- fig %>% add_trace(x = data[data$variable == "ROOM_TS19",colnames(data) == "DateTime"], y = data[data$variable == "ROOM_TS19",colnames(data) == "value"], name = "ROOM_TS19", mode = "lines+markers", type = "scatter")
      }
      ay <- list(
        tickfont = list(color = "red"),
        type = "linear",
        overlaying = "y",
        side = "right",
        title = "<b>Flow Sensor</b>reading")
      
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = list(text = "<b>Hydronics TS<b>", x=.2, y=1.1), 
        margin = list(l = 30, r = 50, b = 10, t = 40),
        yaxis2 = ay,
        xaxis = list(title="DateTime"),
        yaxis = list(title="<b>Temperature</b> ")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
      
    }else{
      for(i in temper_inputs){
        fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "lines+markers", type = "scatter")
      }
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = list(text = "<b>Hydronics_TS<b>", x=.2, y=1.1), 
        margin = list(l = 30, r = 50, b = 10, t = 40),
        #yaxis2 = ay,
        xaxis = list(title="DateTime"),
        yaxis = list(title="<b>Temperature</b> ")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
    }
    
    
    
    
    
    return(fig)
  })
  
  output$OPM_Plot <- renderPlotly({
    DF <- Master_DF_plots()
    
    DF <- DF[,colnames(DF) %in% c("DateTime","OP_Mode")]
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = DF$DateTime, y = DF$OP_Mode, name = "OP Mode", mode = "markers", type = "scatter")
    fig <- fig %>% layout(
      title = list(text = "<b>OPM<b>", x=.2, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40)
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    return(fig)
  })
  
  #######
  #Master Plot
  ########
   observeEvent(input$plotButton, {
     output$Master_Plot <- renderPlotly({
   
      
    DF <- Master_DF_Mergerd()
    y1vars <- isolate(input$Master_Y1_LDESS_2)
    y1multvars <- input$Master_Y1_mult_LDESS_2 %>% isolate()
    y2vars <- input$Master_Y2_LDESS_2 %>% isolate()
    multiplier <- input$MasterMultiplier %>% as.numeric() %>% isolate()
    reqcols <- append(y1vars,y1multvars)    
    reqcols <- append(reqcols, y2vars)
    reqcols <- append(reqcols,"DateTime")
    DF <- DF[,colnames(DF) %in% unique(reqcols)]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    
    
    fig <- plot_ly()
    for(i in y1vars){
      fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], name = i, mode = "lines+markers", type = "scatter")
    }
    if(length(y1multvars) >= 1){
      for(i in y1multvars){
        fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"] * multiplier, name = i, mode = "lines+markers", type = "scatter")
      }
    }
    if(length(y2vars) >=1){
      for(i in y2vars){
        fig <- fig %>% add_trace(x = data[data$variable == i,colnames(data) == "DateTime"], y = data[data$variable == i,colnames(data) == "value"], yaxis = "y2", name = i, mode = "lines+markers", type = "scatter")
      }
      ay <- list(
        tickfont = list(color = "red"),
        type = "linear",
        overlaying = "y",
        side = "right",
        title = "<b>Y2<b>")
      
      fig <- fig %>% layout(
        title = list(text = "<b>Plot<b>", x=.2, y=1.1), 
        margin = list(l = 30, r = 50, b = 10, t = 40),
        yaxis2 = ay,
        xaxis = list(title="DateTime"),
        yaxis = list(title="<b>Y1</b> ")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
      
    }else{
      fig <- fig %>% layout(
        title = list(text = "<b>Plot<b>", x=.2, y=1.1), 
        margin = list(l = 30, r = 50, b = 10, t = 40),
        #yaxis2 = ay,
        xaxis = list(title="DateTime"),
        yaxis = list(title="<b>Y1</b> ")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               legend = list(orientation = "h", xanchor = "center", x = 0.7, y = 1.1),
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'))
    }
    
    
    
    
    return(fig)
    })
  })
  #####
  #Output file
  #####
  output$downloadXLS <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(),"%d_%m_%Y"),"LDESS_data", ".xlsx")
    },
    content = function(file) {
      
      object <- createWorkbook()
        #create worksheet for CSV
        addWorksheet(wb=object, sheetName = "data")
        #write Raw data to page
        writeData(object, sheet ="data", x = Master_DF_Mergerd() )
        
        #save workbook
      saveWorkbook(object, file =  file, overwrite = TRUE)
      
        
    }
  )
  
}

#shinyApp(ui,server)

