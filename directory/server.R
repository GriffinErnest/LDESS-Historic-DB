#Write server output
server <- function(input, output, session){
  #####
  #wip code
  #####
  #BOOKMARK FOR NOW
  #text to display for date inputs
  # output$DateRange <- renderText({
  #   # make sure end date later than start date
  #   validate(
  #     need(input$dates[2] > input$dates[1], "end date is earlier than start date"
  #     )
  #   )
  #   # paste("Date Range:",
  #   #       difftime(input$dates[2], input$dates[1], units="days") %>% as.character(),
  #   #       "days")
  # })
  
  # # fill empty time index
  # full_datetime_range <- seq.POSIXt(from = min(DF$DateTime), to = max(DF$DateTime), by = "min") #create list of times
  # blank_df <- data.frame(DateTime = full_datetime_range) # creat blank df with all the times
  # DF <- merge(DF, blank_df, by = "DateTime", all = TRUE) #merge with master table
  # #####
  #Data Extraction  
  #####

  Master_DF <- reactive({
    #db information:
      db <- 'ldess_db'
      host_db <- '172.187.185.55'  # e.g., 'ec2-54-83-201-96.compute-1.amazonaws.com'
      db_port <- '5432'  # Specify the port (e.g., 5432)
      db_user <- 'ldess_hist_dashboard'
      db_password <- 'SIE09375SDKJF'
      schema_name <- input$EXTEND_ID %>% tolower()
      #schema_name <- "ext0007" #FOR TESTING
      table_name <- "sensor_logs"
      
    #query information
      enddate <- input$dates[2] %>% as.character()
      startdate <- input$dates[1] %>% as.character()
      #enddate <- as.character(Sys.Date())
      #startdate <- as.POSIXct("2024-06-10",tz = 'UTC')
   
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
    })
  
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
   
    
    return(DF) 
    
    
  })
  
  #calculated columns
  Master_DF_2 <- reactive({  
    DF <- Master_DF_2_pre()
    
    
    
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
    #change SOC variables to numeric
    DF$HWB_SOC_pct <- DF$HWB_SOC_pct %>% as.numeric()
    DF$CHB_SOC_pct <- DF$CHB_SOC_pct %>% as.numeric()
    
    #clean operating mode
    Mode_mapping <- read_excel("register_mapping.xlsx",sheet = "Op_mode")
    
    DF$Hex_mode <- as.hexmode(DF$SYS_CURR_OPM)
    DF$OP_Mode <- lapply(DF$Hex_mode, function(x) {
      Mode_mapping$State[match(x, Mode_mapping$Reading)]
    })
    
    
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
    DF$SHB_Electrical_Heat_load <- DF$FS5_FS * specific_heat_PF * abs(DF$HS_TS11  - DF$HS_TS10)
    DF$HP_Brine_Heat_load <- DF$FS6_FS * specific_heat_PF * abs(DF$HP_TS14 - DF$HP_TS15)
    DF$HP_output_Heat_load <- DF$FS6_FS * specific_heat_PF * abs(DF$HP_TS17 - DF$HP_TS16)
    DF$SH_toHX_Heat_load <- DF$FS2_FS * specific_heat_PF * abs(DF$HS_TS7 - DF$HS_TS6)
    DF$SH_fromHX_Heat_load <- DF$FS4_FS * specific_heat_water * abs(DF$HS_TS8 - DF$HS_TS9)
    DF$HP_toThermino_Heat_load <- DF$FS2_FS * specific_heat_PF * abs(DF$HS_TS3 - DF$HS_TS2)
    return(DF)
  })
    
  
  #####
  #TABLE
  #####
  output$OP_mode_table <- renderDataTable({
    DF <- Master_DF_2()
    
    temp <- DF[,colnames(DF) %in% c("DateTime","OP_Mode","DV1","DV2","DV3" ,"DV4","DV5","DV6","DV7","DV8", "MV1","MV2", "MV3")]
    temp <- temp[order(temp$DateTime,decreasing = TRUE),]
    temp$DateTime <- as.POSIXct(temp$DateTime) %>% as.character()
    return(temp)
  })
  
  #####
  #Plots  
  #####   
  #HeatLoad Plots
  ##############
  output$HeatLoad <- renderPlotly({
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- c("SHB_Electrical_Heat_load", "HP_Brine_Heat_load", "HP_output_Heat_load", "SH_toHX_Heat_load", "SH_fromHX_Heat_load", "HP_toThermino_Heat_load" )
    thesecolumns <- append(reqCols,wantedCols)
    
    # fill empty time index
    full_datetime_range <- seq.POSIXt(from = min(DF$DateTime), to = max(DF$DateTime), by = "min") #create list of times
    blank_df <- data.frame(DateTime = full_datetime_range) # creat blank df with all the times
    DF <- merge(DF, blank_df, by = "DateTime", all = TRUE) #merge with master table
    

    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "SHB_Electrical_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SHB_Electrical_Heat_load",colnames(data) == "value"], name = "SHB_Electrical_Heat_load", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "HP_Brine_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HP_Brine_Heat_load",colnames(data) == "value"], name = "HP_Brine_Heat_load", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "HP_output_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HP_output_Heat_load",colnames(data) == "value"], name = "HP_output_Heat_load", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "SH_toHX_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SH_toHX_Heat_load",colnames(data) == "value"], name = "SH_toHX_Heat_load", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "SH_fromHX_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "SH_fromHX_Heat_load",colnames(data) == "value"], name = "SH_fromHX_Heat_load", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "HP_toThermino_Heat_load",colnames(data) == "DateTime"], y = data[data$variable == "HP_toThermino_Heat_load",colnames(data) == "value"], name = "HP_toThermino_Heat_load", mode = "markers", type = "scatter")   # Add traces
    
    
    
    
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
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- c("PM4_I",
                      "PM3_I",
                      "PM2_I",
                      "PM1_I")
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "PM1_I",colnames(data) == "DateTime"], y = data[data$variable == "PM1_I",colnames(data) == "value"], name = "Heat Pump", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "PM2_I",colnames(data) == "DateTime"], y = data[data$variable == "PM2_I",colnames(data) == "value"], name = "Extend Element", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "PM3_I",colnames(data) == "DateTime"], y = data[data$variable == "PM3_I",colnames(data) == "value"], name = "Thermino Element", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "PM4_I",colnames(data) == "DateTime"], y = data[data$variable == "PM4_I",colnames(data) == "value"], name = "Ballance of Plant", mode = "markers", type = "scatter")   # Add traces
      
    
    
    
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
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN) 
    wantedCols <- c("HWB_SOC_pct",
                    "CHB_SOC_pct")
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "HWB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "HWB_SOC_pct",colnames(data) == "value"], name = "HWB SOC %", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "CHB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "CHB_SOC_pct",colnames(data) == "value"], name = "CHB SOC %", mode = "markers", type = "scatter")   # Add traces
   
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>EXTEND system power usage<b>", x=0.1, y=1.1), 
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
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- c("HWB_TSB","HWB_TSM","HWB_TST","HWB_SOC_pct")
    #FOR TESTING
    wantedCols <- input$metric
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
   
    
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    
    if(any(wantedCols %in% c("HWB_TSB","HWB_TSM","HWB_TST"))){
      if(wantedCols %in% c("HWB_SOC_pct") %>% any){ #two axis plot
        fig <- plot_ly()
        fig <- fig %>% add_trace(x = data[data$variable == "HWB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "HWB_SOC_pct",colnames(data) == "value"],yaxis = "y2", name = "SOC", mode = "markers", type = "scatter")   # Add traces
        if(wantedCols %in% c("HWB_TSB")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSB",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSB",colnames(data) == "value"], name = "TSB", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HWB_TST")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TST",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TST",colnames(data) == "value"], name = "TST", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HWB_TSM")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSM",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSM",colnames(data) == "value"], name = "TSM", mode = "markers", type = "scatter")
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
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSB",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSB",colnames(data) == "value"], name = "TSB", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HWB_TST")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TST",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TST",colnames(data) == "value"], name = "TST", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HWB_TSM")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HWB_TSM",colnames(data) == "DateTime"], y = data[data$variable == "HWB_TSM",colnames(data) == "value"], name = "TSM", mode = "markers", type = "scatter")
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
      fig <- fig %>% add_trace(x = data[data$variable == "HWB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "HWB_SOC_pct",colnames(data) == "value"], name = "SOC", mode = "markers", type = "scatter")   # Add traces
      
      
      
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
  
  #'without choise
  output$HWB_bottom <- renderPlotly({
      DF <- Master_DF_2()
      # plottemp <- temp
      reqCols <- c("DateTime")
      # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
      # thesecolumns <- append(reqCols,IN)
     wantedCols <- c("FS3_FS","HS_TS4","HS_TS5")
      thesecolumns <- append(reqCols,wantedCols)

      DF <- DF[,colnames(DF) %in% thesecolumns]
      data <- reshape2::melt(DF,id.var = c("DateTime"))
      #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
      #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()

          fig <- plot_ly()
          fig <- fig %>% add_trace(x = data[data$variable == "FS3_FS",colnames(data) == "DateTime"], y = data[data$variable == "FS3_FS",colnames(data) == "value"],yaxis = "y2", name = "FS3", mode = "markers", type = "scatter")   # Add traces
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS4",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS4",colnames(data) == "value"], name = "HS_TS4", mode = "markers", type = "scatter")
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS5",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS5",colnames(data) == "value"], name = "HS_TS5", mode = "markers", type = "scatter")
          


          ay <- list(
            tickfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "<b>Flow</b> Thermino")



          # Set figure title, x and y-axes titles
          fig <- fig %>% layout(
            title = list(text = "<b>Thermino Hydronics: hot water draw<b>", x=.2, y=1.1),
            margin = list(l = 30, r = 50, b = 10, t = 40),
            yaxis2 = ay,
            xaxis = list(title="Datetime"),
            yaxis = list(title="<b>Temperature</b> Thermino Hydronics")
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
  
 
  output$HWB_bottomid <- renderPlotly({
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- c("FS2_FS","HS_TS2","HS_TS3")
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "FS2_FS",colnames(data) == "DateTime"], y = data[data$variable == "FS2_FS",colnames(data) == "value"],yaxis = "y2", name = "FS2", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "HS_TS2",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS2",colnames(data) == "value"], name = "HS_TS2", mode = "markers", type = "scatter")   # Add traces
    fig <- fig %>% add_trace(x = data[data$variable == "HS_TS3",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS3",colnames(data) == "value"], name = "HS_TS3", mode = "markers", type = "scatter")   # Add traces
  
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "<b>Flow</b> Thermino")
    
    
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>Thermino Hydronics: hot water charge<b>", x=.2, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      yaxis2 = ay,
      xaxis = list(title="Datetime"),
      yaxis = list(title="<b>Temperature</b> Thermino Hydronics")
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
  output$HWB_bottomer <- renderPlotly({
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- input$HWB_PM
   c("PM1_PF","PM1_I","PM1_V","PM1_PWH","PM1_PVA","PM1_W","PM1_FR")
    
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    fig <- plot_ly()
    if(wantedCols %in% c("PM1_PF")  %>% any()){
        fig <- fig %>% add_trace(x = data[data$variable == "PM1_PF",colnames(data) == "DateTime"], y = data[data$variable == "PM1_PF",colnames(data) == "value"], name = "PM1_PF", mode = "markers", type = "scatter")
    }
    if(wantedCols %in% c("PM1_I")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM1_I",colnames(data) == "DateTime"], y = data[data$variable == "PM1_I",colnames(data) == "value"], name = "PM1_I", mode = "markers", type = "scatter")
    }
    if(wantedCols %in% c("PM1_V")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM1_V",colnames(data) == "DateTime"], y = data[data$variable == "PM1_V",colnames(data) == "value"], name = "PM1_V", mode = "markers", type = "scatter")
    }
    if(wantedCols %in% c("PM1_PWH")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM1_PWH",colnames(data) == "DateTime"], y = data[data$variable == "PM1_PWH",colnames(data) == "value"], name = "PM1_PWH", mode = "markers", type = "scatter")
    }
    if(wantedCols %in% c("PM1_PVA")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM1_PVA",colnames(data) == "DateTime"], y = data[data$variable == "PM1_PVA",colnames(data) == "value"], name = "PM1_PVA", mode = "markers", type = "scatter")
    }
    if(wantedCols %in% c("PM1_W")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM1_W",colnames(data) == "DateTime"], y = data[data$variable == "PM1_W",colnames(data) == "value"], name = "PM1_W", mode = "markers", type = "scatter")
    }
    if(wantedCols %in% c("PM1_FR")  %>% any()){
      fig <- fig %>% add_trace(x = data[data$variable == "PM1_FR",colnames(data) == "DateTime"], y = data[data$variable == "PM1_FR",colnames(data) == "value"], name = "PM1_FR", mode = "markers", type = "scatter")
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
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG","CHB_SOC_pct")
    #FOR TESTING
    wantedCols <- input$SHB_TM
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    
    if(any(wantedCols %in% c("CHB_TS1_AVG","CHB_TS2_AVG","CHB_TS3_AVG","CHB_TS4_AVG","CHB_TS5_AVG"))){
      if(wantedCols %in% c("CHB_SOC_pct") %>% any){ #two axis plot
        fig <- plot_ly()
        fig <- fig %>% add_trace(x = data[data$variable == "CHB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "CHB_SOC_pct",colnames(data) == "value"],yaxis = "y2", name = "SOC", mode = "markers", type = "scatter")   # Add traces
        if(wantedCols %in% c("CHB_TS1_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS1_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS1_AVG",colnames(data) == "value"], name = "CHB_TS1_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS2_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS2_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS2_AVG",colnames(data) == "value"], name = "CHB_TS2_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS3_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS3_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS3_AVG",colnames(data) == "value"], name = "CHB_TS3_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS5_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS5_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS5_AVG",colnames(data) == "value"], name = "CHB_TS5_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS4_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS4_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS4_AVG",colnames(data) == "value"], name = "CHB_TS4_AVG", mode = "markers", type = "scatter")
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
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS1_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS1_AVG",colnames(data) == "value"], name = "CHB_TS1_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS2_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS2_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS2_AVG",colnames(data) == "value"], name = "CHB_TS2_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS3_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS3_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS3_AVG",colnames(data) == "value"], name = "CHB_TS3_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS5_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS5_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS5_AVG",colnames(data) == "value"], name = "CHB_TS5_AVG", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("CHB_TS4_AVG")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "CHB_TS4_AVG",colnames(data) == "DateTime"], y = data[data$variable == "CHB_TS4_AVG",colnames(data) == "value"], name = "CHB_TS4_AVG", mode = "markers", type = "scatter")
        }
        
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          title = list(text = "<b>EXTEMD SOC<b>", x=.2, y=1.1), 
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
      fig <- fig %>% add_trace(x = data[data$variable == "CHB_SOC_pct",colnames(data) == "DateTime"], y = data[data$variable == "CHB_SOC_pct",colnames(data) == "value"], name = "SOC", mode = "markers", type = "scatter")   # Add traces
      
      
      
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
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    # wantedCols <- c("HS_TS10","HS_TS11","HS_TS12","FS5_FS")
    #FOR TESTING
    wantedCols <- input$SHB_BM
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    
    if(any(wantedCols %in% c("HS_TS10","HS_TS11","HS_TS12"))){
      if(wantedCols %in% c("FS5_FS") %>% any){ #two axis plot
        fig <- plot_ly()
        fig <- fig %>% add_trace(x = data[data$variable == "FS5_FS",colnames(data) == "DateTime"], y = data[data$variable == "FS5_FS",colnames(data) == "value"],yaxis = "y2", name = "FS5", mode = "markers", type = "scatter")   # Add traces
        if(wantedCols %in% c("HS_TS10")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS10",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS10",colnames(data) == "value"], name = "HS_TS10", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS11")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS11",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS11",colnames(data) == "value"], name = "HS_TS11", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS12")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS12",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS12",colnames(data) == "value"], name = "HS_TS12", mode = "markers", type = "scatter")
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
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS10",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS10",colnames(data) == "value"], name = "HS_TS10", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS11")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS11",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS11",colnames(data) == "value"], name = "HS_TS11", mode = "markers", type = "scatter")
        }
        if(wantedCols %in% c("HS_TS12")  %>% any()){
          fig <- fig %>% add_trace(x = data[data$variable == "HS_TS12",colnames(data) == "DateTime"], y = data[data$variable == "HS_TS12",colnames(data) == "value"], name = "HS_TS12", mode = "markers", type = "scatter")
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
      fig <- fig %>% add_trace(x = data[data$variable == "FS5_FS",colnames(data) == "DateTime"], y = data[data$variable == "FS5_FS",colnames(data) == "value"], name = "FS5", mode = "markers", type = "scatter")   # Add traces
      
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
  output$SHB_bottomer <- renderPlotly({
    DF <- Master_DF_2()
    # plottemp <- temp
    reqCols <- c("DateTime")
    # IN <- c("delta_Immersion_Heater_Energy_Consumed", "Hot_Water_Flow_Temperature")
    # thesecolumns <- append(reqCols,IN)
    wantedCols <- c("PM2_PVA")
    thesecolumns <- append(reqCols,wantedCols)
    
    DF <- DF[,colnames(DF) %in% thesecolumns]
    data <- reshape2::melt(DF,id.var = c("DateTime"))
    #data$temp <- ifelse(data$variable %in% c("HWB_TSB","HWB_TSM","HWB_TST"), data$value, NA) %>% as.numeric()
    #data$soc <- ifelse(data$variable %in% c("HWB_SOC_pct"), data$value,NA) %>% as.numeric()
    fig <- plot_ly()
    fig <- fig %>% add_trace(x = data[data$variable == "PM2_PVA",colnames(data) == "DateTime"], y = data[data$variable == "PM2_PVA",colnames(data) == "value"], name = "PM2_PVA", mode = "markers", type = "scatter")   # Add traces
    
    
    
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = list(text = "<b>EXTEND Electrical Element<b>", x=.2, y=1.1), 
      margin = list(l = 30, r = 50, b = 10, t = 40),
      xaxis = list(title="Datetime"),
      yaxis = list(title="<b>Power usage</b> in VA")
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
        writeData(object, sheet ="data", x = Master_DF_2() )
        
        #save workbook
      saveWorkbook(object, file =  file, overwrite = TRUE)
      
        
    }
  )
  
}

#shinyApp(ui,server)

