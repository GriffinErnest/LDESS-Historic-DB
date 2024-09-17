HoursBack <- input$HoursBack
#FOR TESTING
# HoursBack <- 10
#use hours back to define timeframe
current_DT <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

current_DT <- as.POSIXct(current_DT) -days(1)
current_date <- format(as.POSIXct(current_DT, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
current_hour <- "00"#format(as.POSIXct(current_DT, format = "%Y-%m-%d %H:%M:%S"), "%H")
current_min <- "00"#format(as.POSIXct(current_DT, format = "%Y-%m-%d %H:%M:%S"), "%M")
current_sec <- "00" #format(as.POSIXct(current_DT, format = "%Y-%m-%d %H:%M:%S"), "%S")

past_DT <- format(as.POSIXct(current_DT) - days(14), "%Y-%m-%d %H:%M:%S") 
past_date <- format(as.POSIXct(past_DT, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d") 
past_hour <- "00"#format(as.POSIXct(past_DT, format = "%Y-%m-%d %H:%M:%S"), "%H")
past_min <- "00"#format(as.POSIXct(past_DT, format = "%Y-%m-%d %H:%M:%S"), "%M")
past_sec <- "00" #format(as.POSIXct(past_DT, format = "%Y-%m-%d %H:%M:%S"), "%S")


#maintenence log
#url <- paste0("http://89.197.203.66:3000/sc_maintenance_log?dateFrom=",past_date,"T",past_hour,":",past_min,":",past_sec,"Z&dateTo=",current_date,"T",current_hour,":",current_min,":",current_sec,"Z") #PRODUCTION
#FOR TESTING #url <- "http://192.168.1.106:3000/sc_maintenance_log?dateFrom=2024-06-07T00:00:00Z&dateTo=2024-06-07T08:00:00Z"
#FOR TESTING #url <- "http://89.197.203.66:3000/sc_maintenance_log?dateFrom=2024-05-28T00:00:00Z&dateTo=2024-05-28T16:00:00Z"
url <- paste0("http://192.168.1.106:3000/sc_maintenance_log?dateFrom=",past_date,"T",past_hour,":",past_min,":",past_sec,"Z&dateTo=",current_date,"T",current_hour,":",current_min,":",current_sec,"Z")


handle <- new_handle()
res <- curl(url = url, handle = handle)
json <- fromJSON(res,flatten = TRUE)
DF <- as.data.frame(json)

# Unnest the list column
DF <- DF %>% unnest_wider(input_registers,names_sep = ",")
DF <- DF %>% unnest_wider(discrete_registers ,names_sep = ",")

setwd("C:/Users/griffin.ernest/Desktop")

colnames(DF) <- NULL

DF <- DF[,c(1:(ncol(DF)-1))] #drop random last column 

write.csv(DF, file = "sc_maintenance_log.csv",col.names = FALSE,row.names = FALSE)
