
library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(lubridate)
library(DBI)
library(odbc)


#1) Connect to server 

cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Script began executing.\n")

con = dbConnect(
  odbc(),
  Driver = "/opt/homebrew/lib/libmsodbcsql.18.dylib",
  Server = "resdbsprdhs02.adms.luc.edu",
  Database = "clearlab_purpleair",
  UID = Sys.getenv("DB_USERNAME"),
  PWD = Sys.getenv("DB_PASSWORD"),
  Encrypt = "yes",
  TrustServerCertificate = "yes"
)

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Connected to database\n"
)


#2) Retrieve site names of active Chicago EPA sensors

active_sensors<-dbGetQuery(con, "SELECT * FROM EPA_sensor_information WHERE sensor_activity_status='Active'")
site_names_vec<-active_sensors$site_name

#3) Fetch Live API Timestamps

#I cannot do this step with EPA sensors because, like the Clarity and AirGradient APIs, the last_seen variable does not natively exist in the EPA API like it does in the PurpleAir API.


#4) Fetch and Process Historical Data from the most recent time in the database until the current system time for EPA Chicago sensors


if (length(site_names_vec) > 0) { 
  
  latest_time <- DBI::dbGetQuery(
    con,
    "SELECT MAX(datetime_observed) AS latest_time
   FROM EPA_air_history"
  )$latest_time

  
  beginning_time <- format(latest_time + lubridate::hours(1),
    "%Y-%m-%dT%H" #Notice that we don't include the minute and second portions anymore, because the EPA historical data is hourly, so we need both timestamps to strictly be hourly to match the format of timestamps from the AirNow API
  )

  
  current_time <- format(
    Sys.time(),
    "%Y-%m-%dT%H",
    tz = "UTC"
  )
}

EPA_data<-get_airnow_monitors(box=c(-87.94009, 41.64458, -87.52509, 42.02295), 
                                        parameters=c("pm25", "ozone", "pm10"), 
                                        start_time= beginning_time,
                                        end_time= current_time,
                                        monitor_type="both",
                                        data_type="both",
                                        verbose=TRUE,
                                        raw_concentrations=TRUE,
                                        clean_names=TRUE,
                                        api_key = get_airnow_key())

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Downloaded", nrow(EPA_data), "historical observations\n"
)

EPA_data$raw_concentration=ifelse(EPA_data$raw_concentration==-999, NA, EPA_data$raw_concentration) #Changing -999 values to NA to avoid a number messing up an analysis
EPA_data$value=ifelse(EPA_data$value==-999, NA, EPA_data$value)
EPA_data$aqi=ifelse(EPA_data$aqi==-999, NA, EPA_data$aqi)
EPA_data$category_number=ifelse(EPA_data$category_number==-999, NA, EPA_data$category_number)


EPA_historical_readings<-EPA_data%>%
  select(site_name, datetime_observed, parameter, unit, raw_concentration, value, aqi, category_number)


site_names_pulled_log<-c()
for (i in unique(EPA_historical_readings$site_name)){
  if(i %in% site_names_vec){
  site_names_pulled_log <- c(site_names_pulled_log, as.character(i)) 
  site_names_string <- paste(site_names_pulled_log, collapse = ", ")}
} #Tracks which of the site names have new historical data


#5. Update trigger_pulls table 

trigger_log_df <- data.frame(
  pull_timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  rows_inserted     = as.integer(nrow(EPA_historical_readings)),
  sensors_processed = site_names_string,
  stringsAsFactors  = FALSE
)

dbWriteTable(conn=con, name='EPA_trigger_pulls', value=trigger_log_df, append=TRUE, row.names=FALSE)

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "- Appended historical pull metadata to trigger_pulls table\n"
)

New_pull_id<-dbGetQuery(con, "SELECT TOP 1 pull_id FROM EPA_trigger_pulls ORDER BY pull_id DESC")

New_pull_id_integer<-as.numeric(New_pull_id$pull_id[1])

update_clean_with_pull_id<-EPA_historical_readings%>%
  mutate(pull_id=New_pull_id_integer)%>%
  distinct(site_name, datetime_observed, parameter, .keep_all=TRUE) #Removes any rows in the historical data with duplicate site_name/datetime_observed/parameter combinations

#6. Append historical data to air_history table


if(nrow(update_clean_with_pull_id) > 0){
  dbWriteTable(
    conn      = con,
    name      = "EPA_air_history",
    value     = update_clean_with_pull_id,
    append    = TRUE,         
    row.names = FALSE
  )
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- Successfully appended",
    nrow(update_clean_with_pull_id),
    "new historical observations\n"
  )
  
} else {
  
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "- No new observations to append.\n"
  )
  
} 

#7. Updating sensor metadata with new last_seen timestamps based on the updated air_history table

EPA_sensor_information<-dbGetQuery(con, "SELECT * FROM EPA_sensor_information")

EPA_sensor_information<-EPA_sensor_information%>%
arrange(site_name) 

EPA_air_history<-dbGetQuery(con, "SELECT site_name, datetime_observed, parameter FROM EPA_air_history")

current_last_seen_times<-EPA_air_history%>%
  group_by(site_name)%>%
  summarize(most_recent_timestamp=max(datetime_observed))%>%
  arrange(site_name) #The site_names column is arranged in alphabetical order in both the sensor_information table and the current_last_seen_times table to ensure that indexing matches for metadata updates

unique(current_last_seen_times$site_name)
EPA_sensor_information$site_name

for (i in EPA_sensor_information$site_name) {
  query1 <- dbGetQuery(con, "SELECT last_seen FROM EPA_sensor_information WHERE site_name = ?", params = list(i)) #extracts the currently documented last_seen timestamp for each sensor and assigns it as db_last_seen
  db_last_seen <- if(nrow(query1) > 0) query1$last_seen else NA
  
  api_row <- current_last_seen_times %>% 
    filter(site_name == i)%>%
    select(most_recent_timestamp)
  
  if (nrow(api_row) > 0) {
    api_last_seen <- api_row$most_recent_timestamp} #assigns the current last_seen times for each sensor based on the updated air_history table
  
  if (is.na(db_last_seen) || db_last_seen < api_last_seen) {
    
    position=which(EPA_sensor_information$site_name == i) 
    j=current_last_seen_times$site_name[position]
    
    query2 <- "UPDATE EPA_sensor_information SET last_seen = ? WHERE site_name = ?"
    
    dbExecute(con, query2, params = list(as.character(api_last_seen), j))
    
  }
}

#8. Disconnect cleanly

dbDisconnect(con)
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Script finished executing and disconnected cleanly.\n")

