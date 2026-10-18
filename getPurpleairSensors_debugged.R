
#This is the function for downloading metadata for Purpleair sensors that are not already in the database. It is a modification of the function getPurpleairSensors, which is native to the PurpleAirAPI package. This function tailors the previous function to acommodate the current database schema.

function (apiReadKey = NULL, fields = c("date_created", "last_seen", "name", "location_type", "model", "hardware", "latitude", "longitude")) 
{
  if (is.null(apiReadKey) || length(apiReadKey) != 1 ||
      !nzchar(apiReadKey)) {
    stop("apiReadKey not defined!")
  }
  
    api_endpoint <- paste0("https://api.purpleair.com/v1/sensors?fields=", 
        paste(fields, collapse = "%2C"))
    result <- httr::GET(api_endpoint, config = httr::add_headers(`X-API-Key` = apiReadKey))
    if (httr::http_error(result)) {
        error_content <- httr::content(result, as = "text", encoding = "UTF-8")
        error_details <- jsonlite::fromJSON(error_content)
        error_message <- paste(httr::status_code(result), error_details$error, 
            error_details$description)
        stop(error_message)
    }
    raw <- httr::content(result, as = "text", encoding = "UTF-8")
    response_list <- jsonlite::fromJSON(raw)
    purpleair <- as.data.frame(response_list$data)
    names(purpleair) <- response_list$fields
    date_cols <- c("date_created", "last_seen")
    for (col in date_cols) {
        if (col %in% names(purpleair)) {
            numeric_time<-as.numeric(purpleair[[col]])
            purpleair[[col]] <- as.POSIXct(numeric_time, 
                                           origin = "1970-01-01", 
                                           tz = "UTC")
        }
    }
    return(purpleair)
}
