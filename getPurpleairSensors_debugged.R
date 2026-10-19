
#This is the function for downloading metadata for Purpleair sensors that are not already in the database. It is a modification of the function getPurpleairSensors, which is native to the PurpleAirAPI package. This function tailors the previous function to acommodate the current database schema.

getPurpleairSensors_debugged <- function(
    apiReadKey = NULL,
    fields = c(
      "date_created", "last_seen", "name", "location_type",
      "model", "hardware", "latitude", "longitude"
    )
) {
  if (is.null(apiReadKey) || length(apiReadKey) != 1 ||
      is.na(apiReadKey) || !nzchar(apiReadKey)) {
    stop("apiReadKey not defined!")
  }
  
  api_endpoint <- paste0(
    "https://api.purpleair.com/v1/sensors?fields=",
    paste(fields, collapse = "%2C")
  )
  
  result <- httr::GET(
    api_endpoint,
    httr::add_headers(`X-API-Key` = apiReadKey)
  )
  
  if (httr::http_error(result)) {
    stop(httr::content(result, as = "text", encoding = "UTF-8"))
  }
  
  raw <- httr::content(result, as = "text", encoding = "UTF-8")
  response_list <- jsonlite::fromJSON(raw)
  
  purpleair <- as.data.frame(response_list$data)
  names(purpleair) <- response_list$fields
  
  date_cols <- intersect(
    c("date_created", "last_seen"),
    names(purpleair)
  )
  
  for (col in date_cols) {
    x <- purpleair[[col]]
    
    # Convert Unix timestamps only if values are numeric
    if (is.numeric(x) || is.integer(x)) {
      purpleair[[col]] <- as.POSIXct(
        x, origin = "1970-01-01", tz = "UTC"
      )
    } else {
      # Diagnose unexpected character values
      message("Column: ", col)
      print(utils::head(x))
      
      numeric_time <- suppressWarnings(as.numeric(as.character(x)))
      
      if (all(is.na(x) | !is.na(numeric_time))) {
        purpleair[[col]] <- as.POSIXct(
          numeric_time, origin = "1970-01-01", tz = "UTC"
        )
      } else {
        warning(
          "Column ", col,
          " contains nonnumeric timestamps; leaving it unchanged."
        )
      }
    }
  }
  
  purpleair
}