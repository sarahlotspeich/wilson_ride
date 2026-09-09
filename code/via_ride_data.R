# Read in raw ride request data (from Via)
ride_full <- readxl::read_excel("~/Documents/wilson_ride/data/Ride Requests_2026-06-09.xlsx")

# Replace spaces in column names with underscores 
ride_full <- ride_full |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

# Rename two columns to be shorter/more interpretable
ride_full <- ride_full |> 
  dplyr::rename(
    Pickup_Walking_Distance_ft = Pickup_Walk_Distance,
    Dropoff_Walking_Distance_ft = Dropoff_Walk_Distance, 
    Waiting_time_to_pickup_minutes = On_Demand_Pickup_ETA_Minutes, 
    Deviation_minutes = On_Demand_Pickup_ETA_Deviation_Minutes
  )

# Focus on columns needed for our analysis
ride_full <- ride_full |>
  dplyr::select(
    Request_Creation_Time,
    Request_Status, 
    Rider_ID,
    Booking_Method, 
    Origin_Address,
    Origin_Lat,
    Origin_Lng,
    Destination_Address,
    Destination_Lat,
    Destination_Lng,
    Ride_Distance,
    Pickup_Walking_Distance_ft,
    Dropoff_Walking_Distance_ft,
    Ride_Duration,
    Waiting_time_to_pickup_minutes, 
    Deviation_minutes
  )

# Convert numeric columns to numeric (some read in as strings)
ride_full <- ride_full |>
  dplyr::mutate(
    dplyr::across(
      c(Ride_Distance,
        Pickup_Walking_Distance_ft,
        Dropoff_Walking_Distance_ft,
        Ride_Duration,
        Waiting_time_to_pickup_minutes,
        Deviation_minutes),
      as.numeric))

# Clean up date/time variables 
ride_full <- ride_full |>
  dplyr::mutate(
    Request_Creation_Time = lubridate::force_tz(
      lubridate::as_datetime(Request_Creation_Time),
      "America/New_York"),
    creation_date = lubridate::date(Request_Creation_Time),
    creation_time = hms::as_hms(Request_Creation_Time),
    study_week = paste("Study Week", floor(as.numeric(creation_date - lubridate::ymd("2026-03-30")) / 7) + 1), 
    study_week = factor(x = study_week, levels = paste("Study Week", 1:8))
  ) 

# Arrange by rider ID and date/time and create indicator for first ride of the day 
ride_full <- ride_full |> 
  dplyr::arrange(Rider_ID, creation_date, creation_time) |> ## arrange by rider, day, time
  dplyr::group_by(Rider_ID, creation_date) |> ## group per rider, per day
  dplyr::mutate(first_ride_of_day = dplyr::row_number() == 1) |> ## create indicator of first ride per rider, per day
  dplyr::ungroup() ## remove grouping

# Reorder columns to move creation date/time and ride duration/ID to the beginning
ride_full <- ride_full |>
  dplyr::select(creation_date, creation_time, Ride_Duration, Rider_ID, dplyr::everything())

# Save cleaned ride_full data 
ride_full |> 
  write.csv("~/Documents/wilson_ride/data/via_ride_data.csv", row.names = FALSE)
