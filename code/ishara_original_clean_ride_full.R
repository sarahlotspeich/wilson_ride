ride_full <- read_excel("Ride Requests_2026-06-09.xlsx")

ride_full <- ride_full |>
  select(
    Request_Creation_Time    = `Request Creation Time`,
    Request_Status           = `Request Status`, #should be Completed
    Rider_ID                 = `Rider ID`,
    Booking_Method           = `Booking Method`, #any booking method is accepted
    Origin_Address           = `Origin Address`,
    Origin_Lat               = `Origin Lat`,
    Origin_Lng               = `Origin Lng`,
    Destination_Address      = `Destination Address`,
    Destination_Lat          = `Destination Lat`,
    Destination_Lng          = `Destination Lng`,
    
    Ride_Distance                 = `Ride Distance`,
    Pickup_Walking_Distance_ft    = `Pickup Walk Distance`,
    Dropoff_Walking_Distance_ft   = `Dropoff Walk Distance`,
    
    Ride_Duration            = `Ride Duration`, #amount of time the ride lasted
    Waiting_time_to_pickup_minutes = `On Demand Pickup ETA Minutes`, #Number of minutes from ride request to the estimated time of arrival (ETA)
    Deviation_minutes        = `On Demand Pickup ETA Deviation Minutes` #captures pickup delay, +ve a delay, -ve early
  )

ride_full <- ride_full |>
  mutate(across(c(Ride_Distance,
                  Pickup_Walking_Distance_ft,
                  Dropoff_Walking_Distance_ft,
                  Ride_Duration,
                  Waiting_time_to_pickup_minutes,
                  Deviation_minutes),
                as.numeric))


ride_full <- as.data.frame(ride_full)

ride_full <- ride_full |>
  mutate(Rider_ID = str_extract(as.character(Rider_ID), "\\d{7}"))

ride_full <- ride_full |>
  mutate(
    Request_Creation_Time = force_tz(as_datetime(Request_Creation_Time), "America/New_York"),
    creation_date         = as_date(Request_Creation_Time),
    creation_time         = hms::as_hms(Request_Creation_Time),
    study_week            = case_when(
      creation_date >= ymd("2026-03-30") & creation_date <= ymd("2026-04-04") ~ "Mar30-Apr4",
      creation_date >= ymd("2026-04-06") & creation_date <= ymd("2026-04-11") ~ "Apr 6-11",
      creation_date >= ymd("2026-04-13") & creation_date <= ymd("2026-04-18") ~ "Apr 13-18",
      creation_date >= ymd("2026-04-20") & creation_date <= ymd("2026-04-25") ~ "Apr 20-25",
      creation_date >= ymd("2026-04-27") & creation_date <= ymd("2026-05-02") ~ "Apr27-May2",
      creation_date >= ymd("2026-05-04") & creation_date <= ymd("2026-05-09") ~ "May 4-9",
      creation_date >= ymd("2026-05-11") & creation_date <= ymd("2026-05-16") ~ "May 11-16",
      creation_date >= ymd("2026-05-18") & creation_date <= ymd("2026-05-23") ~ "May 18-23",
      creation_date >= ymd("2026-05-25") & creation_date <= ymd("2026-05-30") ~ "May 25-30",
      TRUE ~ NA_character_
    )
  ) |>
  arrange(Rider_ID, creation_date, creation_time) |>  # order to the second
  group_by(Rider_ID, creation_date) |>
  mutate(first_ride_of_day = row_number() == 1L) |>
  ungroup() |>
  select(-Request_Creation_Time)   

ride_full <- ride_full |>
  relocate(creation_date, creation_time, Ride_Duration, Rider_ID)

glimpse(ride_full)