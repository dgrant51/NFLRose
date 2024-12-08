library(lubridate)
library(dplyr)

nflUpdate <- readRDS("D:/R code/NFL Prediction/nflUpdate.rds")
nfl_df = nflUpdate

nflUpdate <- readRDS("D:/R code/NFL Prediction/nflUpdate.rds")

# Assuming nfl_df is your data frame

nfl_df <- nfl_df %>% select(-Opp, -Tm, -Opp1, -Season, -ToP)

# Process Date (date) if it's not numeric
if (!is.numeric(nfl_df$Date)){
  # Add the current year to the month-day values
  current_year <- format(Sys.Date(), "%Y")  # Get the current year (e.g., "2024")
  
  # Combine the current year with the month-day string
  date_str <- paste(current_year, nfl_df$Date)
  
  # Convert to Date format using ymd()
  date1 <- ymd(date_str)
  
  # Convert to day of the year (Julian day within the year)
  day_of_year <- yday(date1) + 1  # Adding 1 because 'yday' gives a 0-based index
  nfl_df$Date <- day_of_year
}

# Process Day (weekday) if it's not numeric
if (!is.numeric(nfl_df$Day)){
  nfl_df <- nfl_df %>%
    mutate(Day = case_when(
      Day == "Mon"  ~ 1,
      Day == "Tues" ~ 2,
      Day == "Wed"  ~ 3,
      Day == "Thur" ~ 4,
      Day == "Fri"  ~ 5,
      Day == "Sat"  ~ 6,
      Day == "Sun"  ~ 7,
      TRUE ~ NA_real_  # Handle any values that don't match
    ))
}

# Process Outcome (Win/Loss) if it's not numeric
if (!is.numeric(nfl_df$Outcome)){
  nfl_df <- nfl_df %>%
    mutate(Outcome = case_when(
      Outcome == "W" ~ 1,
      Outcome == "L" ~ 0,
      TRUE ~ NA_real_  # Handle any values that don't match
    ))
}

# Process Team (abbreviations) if it's not numeric
if (!is.numeric(nfl_df$Team)){
  nfl_df <- nfl_df %>%
    mutate(Team = case_when(
      Team == 'crd' ~ 1,
      Team == 'atl' ~ 2,
      Team == 'rav' ~ 3,
      Team == 'buf' ~ 4,
      Team == 'car' ~ 5,
      Team == 'chi' ~ 6,
      Team == 'cin' ~ 7,
      Team == 'cle' ~ 8,
      Team == 'dal' ~ 9,
      Team == 'den' ~ 10,
      Team == 'det' ~ 11,
      Team == 'gnb' ~ 12,
      Team == 'htx' ~ 13,
      Team == 'clt' ~ 14,
      Team == 'jax' ~ 15,
      Team == 'kan' ~ 16,
      Team == 'sdg' ~ 17,
      Team == 'ram' ~ 18,
      Team == 'rai' ~ 19,
      Team == 'mia' ~ 20,
      Team == 'min' ~ 21,
      Team == 'nwe' ~ 22,
      Team == 'nor' ~ 23,
      Team == 'nyg' ~ 24,
      Team == 'nyj' ~ 25,
      Team == 'phi' ~ 26,
      Team == 'pit' ~ 27,
      Team == 'sea' ~ 28,
      Team == 'sfo' ~ 29,
      Team == 'tam' ~ 30,
      Team == 'oti' ~ 31,
      Team == 'was' ~ 32,
      Team == 'Bye Week' ~ 0,  # Treat "Bye Week" as 0
      TRUE ~ NA_real_  # Handle any values that don't match
    ))
}

nfl_df[] <- lapply(nfl_df, function(x) as.numeric(as.character(x)))

saveRDS(nfl_df, "nflhistats24.rds")