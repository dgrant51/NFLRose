library(lubridate)
library(dplyr)

nfl2024 <- readRDS("D:/R code/NFL Prediction/nfl2024.rds")
nfl_df = nfl2024
# Assuming nfl_df is your data frame

nfl_df <- nfl_df %>% select(-X8, -X9, -X10, -Season, -X36)

# Process X3 (date) if it's not numeric
if (!is.numeric(nfl_df$X3)){
  # Add the current year to the month-day values
  current_year <- format(Sys.Date(), "%Y")  # Get the current year (e.g., "2024")
  
  # Combine the current year with the month-day string
  date_str <- paste(current_year, nfl_df$X3)
  
  # Convert to Date format using ymd()
  date1 <- ymd(date_str)
  
  # Convert to day of the year (Julian day within the year)
  day_of_year <- yday(date1) + 1  # Adding 1 because 'yday' gives a 0-based index
  nfl_df$X3 <- day_of_year
}

# Process X2 (weekday) if it's not numeric
if (!is.numeric(nfl_df$X2)){
  nfl_df <- nfl_df %>%
    mutate(X2 = case_when(
      X2 == "Mon"  ~ 1,
      X2 == "Tues" ~ 2,
      X2 == "Wed"  ~ 3,
      X2 == "Thur" ~ 4,
      X2 == "Fri"  ~ 5,
      X2 == "Sat"  ~ 6,
      X2 == "Sun"  ~ 7,
      TRUE ~ NA_real_  # Handle any values that don't match
    ))
}

# Process X5 (Win/Loss) if it's not numeric
if (!is.numeric(nfl_df$X5)){
  nfl_df <- nfl_df %>%
    mutate(X5 = case_when(
      X5 == "W" ~ 1,
      X5 == "L" ~ 0,
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

nfl_df <- nfl_df %>% select(-Team)

saveRDS(nfl_df, "nflhistats.rds")



