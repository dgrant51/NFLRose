library(xgboost)
library(dplyr)
library(scales)
library(lubridate)

xgb_model <- xgb.load("xgb_model.rds")
nflhistats24 <- readRDS("D:/R code/NFL Prediction/nflhistats24.rds")
nfl_df = nflhistats24

x_pred <- nfl_df %>% select(-Outcome)

predict_with_user_input <- function(xgb_model) {
  
  # List of NFL team abbreviations in alphabetical order
  teams <- c('crd', 'atl', 'rav', 'buf', 'car', 'chi', 'cin', 'cle', 'dal', 'den',
             'det', 'gnb', 'htx', 'clt', 'jax', 'kan', 'sdg', 'ram', 'rai', 'mia',
             'min', 'nwe', 'nor', 'nyg', 'nyj', 'phi', 'pit', 'sea', 'sfo', 'tam',
             'oti', 'was')
  
  # Function to get team input from user and convert to a numerical value
  get_team_number <- function() {
    # Ask the user to input the team abbreviation
    cat("Available teams:\n")
    cat(teams, sep = ", ")
    cat("\n\n")
    
    # Take user input for the team abbreviation
    team_input <- readline(prompt = "Enter the team abbreviation (e.g., 'atl', 'buf'): ")
    
    # Validate the user input
    # Define the list of teams with corresponding numerical values
    teamsNO <- c(
      'crd' = 1, 'atl' = 2, 'rav' = 3, 'buf' = 4, 'car' = 5, 
      'chi' = 6, 'cin' = 7, 'cle' = 8, 'dal' = 9, 'den' = 10,
      'det' = 11, 'gnb' = 12, 'htx' = 13, 'clt' = 14, 'jax' = 15, 
      'kan' = 16, 'sdg' = 17, 'ram' = 18, 'rai' = 19, 'mia' = 20, 
      'min' = 21, 'nwe' = 22, 'nor' = 23, 'nyg' = 24, 'nyj' = 25, 
      'phi' = 26, 'pit' = 27, 'sea' = 28, 'sfo' = 29, 'tam' = 30, 
      'oti' = 31, 'was' = 32
    )
    
    if (team_input %in% names(teamsNO)) {
      # Find the index (numerical value) of the team
      team_number <<- teamsNO[team_input]
      print(paste("The numerical value for team", team_input, "is", team_number))
      print("Thank you")
    return(team_number)
    } else {
      cat("Invalid team abbreviation entered. Please try again.\n")
    return(NULL)      }
    }
  
  # Call the function to get user input and display the corresponding team number
  get_team_number()
  
  df_filtered <- x_pred %>% filter(Team == team_number)
  
  df_filtered <- na.omit(df_filtered)
  
  # Avoid zeros or negative values by replacing with a small value
  df_filtered <<- df_filtered %>% mutate(across(everything(), ~ ifelse(. <= 0, 1E-13, .)))
  
  # Apply the logarithmic transformation to each column in x
  x_log <- log(df_filtered)
  
  # Quantile transformation using `scales` or custom method (exclude Day column)
  df_quantile <- x_log %>%
    mutate(across(-c(Day), ~ rescale(.)))
  
  season_week <- readline(prompt = "What week of the season is it?: ")
  
  season_week <- as.numeric(season_week)
  
  season_week <- log(season_week)
  
  # 1. Take user input for day of the week
  day_of_week <- readline(prompt = "Enter the day of the week for gameday (Mon, Tues, Wed, Thur, Fri, Sat, Sun): ")
  
  # Map the day of the week to a numeric value
  day_of_week_numeric <- switch(day_of_week,
                                "Mon" = 1,
                                "Tues" = 2,
                                "Wed" = 3,
                                "Thur" = 4,
                                "Fri" = 5,
                                "Sat" = 6,
                                "Sun" = 7,
                                NA)  # NA if invalid input
  
  if (is.na(day_of_week_numeric)) {
    cat("Invalid day of the week entered. Exiting...\n")
    return(NULL)
  day_of_week_numeric = as.numeric(day_of_week_numeric)
  }
  
  # 2. Take user input for the date (month-day format, e.g., "Jan 1", "Dec 25")
  date_input <- readline(prompt = "Enter the date of gameday (e.g., Jan 1, Dec 25): ")
  
  # Add the current year to the input
  current_year <- format(Sys.Date(), "%Y")
  date_str <- paste(current_year, date_input)
  
  # Convert the date to day of the year
  date_parsed <- ymd(date_str)
  day_of_year <- yday(date_parsed) + 1  # Add 1 to adjust for 0-based index
  
  day_of_year <- log(day_of_year)
  
  # Calculate the average of each column (excluding non-predictive columns)
  x_ave <- colMeans(df_filtered, na.rm = TRUE)
  
  # Create a data frame for prediction input
  x_ave <- as.data.frame(t(x_ave))
  
  x_ave <- x_ave %>% select(-Team)
  
  # Convert all columns to numeric
  x_ave <- x_ave %>% mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  x_ave$Day <- day_of_week_numeric
  x_ave$Date <- day_of_year
  x_ave$Week <- season_week
  
  pred_mat <- as.matrix(x_ave)
  
  print(x_ave)
  print(pred_mat)
  print(team_number)
  
  # Make prediction using the trained xgboost model
  y_pred <- predict(xgb_model, newdata = pred_mat)
  
  # 5. Return or print the predicted value
  cat("The predicted value is:", y_pred, "\n")
}

# Call the function
predict_with_user_input(xgb_model)



