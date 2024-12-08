# Load necessary libraries
library(rvest)
library(dplyr)
library(readr)

# Define the seasons and team abbreviations
seasons <- as.character(2024)  # Modify this range for your required seasons
team_abbrs <- c('crd', 'atl', 'rav', 'buf', 'car', 'chi', 'cin', 'cle', 'dal', 'den',
                'det', 'gnb', 'htx', 'clt', 'jax', 'kan', 'sdg', 'ram', 'rai', 'mia',
                'min', 'nwe', 'nor', 'nyg', 'nyj', 'phi', 'pit', 'sea', 'sfo', 'tam',
                'oti', 'was')

# List to hold the data frames for each team/season
nfl_df_list24 <- list()

# Loop through each season and team to scrape data
for (season in seasons) {
  for (team in team_abbrs) {
    # Construct the URL for the current team and season
    url <- paste0('https://www.pro-football-reference.com/teams/', team, '/', season, '/gamelog/')
    
    cat("Fetching data from:", url, "\n")
    
    tryCatch({
      # Read the page from the URL
      page <- read_html(url)
      
      # Construct the dynamic table ID
      table_id <- paste0("table#gamelog", season)  # The ID of the table doesn't change with season in this case.
      
      # Try to extract the table using the dynamically generated table ID
      table <- page %>% html_node(table_id) %>% html_table(header = FALSE)  # Don't assume any header initially
      
      # Check if the table exists and has rows
      if (length(table) > 0 && nrow(table) > 0) {
        
        # Print the first few rows to check the structure (for debugging)
        print(head(table))  # Check the first few rows to inspect
        
        # Set the 3rd row as column names (the 3rd row in the table after header rows)
        new_colnames <- c("Week", "Day", "Date", "Boxscore", "Outcome", "OT", "@", "Opp", "Tm", 
                          "Opp1", "PCmp", "PAtt", "PYds1", "PTD", "PInt", "PSk", "PYds", "PY/A", 
                          "PNY/A", "PCmp%", "PRate", "RAtt", "RYds", "RY/A", "RTD", "FGM", "FGA", 
                          "XPM", "XPA", "#Pnt", "PntYds", "3DConv", "3DAtt", "4DConv", "4DAtt", "ToP")
        
        colnames(table) <- new_colnames  # Apply new column names
        
        # Remove the first two rows (since they are not part of the data)
        table <- table[-c(1, 2), ]  # Use indexing to remove rows, avoids 'slice()' issue
        
        # Remove any unnecessary columns (columns 4, 6, and 7 in this case)
        table <- table %>%
          select(-c(4, 6, 7))
        
        # Remove any empty columns (those named "")
        table <- table %>%
          select(-which(names(table) == ""))
        
        # Add Team and Season columns
        table <- table %>%
          mutate(Team = team, Season = season)
        
        # Append the data frame to the list
        nfl_df_list24[[length(nfl_df_list24) + 1]] <- table
        
      } else {
        cat("No data found for", team, "in season", season, "\n")
      }
      
    }, error = function(e) {
      cat("Failed to load data for", team, "in", season, ":", conditionMessage(e), "\n")
    })
    
    # Dynamic sleep to avoid being blocked
    Sys.sleep(runif(1, 7, 8))
  }
}

# Combine all data frames into one
if (length(nfl_df_list24) > 0) {
  nfl_df24 <- bind_rows(nfl_df_list24)  # Bind rows efficiently
  print(head(nfl_df24))  # Display the first few rows
  cat("Data fetching complete.\n")
  
  # Save the combined data to a file
  saveRDS(nfl_df24, "nflUpdate.rds")
} else {
  cat("No data fetched.\n")
}

