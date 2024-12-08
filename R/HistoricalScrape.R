# Load necessary libraries
library(rvest)
library(dplyr)
library(readr)

# Define the seasons and team abbreviations
seasons <- as.character(2000:2023)  # Modify this range for your required seasons
team_abbrs <- c('crd', 'atl', 'rav', 'buf', 'car', 'chi', 'cin', 'cle', 'dal', 'den',
                'det', 'gnb', 'htx', 'clt', 'jax', 'kan', 'sdg', 'ram', 'rai', 'mia',
                'min', 'nwe', 'nor', 'nyg', 'nyj', 'phi', 'pit', 'sea', 'sfo', 'tam',
                'oti', 'was')

# List to hold the data frames for each team/season
nfl_df_list <- list()

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
      
      # Check if the table exists
      if (length(table) > 0) {
        
        # Print the first few rows to check the structure (for debugging)
        print(head(table))  # Check the first few rows to inspect
        
        # Set the 3rd row as column names (the 3rd row in the table after header rows)
        new_colnames <- ifelse(is.na(colnames(table)) | colnames(table) == "", 
                               c("Week", "Day", "Date", "Boxscore", "Outcome", "OT", "@", "Opp", "Tm", 
                                 "Opp", "PCmp", "PAtt", "PYds", "PTD", "PInt", "PSk", "PYds", "PY/A", 
                                 "PNY/A", "PCmp%", "PRate", "RAtt", "RYds", "RY/A", "RTD", "FGM", "FGA", 
                                 "XPM", "XPA", "#Pnt", "PntYds", "3DConv", "3DAtt", "4DConv", "4DAtt", "ToP"), 
                               colnames(table[2,]))  # Fill missing names with default names
        colnames(table) <- new_colnames  # Apply new column names
        
        # Remove the first three rows (since they are not part of the data)
        table <- table %>%
          slice(-c(1, 2))  # Removes rows 1, 2 (metadata, extra headers)
        
        # Remove columns 4, 6, and 7 (if needed)
        table <- table %>%
          select(-c(4, 6, 7))
        
        # Remove any empty columns (those named "")
        table <- table %>%
          select(-which(names(table) == ""))
        
        # Add Team and Season columns
        table <- table %>%
          mutate(Team = team, Season = season)
        
        # Append the data frame to the list
        nfl_df_list <- append(nfl_df_list, list(table))
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
if (length(nfl_df_list) > 0) {
  nfl_df <- bind_rows(nfl_df_list)
  print(head(nfl_df))  # Display the first few rows
  cat("Data fetching complete.\n")
  
  # Save the combined data to a file
  saveRDS(nfl_df, "nfl2024.rds")
} else {
  cat("No data fetched.\n")
}
