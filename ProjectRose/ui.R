server <- function(input, output) {

  # Define an event that listens for the second button press (Run Script 2)
  observeEvent(input$r2, {
    withProgress(message = 'Running Prediction...', value = 0, {
      tryCatch({
        # Capture the selected team value
        team_number <- input$team  # Numeric value of the selected team

        # Map the team number to the team name (mapping)
        team_names <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN',
                        'DET', 'GNB', 'HOU', 'IND', 'JAX', 'KAN', 'LAC', 'LAR', 'LVR', 'MIA',
                        'MIN', 'NWE', 'NOR', 'NYG', 'NYJ', 'PHI', 'PIT', 'SEA', 'SFO', 'TAM',
                        'TEN', 'WAS')

        # Ensure 'team_name' is a character string
        team_name <- as.character(team_names[team_number])  # Get the team name based on selected team number

        # Save the team name to the global environment for use in external scripts
        assign("team_name", team_name, envir = .GlobalEnv)

        # Download and source the prediction script from GitHub
        download.file("https://raw.githubusercontent.com/dgrant51/NFLRose/refs/heads/RforRose/R/Prediction.R", destfile = "Prediction.R")
        incProgress(30/100)

        source("Prediction.R")  # Now Prediction.R can access 'team_name' as a string

        incProgress(60/100)

        # After prediction script execution, show success message
        output$result <- renderText({
          paste("Prediction for team", team_name, "executed successfully!")
        })

      }, error = function(e) {
        # Handle error for Script 2
        output$result <- renderText({
          paste("Error in Script 2:", e$message)
        })
      })
    })
  })
}

