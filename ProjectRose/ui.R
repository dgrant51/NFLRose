library(shiny)

# Define the UI
ui <- fluidPage(

  # Title
  titlePanel("NFL Team Prediction"),

  # Layout for the UI elements
  sidebarLayout(
    sidebarPanel(
      # Dropdown for team selection
      selectInput("team",
                  "Choose a Team:",
                  choices = c('ARI' = 1, 'ATL' = 2, 'BAL' = 3, 'BUF' = 4, 'CAR' = 5, 'CHI' = 6,
                              'CIN' = 7, 'CLE' = 8, 'DAL' = 9, 'DEN' = 10, 'DET' = 11, 'GNB' = 12,
                              'HOU' = 13, 'IND' = 14, 'JAX' = 15, 'KAN' = 16, 'LAC' = 17,
                              'LAR' = 18, 'LVR' = 19, 'MIA' = 20, 'MIN' = 21, 'NWE' = 22,
                              'NOR' = 23, 'NYG' = 24, 'NYJ' = 25, 'PHI' = 26, 'PIT' = 27,
                              'SEA' = 28, 'SFO' = 29, 'TAM' = 30, 'TEN' = 31, 'WAS' = 32),
                  selected = 1),  # Default selected team is 'ARI'

      # Action button to trigger prediction
      actionButton("r2", "Predict", class = "btn-primary")
    ),

    mainPanel(
      # Progress bar and result output
      verbatimTextOutput("result")
    )
  )
)

# Define the server logic
server <- function(input, output) {

  # Define an event that listens for the second button press (Run Script 2)
  observeEvent(input$r2, {
    withProgress(message = 'Running Prediction...', value = 0, {
      tryCatch({
        # Capture the selected team value (numeric value)
        team_number <- input$team  # Numeric value of the selected team

        # Check if the team number is valid (between 1 and 32)
        if (team_number < 1 || team_number > 32) {
          stop("Invalid team number")
        }

        # Save the team number to the global environment for use in external scripts
        assign("team_number", team_number, envir = .GlobalEnv)  # Assign to global environment

        # Map the team number to the team name (mapping)
        team_names <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN',
                        'DET', 'GNB', 'HOU', 'IND', 'JAX', 'KAN', 'LAC', 'LAR', 'LVR', 'MIA',
                        'MIN', 'NWE', 'NOR', 'NYG', 'NYJ', 'PHI', 'PIT', 'SEA', 'SFO', 'TAM',
                        'TEN', 'WAS')
        team_name <- team_names[team_number]  # Get the team name based on selected team number

        # Save the team name to the global environment for use in external scripts
        assign("team_name", team_name, envir = .GlobalEnv)  # Assign team name to global env

        # Download and source the prediction script from GitHub
        download.file("https://raw.githubusercontent.com/dgrant51/NFLRose/refs/heads/RforRose/R/Prediction.R", destfile = "Prediction.R")
        incProgress(30/100)

        # Now Prediction.R can access 'team_number' and 'team_name' from the global environment
        source("Prediction.R")

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

# Run the Shiny app
shinyApp(ui = ui, server = server)
