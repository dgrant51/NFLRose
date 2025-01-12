library(shiny)

# Define the UI
ui <- fluidPage(
  # Apply custom CSS to center the title panel
  tags$style(HTML("
    .container-fluid {
      text-align: center;
    }
  ")),

  # Title Panel (Centered)
  titlePanel("NFL Team Prediction"),

  # Create the first row for the content (main panel)
  fluidRow(
    column(12,
           style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",  # CSS to center the content in the column

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
           actionButton("r2", "Predict", class = "btn-primary"),

           # Progress bar and result output
           verbatimTextOutput("result")
    )
  ),

  # Create the second row for the sidebar (bottom panel) with 3 columns
  fluidRow(
    column(4,  # First column
           style = "display: flex; align-items: center; justify-content: center;",  # Center the content in the column

           # Action button to trigger Update
           actionButton("r", "Update", class = "btn-primary"),

           textOutput("team_label")
    ),
    column(4,  # Second column (for result)
           style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",  # Center the content in the column

           verbatimTextOutput("result")
    ),
    column(4,  # Third column (empty)
           style = "display: flex; align-items: center; justify-content: center;"  # Empty column
    )
  )
)




# Define the server logic
server <- function(input, output) {

  observeEvent(input$r, {
    tryCatch({
      # Define URLs for each file
      url_scrape <- "https://raw.githubusercontent.com/dgrant51/NFLRose/refs/heads/RforRose/ProjectRose/2024Scrape.R"
      url_clean <- "https://raw.githubusercontent.com/dgrant51/NFLRose/refs/heads/RforRose/ProjectRose/2024clean.R"
      url_log <- "https://raw.githubusercontent.com/dgrant51/NFLRose/refs/heads/RforRose/ProjectRose/2024log.R"
      url_update <- "https://raw.githubusercontent.com/dgrant51/NFLRose/refs/heads/RforRose/ProjectRose/2024update.R"

      # Create temporary files to download the R scripts
      Scrape <- tempfile("Scrape", fileext = ".R")
      Clean <- tempfile("Clean", fileext = ".R")
      Log <- tempfile("Log", fileext = ".R")
      Update <- tempfile("Update", fileext = ".R")

      # Download the R scripts to the temporary files
      download.file(url_scrape, Scrape)
      download.file(url_clean, Clean)
      download.file(url_log, Log)
      download.file(url_update, Update)

      # Source the downloaded R scripts
      source(Scrape)
      source(Clean)
      source(Log)
      source(Update)

      # Show a success notification
      showNotification("Updated!", type = "message", duration = 5)

    }, error = function(e) {
      # Handle any errors that occur during download or sourcing
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred: ", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })



  # Define an event that listens for the second button press (Run Prediction)
  observeEvent(input$r2, {
    withProgress(message = 'Running Prediction...', value = 0, {
      tryCatch({
        # Capture the selected team value (numeric value)
        team_number <- input$team  # Numeric value of the selected team

        # Print team_number for debugging
        print(paste("Selected team number:", team_number))

        # Map team number to team name (team_name)
        team_names <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN',
                        'DET', 'GNB', 'HOU', 'IND', 'JAX', 'KAN', 'LAC', 'LAR', 'LVR', 'MIA',
                        'MIN', 'NWE', 'NOR', 'NYG', 'NYJ', 'PHI', 'PIT', 'SEA', 'SFO', 'TAM',
                        'TEN', 'WAS')

        # Get the team name based on the numeric value (team_number)
        team_name <- team_names[team_number]

        # Save team number and name to the global environment
        assign("team_number", team_number, envir = .GlobalEnv)  # Save to global environment
        assign("team_name", team_name, envir = .GlobalEnv)  # Save team name to global environment

        # Download and source the prediction script from GitHub
        download.file("https://raw.githubusercontent.com/dgrant51/NFLRose/refs/heads/RforRose/R/Prediction.R", destfile = "Prediction.R")
        incProgress(30/100)

        # Now Prediction.R can access 'team_number' and 'team_name' from the global environment
        source("Prediction.R")

        incProgress(60/100)

        # After prediction script execution, show success message
        output$result <- renderText({
          paste("Prediction for team", team_name, "executed successfully!")
          paste("Chance of a win:", score)
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

