# Load shiny library
library(shiny)

# Define the UI
ui <- fluidPage(
  # Create multiple buttons in the UI
  actionButton("r1", "Update Model"),
  actionButton("r2", "Predict"),

  # Output to display results
  textOutput("result")
)

# Define the server function
server <- function(input, output) {

  # Define an event that listens for the first button press (Run Script 1)
  observeEvent(input$r1, {
    # Show the progress bar
    withProgress(message = 'Running Script 1...', value = 0, {
      tryCatch({
        # Download the scripts from GitHub (using raw GitHub URLs)
        download.file("https://raw.githubusercontent.com/dgrant51/NFLRose/3b81f9d1eb63de81385fb799a90e582c771a82ff/R/2024Scrape.R", destfile = "2024Scrape.R")
        incProgress(5/100)  # Increment the progress bar

        download.file("https://raw.githubusercontent.com/dgrant51/NFLRose/3b81f9d1eb63de81385fb799a90e582c771a82ff/R/2024update.R", destfile = "2024update.R")
        incProgress(10/100)  # Increment the progress bar

        download.file("https://raw.githubusercontent.com/dgrant51/NFLRose/3b81f9d1eb63de81385fb799a90e582c771a82ff/R/Log24.R", destfile = "2024log.R")
        incProgress(15/100)  # Increment the progress bar

        download.file("https://raw.githubusercontent.com/dgrant51/NFLRose/3b81f9d1eb63de81385fb799a90e582c771a82ff/R/StatsCleanup24.R", destfile = "2024clean.R")
        incProgress(20/100)  # Increment the progress bar

        # Source the downloaded scripts
        source("2024Scrape.R")
        incProgress(40/100)  # Increment the progress bar

        source("2024clean.R")
        incProgress(50/100)  # Increment the progress bar

        source("2024log.R")
        incProgress(70/100)  # Increment the progress bar

        source("2024update.R")
        incProgress(90/100)  # Increment the progress bar

        # Set result message
        output$result <- renderText({
          "Scripts executed successfully!"
        })

      }, error = function(e) {
        # Handle error and set result message
        output$result <- renderText({
          paste("Error in Script 1:", e$message)
        })
      })
    })
  })

  # Define an event that listens for the second button press (Run Script 2)
  observeEvent(input$r2, {
    tryCatch({
      # Run another script by sourcing it (this can be a different script)
      # Replace with the script you want to run
      source("path/to/your/script2.R")

      # Set result message
      output$result <- renderText({
        "Script 2 executed successfully!"
      })
    }, error = function(e) {
      # Handle error for Script 2
      output$result <- renderText({
        paste("Error in Script 2:", e$message)
      })
    })
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)



