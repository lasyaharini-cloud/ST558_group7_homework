
library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Create the user interface
ui <- fluidPage(
  useShinyalert(),
  titlePanel("Correlation Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      
      # Choose two numeric variables
      selectizeInput("corr_x", "x Variable",
                     choices = numeric_vars, selected = "PINCP"),
      
      selectizeInput("corr_y", "y Variable",
                     choices = numeric_vars, selected = "JWMNP"),
      
      br(),
      h2("Choose a subset of the data:"),
      
      # Household language
      radioButtons("hhl_corr", "Household Language",
                   choices = c("All" = "all",
                               "English only" = "english",
                               "Spanish" = "spanish",
                               "Other" = "other")),
      
      # SNAP recipient
      radioButtons("fs_corr", "SNAP Recipient",
                   choices = c("All" = "all",
                               "Yes" = "yes",
                               "No" = "no")),
      
      # Education level
      radioButtons("schl_corr", "Educational attainment",
                   choices = c("All" = "all",
                               "High School not Completed" = "no_hs",
                               "High School or GED" = "hs",
                               "College Degree" = "college")),
      
      h2("Select a Sample Size"),
      
      # Select the number of observations
      sliderInput("corr_n", "", min = 20, max = 500, value = 20),
      actionButton("corr_sample", "Get a Sample!"),
      width = 4
    ),
    
    mainPanel(
      plotOutput("corr_plot"),
      
      # Show guessing section after taking a sample
      conditionalPanel(
        "input.corr_sample > 0",
        h2("Guess the correlation!"),
        column(6, numericInput("corr_guess", "",
                               value = 0, min = -1, max = 1)),
        column(6, actionButton("corr_submit", "Check Your Guess!"))
      ),
      width = 8
    )
  )
)

# Read the sample data
my_sample <- readRDS("my_sample_temp.rds")

# Server function
server <- function(input, output, session) {
  
  # Update y variable choices when x changes
  observeEvent(input$corr_x, {
    x <- input$corr_x
    y <- input$corr_y
    
    if (x != y) {
      choices <- numeric_vars[numeric_vars != x]
      updateSelectizeInput(session, "corr_y",
                           choices = choices, selected = y)
    }
  })
  
  # Update x variable choices when y changes
  observeEvent(input$corr_y, {
    x <- input$corr_x
    y <- input$corr_y
    
    if (x != y) {
      choices <- numeric_vars[numeric_vars != y]
      updateSelectizeInput(session, "corr_x",
                           choices = choices, selected = x)
    }
  })
  
  # Store the sample and the true correlation
  sample_corr <- reactiveValues(
    corr_data = NULL,
    corr_truth = NULL
  )
  
  # Take a new sample when the button is clicked
  observeEvent(input$corr_sample, {
    
    # Check that two different variables are selected
    if (input$corr_x == input$corr_y) {
      showNotification("Please choose different variables.",
                       type = "warning")
      return()
    }
    
    # Household language filter
    if (input$hhl_corr == "all") {
      hhl_sub <- HHLvals
    } else if (input$hhl_corr == "english") {
      hhl_sub <- HHLvals["1"]
    } else if (input$hhl_corr == "spanish") {
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    
    # SNAP filter
    if (input$fs_corr == "all") {
      fs_sub <- FSvals
    } else if (input$fs_corr == "yes") {
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }
    
    # Education filter
    if (input$schl_corr == "all") {
      schl_sub <- SCHLvals
    } else if (input$schl_corr == "no_hs") {
      schl_sub <- SCHLvals[c("0", "01", "02", "03", "04",
                             "05", "06", "07", "08", "09",
                             "10", "11", "12", "13", "14", "15")]
    } else if (input$schl_corr == "hs") {
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    
    # Get the selected numeric variables
    corr_vars <- c(input$corr_x, input$corr_y)
    
    # Filter data using selected categories
    subsetted_data <- my_sample |>
      filter(HHLfac %in% hhl_sub,
             FSfac %in% fs_sub,
             SCHLfac %in% schl_sub)
    
    # Filter invalid numeric values
    if ("WKHP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, WKHP > 0)
    }
    if ("VALP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, !is.na(VALP))
    }
    if ("TAXAMT" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, !is.na(TAXAMT))
    }
    if ("GRPIP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, GRPIP > 0)
    }
    if ("GASP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, GASP > 0)
    }
    if ("ELEP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, ELEP > 0)
    }
    if ("WATP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, WATP > 0)
    }
    if ("PINCP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, AGEP > 18)
    }
    if ("JWMNP" %in% corr_vars) {
      subsetted_data <- filter(subsetted_data, !is.na(JWMNP))
    }
    
    # Remove missing values for the chosen variables
    subsetted_data <- subsetted_data |>
      filter(if_all(all_of(corr_vars), ~ !is.na(.x)))
    
    # Check that data is available
    if (nrow(subsetted_data) == 0) {
      showNotification("No data available for this selection.",
                       type = "warning")
      return()
    }
    
    # Take a weighted random sample
    index <- sample(
      1:nrow(subsetted_data),
      size = input$corr_n,
      replace = TRUE,
      prob = subsetted_data$PWGTP / sum(subsetted_data$PWGTP)
    )
    
    # Save the sample and calculate correlation
    sample_corr$corr_data <- subsetted_data[index, ]
    sample_corr$corr_truth <- cor(
      sample_corr$corr_data |> select(all_of(corr_vars)),
      use = "complete.obs"
    )[1, 2]
  })
  
  # Create the scatter plot
  output$corr_plot <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data),
           "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    
    ggplot(sample_corr$corr_data,
           aes(x = .data[[isolate(input$corr_x)]],
               y = .data[[isolate(input$corr_y)]])) +
      geom_point() +
      labs(x = isolate(input$corr_x),
           y = isolate(input$corr_y))
  })
  
  # Check the correlation guess
  observeEvent(input$corr_submit, {
    req(!is.null(sample_corr$corr_truth))
    
    true_corr <- sample_corr$corr_truth
    
    if (is.na(true_corr)) {
      shinyalert("Correlation unavailable",
                 "Please try another sample.", type = "warning")
      return()
    }
    
    if (abs(input$corr_guess - true_corr) <= 0.05) {
      shinyalert("Nicely done!",
                 paste0("The sample correlation is ",
                        round(true_corr, 4), "."),
                 type = "success")
    } else if (input$corr_guess > true_corr) {
      shinyalert("Try again!", "Try guessing a lower value.")
    } else {
      shinyalert("Try again!", "Try guessing a higher value.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
