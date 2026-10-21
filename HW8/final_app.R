library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel('Correlation Exploration'),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      
      #Choose two numeric variables
      selectInput(
        inputId = "corr_x",
        label = "x Variable",
        choices = numeric_vars,
        selected = "PINCP"
      ),
      selectInput(
        inputId = "corr_y",
        label = "y Variable",
        choices = numeric_vars,
        selected = "JWMNP"
      ),
      br(),
      h2("Choose a subset of the data: "),
      
      #Radio Buttons for household language, SNAP recipient, and educational attainment
      radioButtons(
        inputId = "hhl_corr",
        label = "Household Language",
        choices = c("All" = "all",
                    "English Only" = "english",
                    "Spanish" = "spanish",
                    "Other" = "other")),
      
      radioButtons(
        inputId = "fs_corr",
        label = "SNAP Recipient",
        choices = c("All" = "all",
                    "Yes" = "yes",
                    "No" = "no")),
      
      radioButtons(
        inputId = "schl_corr",
        label = "Educational attainment",
        choices = c("All" = "all",
                    "High School not Completed" = "no_hs",
                    "High School or GED" = "hs",
                    "Collge Degree" = "college")),
      h2("Select a Sample Size"),
      
      #Slider for the number of observations
      sliderInput(
        inputId = "corr_n",
        label = "",
        min = 20,
        max = 500,
        value = 20),
      actionButton("corr_sample","Get a Sample!"),
      width = 4
    ),
    mainPanel(
      plotOutput("corr_plot"),
      conditionalPanel("input.corr_sample > 0", #only show if a sample has been taken
                       h2("Guess the correlation!"),
                       column(6, 
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1, 
                                           max = 1
                              )
                       ),
                       column(6, 
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

my_sample <- readRDS("my_sample_temp.rds")

# Define server logic for drawing histogram
server <- function(input, output, session) {
  
  
  #Update y variable choices when x changes
  observeEvent(input$corr_x, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices,
                           selected = corr_y)
    }
  })
  #update the 'x' selections available
  observeEvent(input$corr_y, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y){
      choices <- choices[-which(choices == corr_y)]
      updateSelectizeInput(session,
                           "corr_x",
                           choices = choices,
                           selected = corr_x)
    }
  })
  
  
  #store sample and true correlation
  sample_corr <- reactiveValues(
    corr_data = NULL,
    corr_truth = NULL
  )
  
  #Take a new sample when the button is clicked
  observeEvent(input$corr_sample, {
    
    #Household filter
    if(input$hhl_corr == "all"){
      hhl_sub <- HHLvals
    } else if(input$hhl_corr == "english"){
      hhl_sub <- HHLvals["1"]
    } else if(input$hhl_corr == "spanish"){
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    
    #SNAP filter
    if(input$fs_corr == "all"){
      fs_sub <- FSvals
    } else if(input$fs_corr == "yes"){
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }
    
    #Education filter
    if(input$schl_corr == "all"){
      schl_sub <- SCHLvals
    } else if(input$schl_corr == "no_hs"){
      schl_sub <- SCHLvals[c("0", "01", "02", "03", "04",
                             "05", "06", "07", "08", "09",
                             "10", "11", "12", "13", "14", "15")]
    } else if(input$schl_corr == "hs"){
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    
    #get the numeric variables selectec
    corr_vars <- c(input$corr_x, input$corr_y)
    
    #filter data from the categories selected
    subsetted_data <- my_sample |>
      filter(#cat vars first
        HHLfac %in% hhl_sub,
        FSfac %in% fs_sub,
        SCHLfac %in% schl_sub
      ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}
    
    #Take weighted random sample
    index <- sample(1:nrow(subsetted_data),
                    size = input$corr_n,
                    replace = TRUE,
                    prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
    
    #Save the sample and calculate correlation
    sample_corr$corr_data <- subsetted_data[index,]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |>
                                    select(all_of(corr_vars)))[1,2]
  })
  
  #Create scatter plot output
  output$corr_plot <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    ggplot(sample_corr$corr_data, 
           aes_string(x = isolate(input$corr_x), 
                      y = isolate(input$corr_y))) +
      geom_point() +
      labs(x = isolate(input$corr_x),
           y = isolate(input$corr_y))
  })
  
  #Correlation guessing game
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ", 
                        round(sample_corr$corr_truth, 4), 
                        "."),
                 type = "success"
      )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!",
                   "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!",
                   "Try guessing a higher value.")
      }
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
