# Start by setting working directory to location of the current interface script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load packages and functions
options(encoding = "UTF-8") # Make sure to use right encoding to avoid issues when first running model
source(file.path("Initialize/Load_packagesNfunctions.R"))
Time_model = 0

# Define UI for the application
ui <- fluidPage(
  
  # Initialize shinyjs
  useShinyjs(),
  
  # Application title
  titlePanel("Parameter Interface"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Dropdown for raw data options
      h3("Data"),
      selectInput("rawDataChoice", "Do you have raw data of the right structure (located: ../Data/Raw/rawdata.rds):",
                  choices = list("No, I don't have raw data" = 'no_raw_data',
                                 "Yes, I have raw data" = 'use_raw_data')),
      
      # Output for raw data status message
      uiOutput("DataCheck"),
      
      # Movement frequency options
      h3("Movement frequency"),
      conditionalPanel(
        condition = "input.rawDataChoice == 'use_raw_data'",
        radioButtons("moveFreq", "% of Movement used:",
                     choices = list("Default  - (uses included simulations)" = "default",
                                    "Input your own frequency" = "custom"))
      ),
      conditionalPanel(
        condition = "input.rawDataChoice != 'use_raw_data'",
        radioButtons("moveFreq", "% of Movement used:",
                     choices = list("Default - (uses included simulations)" = "default"))
      ),
      conditionalPanel(
        condition = "input.moveFreq == 'default'",
        radioButtons("defaultFreq", "Choose default movement frequency:",
                     choices = list("100% (all movements included)" = 100,
                                    "50% (half of movements included)" = 50,
                                    "33% (A third of movements included)" = 33))
      ),
      conditionalPanel(
        condition = "input.moveFreq == 'custom'",
        numericInput("customFreq", "Custom movement Frequency (1-100) (% of movements included):", value = NULL, min = 0, step = 1)
      ),
      
      # Transmission rate Beta options
      h3("Transmission Rate, Beta"),
      conditionalPanel(
        condition = "input.rawDataChoice == 'use_raw_data'",
        radioButtons("b", "Transmission rate Beta:",
                     choices = list("Default  - (uses included simulations)" = "default",
                                    "Input your own rate" = "custom",
                                    "Estimate Beta" = "estimate"))
      ),
      conditionalPanel(
        condition = "input.rawDataChoice != 'use_raw_data'",
        radioButtons("b", "Transmission rate Beta:",
                     choices = list("Default - (uses included simulations)" = "default"))
      ),
      conditionalPanel(
        condition = "input.b == 'default'",
        radioButtons("defaultBeta", "Choose default transmission rate:",
                     choices = list("Bacteria (0.05)" = "bacteria",
                                    "Virus (0.2)" = "virus"))
      ),
      conditionalPanel(
        condition = "input.b == 'custom'",
        numericInput("customBeta", "Custom transmission rate Beta:", value = NULL, min = 0, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.b == 'estimate'",
        textInput("timeVector", "Time in days (comma-separated):"),
        textInput("infectedVector", "Number of infected (comma-separated):"),
        textOutput("estimatedBeta"),
        textOutput("estimateError")
      ),
      
      # Pathogen option
      h3("Agent Type"),
      checkboxInput("pathogen", "The agent is a pathogen and causes clinical symptoms in infected animals"),
      
      # Testing parameters options
      h3("Testing Parameters"),
      conditionalPanel(
        condition = "input.rawDataChoice == 'use_raw_data'",
        radioButtons("testingParams", "Testing parameters:",
                     choices = list("Default  - (uses included simulations)" = "default",
                                    "Input your own parameters" = "custom"))
      ),
      conditionalPanel(
        condition = "input.rawDataChoice != 'use_raw_data'",
        radioButtons("testingParams", "Testing parameters:",
                     choices = list("Default - (uses included simulations)" = "default"))
      ),
      conditionalPanel(
        condition = "input.testingParams == 'default'",
        radioButtons("defaultNTest", "Choose default number of daily tests:",
                     choices = list("5 tests per day" = 5,
                                    "50 tests per day" = 50)),
        radioButtons("defaultSensitivity", "Choose default test sensitivity:",
                     choices = list("Sensitivity 0.85" = 0.85,
                                    "Sensitivity 0.7" = 0.7))
      ),
      conditionalPanel(
        condition = "input.testingParams == 'custom'",
        numericInput("customNTest", "Custom number of daily tests:", value = NULL, min = 1, step = 1),
        numericInput("customSensitivity", "Custom test sensitivity:", value = NULL, min = 0, max = 1, step = 0.01)
      ),
      # Button to run the model
      actionButton("run_model", "Run Model")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Display the selected parameters
      textOutput("moveFreqValue"),
      textOutput("bValue"),
      textOutput("nTestValue"),
      textOutput("sensitivityValue"),
      textOutput("errorMessage")
    )  # Closing parenthesis for mainPanel
  )  # Closing parenthesis for sidebarLayout
)  # Closing parenthesis for fluidPage 

server <- function(input, output, session) {
  observe({
    if (input$moveFreq == "custom"){
      shinyjs::show("customFreq")
    } else {
      shinyjs::hide("customFreq")
    }
    if (input$b == "custom") {
      shinyjs::show("customBeta")
      shinyjs::hide("timeVector")
      shinyjs::hide("infectedVector")
      shinyjs::hide("estimatedBeta")
      shinyjs::hide("estimateError")
    } else if (input$b == "estimate") {
      shinyjs::hide("customBeta")
      shinyjs::show("timeVector")
      shinyjs::show("infectedVector")
      shinyjs::show("estimatedBeta")
      shinyjs::show("estimateError")
    } else {
      shinyjs::hide("customBeta")
      shinyjs::hide("timeVector")
      shinyjs::hide("infectedVector")
      shinyjs::hide("estimatedBeta")
      shinyjs::hide("estimateError")
    }
    if (input$testingParams == "custom") {
      shinyjs::show("customNTest")
      shinyjs::show("customSensitivity")
    } else {
      shinyjs::hide("customNTest")
      shinyjs::hide("customSensitivity")
    }
  })
  
  estimatedBeta <- reactive({
    if (input$b == "estimate") {
      time_vector <- as.numeric(unlist(strsplit(input$timeVector, ",")))
      infected_vector <- as.numeric(unlist(strsplit(input$infectedVector, ",")))
      if (length(time_vector) < 2 || length(infected_vector) < 2) {
        return("Error: Vectors must have at least 2 entries.")
      }
      if (length(time_vector) != length(infected_vector)) {
        return("Error: Vectors must be of equal length.")
      }
      try({
        logistic_model <- nlsLM(
          infected_vector ~ K / (1 + ((K - infected_vector[1]) / infected_vector[1]) * exp(-r * time_vector)),
          start = list(K = max(infected_vector), r = 0.1),
          control = nls.lm.control(maxiter = 500)
        )
        beta_estimate <- coef(logistic_model)["r"]
        return(paste("Estimated transmission rate Beta is:", round(beta_estimate, 4)))
      }, silent = TRUE)
    }
  })
  output$estimatedBeta <- renderText({
    estimatedBeta()
  })
  
  DataCheck <- reactive({
    rawDataCheck <- file.exists(file.path(dirname(getwd()), "Data/Raw"))
    if (input$rawDataChoice == "use_raw_data") {
      if (!rawDataCheck) {
        return(div(style="color: red;", "ERROR: Could not find raw data: Make sure data is stored correctly /Data/Raw/yourdata_files OR set rawData = No"))
      } else {
        return(div(style="color: green;", "The Raw data folder is found and can be used to simulate the model"))
      }
    } else {
      if (rawDataCheck) {
        return(div(style="color: orange;", "OBS: A Raw data folder was found under Data, if you wish to use it please change; rawData = Yes"))
      } else {
        return(div(style="color: green;", "The model will be run with data based on previous simulations"))
      }
    }
  })
  output$DataCheck <- renderUI({
    DataCheck()
  })
  
  observeEvent(input$run_model, {
    rawData <<- ifelse(input$rawDataChoice == 'use_raw_data', TRUE, FALSE)
    move_freq <<- if (input$moveFreq == "custom") input$customFreq else input$defaultFreq
    b <<- if (input$b == "custom") input$customBeta else if (input$b == "estimate") as.numeric(sub(".: (.)", "\1", estimatedBeta())) else if (input$defaultBeta == "bacteria") 0.05 else 0.2
    n_test <<- if (input$testingParams == "custom") as.numeric(input$customNTest) else as.numeric(input$defaultNTest)
    test_sensitivity <<- if (input$testingParams == "custom") as.numeric(input$customSensitivity) else as.numeric(input$defaultSensitivity)
    pathogen <<- input$pathogen
    
    output$moveFreqValue <- renderText({
      paste("Selected movement frequency is:", move_freq, "%")
    })
    
    output$bValue <- renderText({
      paste("Selected transmission rate Beta is:", b)
    })
    
    output$nTestValue <- renderText({
      paste("Number of daily tests is:", n_test)
    })
    
    output$sensitivityValue <- renderText({
      paste("Test sensitivity is:", test_sensitivity)
    })
    
    # Source the model script with the set parameters
    source(file.path("Modules/RunningModel.R"), local = TRUE)
    
    # Show message when the model is done running
    showModal(
      modalDialog(
        title = "Model Execution Complete",
        HTML(paste(
          "The model has succesfully compleated - Results in; <b><i>Results/OutputModel.","</i>", "</b>", 
          "<br><br>", 
          "<b>Total model run time: ", Time_model, "</b>", "<br><br>",
          "Parameters used:", "<br><br>",
          "<ul>",
          "<li>Movement frequency = ", move_freq, "%</li>",
          "<li>Beta = ", b, "</li>",
          "<li>Number of daily tests at slaughter = ", n_test, "</li>",
          "<li>Test sensitivity = ", test_sensitivity, "</li>",
          "</ul>"
        ))
      )
    )
  })
}

shinyApp(ui = ui, server = server)
