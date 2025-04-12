#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(shinyjs)
# Set CRAN mirror globally
options(repos = c(CRAN = "https://cloud.r-project.org"))
ui <- fluidPage(
  titlePanel("Chat with Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV (optional)", accept = ".csv"),
      textAreaInput("question", "Your Question:", 
                    value = "Show relationship between mpg and wt with regression line",
                    rows = 3),
      actionButton("analyze", "Analyze"),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Analysis", uiOutput("analysis_output")),
        tabPanel("Results",
                 h4("Generated Code"),
                 verbatimTextOutput("code_display"),
                 
                 # Changed validation messages to be collapsible
                 #actionButton("show_validation", "Show Validation Messages", 
                 #              icon = icon("chevron-down"),
                 #             style = "margin-bottom: 10px;"),
                 #hidden(
                 #   div(id = "validation_panel",
                 #      h4("Validation Messages"),
                 #       verbatimTextOutput("validation_output")
                 # )
                 #),
                 
                 h4("Text Output"),
                 verbatimTextOutput("text_output"),
                 h4("Plot Output"),
                 plotOutput("dynamic_plot", height = "500px"),
                 h4("Summary & Conclusions"),
                 #verbatimTextOutput("summary_output")),
                 #verbatimTextOutput("summary_output", placeholder = TRUE)),
                 div(style = "height: 17em; overflow-y: auto; border: 1px solid #ccc; padding: 8px; white-space: pre-wrap; background-color: #f9f9f9;",
                     textOutput("summary_output")
                 )),
        tabPanel("Data", 
                 h4("Column Names"),
                 verbatimTextOutput("column_names"),
                 h4("Data Preview"),
                 tableOutput("data_preview"))
      ),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  # Reactive data storage
  data <- reactiveVal(mtcars)
  
  # Toggle validation panel visibility
  #observeEvent(input$show_validation, {
  #  toggle("validation_panel")
  #  if (input$show_validation %% 2 == 1) {
  #    updateActionButton(session, "show_validation", 
  #                       icon = icon("chevron-up"),
  #                       label = "Hide Validation Messages")
  #  } else {
  #    updateActionButton(session, "show_validation",
  #                       icon = icon("chevron-down"),
  #                       label = "Show Validation Messages")
  #  }
  #})
  
  # Handle file uploads
  observeEvent(input$file, {
    tryCatch({
      new_data <- read_csv(input$file$datapath)
      data(new_data)
    }, error = function(e) {
      showNotification("Error reading file. Using default data.", type = "error")
      data(mtcars)
    })
  })
  
  # Data preview and column names
  output$data_preview <- renderTable({
    head(data(), 5)
  })
  
  output$column_names <- renderPrint({
    cat("Available columns:\n")
    cat(paste(names(data()), collapse = ", "), "\n")
  })
  
  # Function to validate code against dataset
  validate_code <- function(code, dataset) {
    errors <- character(0)
    warnings <- character(0)
    
    # Check for variable references
    vars_in_code <- unique(str_extract_all(code, "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b")[[1]])
    vars_in_code <- vars_in_code[!vars_in_code %in% c("plot_data", "ggplot", "print", "library", "require")]
    
    missing_vars <- vars_in_code[!vars_in_code %in% names(dataset)]
    
    if (length(missing_vars) > 0) {
      warnings <- c(warnings, paste("Potential missing variables:", paste(missing_vars, collapse = ", ")))
      
      # Try to find similar column names
      suggestions <- lapply(missing_vars, function(var) {
        agrep(var, names(dataset), max.distance = 0.3, value = TRUE)
      })
      
      if (any(sapply(suggestions, length) > 0)) {
        warnings <- c(warnings, "Did you mean:")
        for (i in seq_along(missing_vars)) {
          if (length(suggestions[[i]]) > 0) {
            warnings <- c(warnings, paste(missing_vars[i], "->", paste(suggestions[[i]], collapse = " or ")))
          }
        }
      }
    }
    
    # Check for ggplot aesthetics
    if (str_detect(code, "ggplot")) {
      aes_matches <- str_extract_all(code, "aes\\(([^)]+)\\)")[[1]]
      aes_vars <- unique(unlist(str_extract_all(aes_matches, "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b")))
      aes_vars <- aes_vars[!aes_vars %in% c("aes", "x", "y", "color", "fill", "size")]
      
      missing_aes <- aes_vars[!aes_vars %in% names(dataset)]
      
      if (length(missing_aes) > 0) {
        errors <- c(errors, paste("Missing ggplot aesthetics variables:", paste(missing_aes, collapse = ", ")))
      }
    }
    
    list(errors = errors, warnings = warnings)
  }
  
  # Function to generate summary from analysis
  generate_summary <- function(question, text_output, plot_data) {
    prompt <- paste(
      "Based on the following analysis results, provide a concise summary (3-5 sentences) with conclusions:\n",
      "Original question: ", question, "\n",
      "Analysis output:\n", text_output, "\n",
      "Dataset variables: ", paste(names(plot_data), collapse = ", "), "\n",
      "Focus on key findings, statistical significance, and practical implications. ",
      "Use simple, non-technical language where possible.",
      sep = ""
    )
    
    api_key <- Sys.getenv("API_KEY")
    
    if (api_key == "") {
      return("Summary unavailable: API key not configured")
    }
    
    response <- tryCatch({
      POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)),
        body = list(
          model = "gpt-4-turbo",
          messages = list(list(role = "user", content = prompt)),
          temperature = 0.2
        ),
        encode = "json",
        timeout = 20
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(response) || status_code(response) != 200) {
      return("Summary unavailable: API error")
    }
    
    content(response)$choices[[1]]$message$content
  }
  
  # Main analysis function
  observeEvent(input$analyze, {
    req(input$question)
    
    showNotification("Analyzing...", type = "message", duration = NULL)
    on.exit(removeNotification("analyzing"))
    
    tryCatch({
      current_data <- data()
      
      # Build prompt with actual column names
      prompt <- paste(
        "You are analyzing a dataset with these columns: [", 
        paste(names(current_data), collapse = ", "), "].\n\n",
        "For the question: '", input$question, "'\n\n",
        "Provide R code that:\n",
        "1. Uses ONLY these variables from the dataset\n",
        "2. Creates a visualization using ggplot2\n",
        "3. Computes relevant statistics\n",
        "4. Uses the dataset which is already loaded as 'plot_data'\n",
        "IMPORTANT: Use print() on the ggplot object\n",
        "Format response EXACTLY like this:\n```r\n# R code here\n```\n",
        "Do not include any text outside the code block.",
        sep = ""
      )
      
      # API call - Use your preferred method to get the API key
      api_key <- Sys.getenv("API_KEY")
      
      if (api_key == "") {
        stop("API key not configured")
      }
      
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)),
        body = list(
          model = "gpt-4-turbo",
          messages = list(list(role = "user", content = prompt)),
          temperature = 0.3
        ),
        encode = "json",
        timeout = 20
      )
      
      if (status_code(response) != 200) {
        stop(paste("API Error:", content(response, "text")))
      }
      
      ai_content <- content(response)$choices[[1]]$message$content
      
      # Display analysis text
      output$analysis_output <- renderUI({
        HTML(gsub("\n", "<br>", ai_content))
      })
      
      # Extract R code
      r_code <- str_match(ai_content, "```(?:r)?\\n([\\s\\S]*?)```")[,2]
      
      if (!is.na(r_code) && nchar(trimws(r_code)) > 0) {
        # Clean the code
        r_code <- trimws(gsub("^```r?|```$", "", r_code))
        
        # Validate the code
        validation <- validate_code(r_code, current_data)
        
        # Display validation messages (hidden by default)
        output$validation_output <- renderPrint({
          if (length(validation$errors) > 0) {
            cat("ERRORS:\n")
            cat(paste(validation$errors, collapse = "\n"), "\n\n")
          }
          if (length(validation$warnings) > 0) {
            cat("WARNINGS:\n")
            cat(paste(validation$warnings, collapse = "\n"), "\n")
          }
          if (length(validation$errors) == 0 && length(validation$warnings) == 0) {
            cat("Code validation passed successfully - no issues found\n")
          }
        })
        
        # Only proceed if no critical errors
        if (length(validation$errors) == 0) {
          # Display raw code
          output$code_display <- renderPrint({
            cat(r_code)
          })
          
          # Create execution environment
          env <- new.env(parent = globalenv())
          env$plot_data <- current_data
          env$ggplot <- ggplot2::ggplot
          
          # Execute code and capture output
          text_result <- capture.output({
            tryCatch({
              eval(parse(text = r_code), envir = env)
            }, error = function(e) {
              paste("Execution error:", e$message)
            })
          })
          
          output$text_output <- renderPrint({
            cat(text_result, sep = "\n")
          })
          
          # Generate and display summary
          output$summary_output <- renderPrint({
            if (length(text_result) > 0 && !any(grepl("Execution error", text_result))) {
              summary_text <- generate_summary(input$question, paste(text_result, collapse = "\n"), current_data)
              cat(summary_text)
            } else {
              cat("Summary unavailable due to execution errors")
            }
          })
          
          # Render plot
          output$dynamic_plot <- renderPlot({
            # Create fresh environment for plot
            plot_env <- new.env(parent = globalenv())
            plot_env$plot_data <- current_data
            plot_env$ggplot <- ggplot2::ggplot
            
            # Evaluate the code
            result <- tryCatch({
              eval(parse(text = r_code), envir = plot_env)
            }, error = function(e) {
              showNotification(paste("Plot error:", e$message), type = "error")
              return(NULL)
            })
            
            # Handle ggplot objects
            if (!is.null(result) && inherits(result, "ggplot")) {
              print(result)
            } else if (exists("last_plot", envir = plot_env)) {
              print(plot_env$last_plot)
            }
          })
        }
      } else {
        showNotification("No executable code found in response", type = "warning")
        output$code_display <- renderPrint({
          cat("Full AI response:\n", ai_content)
        })
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

shinyApp(ui, server)
