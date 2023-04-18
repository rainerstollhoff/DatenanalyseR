library(shiny)

# Define UI
ui <- fluidPage(
  # Render the .RMD file as HTML using rmarkdown::render()
  htmlOutput("report")
)

# Define server
server <- function(input, output, session) {
  # Render the .RMD file as HTML using rmarkdown::render()
  output$report <- renderUI({
    file <- normalizePath(paste0(getwd(), "/", input$uri))
    if (!file.exists(file)) {
      available_files <- list.files(getwd())
      error_message <- paste("The file you requested does not exist.", 
                             "<br>Requested file:", input$uri, 
                             "<br>HTTP request:", toString(session$request), 
                             "<br>Available files:", paste(available_files, collapse = ", "))
      return(tags$div(error_message))
    }
    if (tolower(substr(file, nchar(file) - 3, nchar(file))) != ".rmd") {
      error_message <- paste("The file you requested is not a valid .Rmd file.", 
                             "<br>Requested file:", input$uri, 
                             "<br>HTTP request:", toString(session$request))
      return(tags$div(error_message))
    }
    rmarkdown::render(file)
    HTML(paste(readLines(gsub(".Rmd", ".html", file)), collapse="\n"))
  })
}

# Run the application
shinyApp(ui, server, options = list(launch.browser = TRUE))
