library(shiny)

# Define UI
ui <- fluidPage(
  # Render the .RMD file as HTML using rmarkdown::render()
  htmlOutput("report")
)

# Define server
server <- function(input, output) {
  # Render the .RMD file as HTML using rmarkdown::render()
  output$report <- renderUI({
    file <- normalizePath(paste0("path/to/your/files/", input$uri))
    if (!file.exists(file)) {
      return(tags$div("The file you requested does not exist."))
    }
    if (substr(file, nchar(file) - 3, nchar(file)) != ".Rmd") {
      return(tags$div("The file you requested is not a valid .Rmd file."))
    }
    rmarkdown::render(file)
    HTML(paste(readLines(gsub(".Rmd", ".html", file)), collapse="\n"))
  })
}

# Run the application
shinyApp(ui, server, options = list(launch.browser = TRUE))
