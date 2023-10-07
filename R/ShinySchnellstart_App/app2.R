#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with
    sidebarLayout(
        sidebarPanel(
          
          # Radio Button for the choice of the variable
            radioButtons("var",
                         "Variablenauswahl",
                         choices = colnames(faithful)),
            
            # Checkbox for the selection of variables
            checkboxGroupInput("stats",
                               "Statistiken",
                               choices = c("Mittelwert","Median","Minimum","Maximum")),
            
            # a slider input for number of bins 
            sliderInput("bins",
                        "Number of bins:",
                        min = 5,
                        max = 20,
                        value = 10)
            
        ),

        # Show a plot of the generated distribution and the statistics table 
        mainPanel(
          fluidRow(
            column(6,
              plotOutput("distPlot")
            ),
            column(6,
              textOutput("statHeader"),
              tableOutput("statTab")
            )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # select variable based on input$var from ui.R 
        x    <- faithful[,input$var]
        # generate bins based on input$bins from ui.R
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Eruption times',
             main = 'Histogram of eruption times')
    })
    
    output$statHeader <- renderText({"Ausgewählte Statistiken"})
    
    output$statTab <- renderTable({
      # select variable based on input$var from ui.R 
      x    <- faithful[,input$var]
     
      # Erstelle eine Funktion
      multiple.func <- function(x) {
        c(Mittelwert = mean(x), 
          Median = median(x), 
          Minimum = min(x), 
        Maximum = max(x))
      }
      multiple.func(x)[input$stats]
    },rownames = T, colnames = F
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
