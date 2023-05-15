# Copyright 2021 Rainer Stollhoff
# MIT License
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(fPortfolio)){
  install.packages("fPortfolio")
  require(fPortfolio)
}

#Create Dataset
loaddata <- function(){
  filenames <- list.files("DaxData_5Y_weekly", pattern="*.csv", full.names=TRUE)
  
  df <- data.frame(read.csv(filenames[1]))[,c(1,6)]
  df$Date <- as.Date(as.character(df$Date),format="%Y-%m-%d")
  df <- df %>%
    mutate(perc_diff = as.double(100 * (Adj.Close - dplyr::lag(Adj.Close))/dplyr::lag(Adj.Close)))
  df[,2] <- NULL
  names(df)[2] <- gsub(".*/|.csv", "", filenames[1])
  
  
  for (i in 2:length(filenames)){
    file <- filenames[i]
    data <- data.frame(read.csv(file))[,c(1,6)]
    data$Date <- as.Date(as.character(data$Date),format="%Y-%m-%d")
    data <- data %>%
      mutate(perc_diff = as.double(100 * (Adj.Close - dplyr::lag(Adj.Close))/dplyr::lag(Adj.Close)))
    data[,2] <- NULL
    names(data)[2] <- gsub(".*/|.csv", "", filenames[i])
    df <- left_join(df,data,by="Date")
  }
  
  rownames(df) <- df$Date
  
  # remove NA 
  df <- df[-1,]
  df <- df[ , colSums(is.na(df)) == 0]
  df <- df[,-1]
  
  # create timeseries
  df <- as.timeSeries(df,format="%Y-%m-%d")

  return(df)
}

#Define dataset
#df <- SPISECTOR.RET
df <- loaddata()


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Portfolioanalyse"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("select_portfolio","Portfolioanalyse - Methode",
                   c("Gleichgewichtetes Portfolio" = "check_ggw",
                     "Portfolio mit global minimaler Varianz" = "check_miniVar",
                     "Tangentialportfolio" = "check_tang")),
      dateRangeInput("dateRange",
                     label= "Filter Date",
                     start = as.Date(rownames(df)[1]),
                     end = as.Date(rownames(df)[length(rownames(df))])),
      checkboxGroupInput("select_sector","Sektoren auswählen",
                      choices = c(names(df)),
                      selected = c(names(df)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("main_plot"),
      column(6,plotOutput("plot_gewichte")),
      column(6,plotOutput("plot_rendite")),
      plotOutput("plot_rand")
    )
  ),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- reactive({
    mindate <-input$dateRange[1]
    maxdate <- input$dateRange[2]
    dates <- rownames(df)
    datesfilter <- dates >= mindate & dates <= maxdate
    
    df <- df[datesfilter,c(input$select_sector)]
    df
  })
  
  # 1.Gleichgewichtetes Portfolio EWP
  fix_pf <- reactive({
    fix_spec <- portfolioSpec()
    
    setWeights(fix_spec) <- rep(1/length(names(data())), length(names(data())))
    
    feasiblePortfolio(
      data = data(),
      spec = fix_spec,
      constraints = "LongOnly")
  })
  
  # 2. Portfolio mit global minimaler Varianz
  global_pf <- reactive({
    global_spec <- portfolioSpec()
    
    minvariancePortfolio(
      data = data(),
      spec = global_spec,
      constraints = "LongOnly")
  })
  
  # 3 Portfolio mit optimalem Sharpe-Quotienten (Tangentialportfolio)
  sharpe_pf <- reactive({
    sharpe_spec <- portfolioSpec()
    setRiskFreeRate(sharpe_spec) <- 0.0000 # Da der Risikolose Zinssatz aktuell negativ ist, wird das Modell mit 0 fortgef?hrt
    
    sharpe_pf <- tangencyPortfolio(
      data = data(),
      spec = sharpe_spec,
      constraints = "LongOnly")
  })

  # Main Plot
  output$main_plot <- renderPlot({
    spi.pf <- portfolioFrontier(data())
    tailoredFrontierPlot(spi.pf)
  })
  
  methode <- reactive({
    switch(input$select_portfolio,
           check_ggw = fix_pf(),
           check_miniVar = global_pf(),
           check_tang = sharpe_pf(),
           fix_pf())
  })
  
  # Plot Gewichte
  output$plot_gewichte <- renderPlot({
    weightsPie(methode())
  })
  
  # Plot Rendite
  output$plot_rendite <- renderPlot({
    weightedReturnsPie(methode())
  })

  output$plot_rand <- renderPlot({
    spi.pf <- portfolioFrontier(data())
    weightsPlot(spi.pf)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
