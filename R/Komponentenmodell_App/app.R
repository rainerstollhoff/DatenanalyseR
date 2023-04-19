#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

##exponentielles Glaetten erster Ordnung
es <- function(x,alpha){
  n <- x # Übernahme des Datentyps und der Länge
  n[1] <- x[1]
  for(t in 2:length(x)){
    n[t] <- alpha*x[t]+(1-alpha)*x[t-1]
  }
  n
}

##exponentielles Glaetten zweiter Ordnung
esg <- function(x,alpha,beta){
  n <- m <- x # Übernahme des Datentyps und der Länge
  n[1] <- x[1]
  m[1] <- x[2]-x[1]
  for(t in 2:length(x)){
    n[t] <- alpha*x[t]+(1-alpha)*(n[t-1]+m[t-1])
    m[t] <- beta*(n[t]-n[t-1]) + (1-beta)*m[t-1]
  }
  n
}

ts2Date <- function(ints){
  as.Date(paste(c(ints,1),collapse="-"),format="%Y-%m-%d")
}

Date2ts <- function(ind){
  as.numeric(strsplit(as.character(ind),split = "-",fixed = T)[[1]][1:2])
}

data_ts <- ts(co2,start=1959,frequency = 12)
data_lr <- data.frame("x"=data_ts,"t"=time(data_ts),"s"=factor(cycle(data_ts)))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Klassische Zeitreihenanalyse Komponentenzerlegung"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("ts_window",
                        "Zeitausschnitt anzeigen:",
                        min = ts2Date(start(data_ts)),
                        max = ts2Date(end(data_ts)),
                        value = c(ts2Date(start(data_ts)),ts2Date(end(data_ts)))),
            sliderInput("ts_filter",
                         "Filtern der Originaldaten - Anzahl Werte pro Jahr",
                        min=1,
                        max=12,
                        value=12),
            
            checkboxInput('set_lr',"Lineare Regression hinzufügen"),
            
            checkboxInput('set_ma',"Gleitenden Durchschnitt hinzufügen"),
            conditionalPanel(
              condition = "input.set_ma == true",
              sliderInput("ma_window",
                          "Horizont festlegen:",
                          min = -12,
                          max = 12,
                          value = c(-6,6))),
            
            checkboxInput('set_es',"Exponentielle Glättung hinzufügen"),
            conditionalPanel(
              condition = "input.set_es == true",
              numericInput('es_alpha','Niveauparameter alpha',value=1),
              numericInput('es_beta','Wachstumsparameter beta',value=1)
            ),
            
            checkboxInput('set_ms',"Gleitenden Durchschnitt mit additiven Saisonkomponenten hinzufügen"),
            conditionalPanel(
              condition = "input.set_ms == true",
              sliderInput("ms_window",
                          "Horizont festlegen:",
                          min = -12,
                          max = 12,
                          value = c(-6,6))),
            
            checkboxInput('set_ls',"Lineare Regression mit mit additiven Saisonkomponenten hinzufügen"),
            
            checkboxInput('set_hw',"Holt-Winters Modell (additiv) hinzufügen"),
            conditionalPanel(
              condition = "input.set_hw == true",
              numericInput('hw_alpha','Niveauparameter alpha',value=1),
              numericInput('hw_beta','Wachstumsparameter beta',value=1),
              numericInput('hw_gamma','Wachstumsparameter gamma',value=1)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          titlePanel("Graph der Zeitreihe"), 
          plotOutput("ts_Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    methods_selected <- reactive({
      c(
        "Lineare Regression" = input$set_lr, 
        "Gleitender Durchschnitt" = input$set_ma,
        "Exponentielle Glättung" = input$set_es, 
        "Lineare Regression mit SK" = input$set_ls, 
        "Gleitender Durchschnitt mit SK" = input$set_ms, 
        "Holt-Winters" = input$set_hw
      )
    })
    
    methods_results <-  reactive({
      dec <- decompose(data_ts,
                       filter = rep(1/(input$ms_window[2]-input$ms_window[1]+1),(input$ms_window[2]-input$ms_window[1]+1)),
                       type="additive")
      list(
        "Lineare Regression" = ts(predict(lm(data=data_lr, x ~ t)),
                                      start=start(data_ts),frequency=12),
        "Gleitender Durchschnitt" = filter(data_ts,
                                           filter = rep(1/(input$ma_window[2]-input$ma_window[1]+1),(input$ma_window[2]-input$ma_window[1]+1)),
                                           sides = 1 + input$ma_window[2] > 0,
                                           method="convolution"),
        "Exponentielle Glättung" = esg(data_ts,input$es_alpha,input$es_beta),
        "Lineare Regression mit SK" = ts(predict(lm(data=data_lr, x ~ t +s)),
                                             start=start(data_ts),frequency=12), 
        "Gleitender Durchschnitt mit SK" = dec$seasonal + dec$trend, 
        "Holt-Winters" = HoltWinters(data_ts,"additive",
                                     alpha=input$hw_alpha,
                                     beta=input$hw_beta,
                                     gamma=input$hw_gamma)$fitted[,"xhat"]
      )
    })
      
    output$ts_Plot <- renderPlot({
      plot(window(data_ts,
                  frequency=input$ts_filter,
                  start = Date2ts(input$ts_window[1]),
                  end = Date2ts(input$ts_window[2])),
           ylab="x",
           xlab="t")
      if(any(methods_selected())){
        for(met in which(methods_selected())){
          lines(methods_results()[[met]],col=met+1)
        }
        legend("topleft",legend=c("Original",names(methods_selected())[which(methods_selected())]),
               col=c(1,which(methods_selected())+1),
               lty=1)
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
