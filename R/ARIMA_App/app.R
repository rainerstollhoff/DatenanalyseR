# Copyright 2021 Rainer Stollhoff
# MIT License
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

library(shiny)

simulate_arima <- function(w=NULL,phi,d,theta,sigma2,n){
  ##Zufallsprozess
  if(is.null(w)) w <- rnorm(n=n,sd = sqrt(sigma2))
  ## Verschiedene ARMA-Prozesse
  p <- length(phi)
  q <- length(theta)
  if(max(p,q)>0){
    w <- c(rep(0,max(p,q)),w)
    x <- w
    phi <- c(0,phi)
    theta <- c(1,theta)
    for(t in (1+max(p,q)):length(w)){
      x[t] <- sum(phi*x[t-(0:p)])+sum(theta*w[t-(0:q)]) #ARMA(p,q)
    } 
    x <- x[-(1:max(p,q))]
  }else{
    x <- w
  }
  if(d>0){
   for(de in 1:d){
     x <- cumsum(x)
   }
  }
  return(x) 
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ARIMA-Prozesse"),

    # Sidebar with a slider input for number of observations 
    sidebarLayout(
        sidebarPanel(
            sliderInput("set_n",
                        "Anzahl Zeitschritte:",
                        min = 10,
                        max = 1000,
                        value = 100),
            
            sliderInput("set_ylim",
                        "Wertebereich des Plots festlegen",
                        min = -100,
                        max = 100,
                        value = c(-10,10)),
            
            checkboxInput('set_w',"Feste Zufallskomponente für alle Prozesse"),
            conditionalPanel(
              condition = "input.set_w == true",
              numericInput("w_sigma2",
                          "Varianz der Zufallskomponente (Weisses Rauschen) festlegen:",
                          min = 1,
                          max = 10,
                          value = 1),
              ),
            
            checkboxInput('set_arima1',"ARIMA-Prozess hinzufügen"),
            conditionalPanel(
              condition = "input.set_arima1 == true",
              textInput('arima1_ar', 'Geben Sie die AR Koeffizienten Phi an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.5,0.2"),
              textInput('arima1_ma', 'Geben Sie die MA Koeffizienten Theta an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.3,0.2,0.1"),
              sliderInput("arima1_d",
                          "Differenzierungsordnung d festlegen:",
                          min = 0,
                          max = 10,
                          value = 0),
              conditionalPanel(
                condition = "input.set_w == false",
                numericInput("arima1_sigma2",
                             "Varianz der Zufallskomponente (Weisses Rauschen) festlegen:",
                             min = 1,
                             max = 10,
                             value = 1),
              )
            ),
            checkboxInput('set_arima2',"ARIMA-Prozess hinzufügen"),
            conditionalPanel(
              condition = "input.set_arima2 == true",
              textInput('arima2_ar', 'Geben Sie die AR Koeffizienten Phi an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.5,0.2"),
              textInput('arima2_ma', 'Geben Sie die MA Koeffizienten Theta an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)',  "0.3,0.2,0.1"),
              sliderInput("arima2_d",
                          "Differenzierungsordnung d festlegen:",
                          min = 0,
                          max = 10,
                          value = 0),
              conditionalPanel(
                condition = "input.set_w == false",
                numericInput("arima2_sigma2",
                             "Varianz der Zufallskomponente (Weisses Rauschen) festlegen:",
                             min = 1,
                             max = 10,
                             value = 1),
              )
            ),
            checkboxInput('set_arima3',"ARIMA-Prozess hinzufügen"),
            conditionalPanel(
              condition = "input.set_arima3 == true",
                textInput('arima3_ar', 'Geben Sie die AR Koeffizienten Phi an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.5,0.2"),
                textInput('arima3_ma', 'Geben Sie die MA Koeffizienten Theta an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.3,0.2,0.1"),
                sliderInput("arima3_d",
                          "Differenzierungsordnung d festlegen:",
                          min = 0,
                          max = 10,
                          value = 0),
              conditionalPanel(
                condition = "input.set_w == false",
                numericInput("arima3_sigma2",
                             "Varianz der Zufallskomponente (Weisses Rauschen) festlegen:",
                             min = 1,
                             max = 10,
                             value = 1),
              )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          titlePanel("Simulierte Prozesse"), 
          plotOutput("arima_Plot"),
          fluidRow(
            column(width=6,
                   titlePanel("emp. Autokorrelationsf."), 
                   plotOutput("acf_Plot")
                   ),
            column(width=6,
                   titlePanel("emp. partielle Autokorrelationsf."), 
                   plotOutput("pacf_Plot")
            )
          )
        )
    ),
    # Lizenz
    hr(),
    print("© 2021 by Rainer Stollhoff, licensed under MIT license. To view a copy of this license, visit https://spdx.org/licenses/MIT.html")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    methods_selected <- reactive({
      c(
        "ARIMA1" = input$set_arima1, 
        "ARIMA2" = input$set_arima2, 
        "ARIMA3" = input$set_arima3
      )
    })
    
    gwn <- reactive({
      w0 <- rnorm(n=input$set_n,sd = 1)
      w1 <- w2 <- w3 <- NULL
      if(input$set_w) w0 <- w1 <- w2 <- w3 <- rnorm(n=input$set_n,sd = sqrt(input$w_sigma2))
      list("w0"=w0,"w1"=w1,"w2"=w2,"w3"=w3)
    })
    
    methods_results <-  reactive({
      list(
        "ARIMA1" = simulate_arima(w=gwn()[["w1"]],
                                  phi=as.numeric(unlist(strsplit(input$arima1_ar,","))),
                                  d=input$arima1_d,
                                  theta=as.numeric(unlist(strsplit(input$arima1_ma,","))),
                                  sigma2=input$arima1_sigma2,
                                  n=input$set_n),
        "ARIMA2" = simulate_arima(w=gwn()[["w2"]],
                                  phi=as.numeric(unlist(strsplit(input$arima2_ar,","))),
                                  d=input$arima2_d,
                                  theta=as.numeric(unlist(strsplit(input$arima2_ma,","))),
                                  sigma2=input$arima2_sigma2,
                                  n=input$set_n),
        "ARIMA3" = simulate_arima(w=gwn()[["w3"]],
                                  phi=as.numeric(unlist(strsplit(input$arima3_ar,","))),
                                  d=input$arima3_d,
                                  theta=as.numeric(unlist(strsplit(input$arima3_ma,","))),
                                  sigma2=input$arima3_sigma2,
                                  n=input$set_n)
      )
    })
      
    output$arima_Plot <- renderPlot({
      if(input$set_w){w_plot <- gwn()[["w1"]]}else{w_plot <- gwn()[["w0"]]}
      plot(w_plot,
           ylab="x bzw. w",
           xlab="t",
           ylim=input$set_ylim,
           main="Darstellung der Stochastischen Prozesse")
      abline(h=0,lty=2)
      
      if(any(methods_selected())){
        for(met in which(methods_selected())){
          lines(methods_results()[[met]],col=met+1)
        }
        legend("topleft",legend=c("Weisses Rauschen",names(methods_selected())[which(methods_selected())]),
               col=c(1,which(methods_selected())+1),
               lty=1)
      }
    })
    
    output$acf_Plot <- renderPlot({
      if(input$set_w){w_plot <- gwn()[["w1"]]}else{w_plot <- gwn()[["w0"]]}
      plot(acf(w_plot,lag.max=10,plot=F),type="n",main="Autokorrelation",ylim=c(-1,1))
      if(any(methods_selected())){
        for(met in which(methods_selected())){
          lines(0:10,acf(methods_results()[[met]],lag.max=10,plot=F)$acf,col=met+1)
        }
        legend("topright",legend=c(names(methods_selected())[which(methods_selected())]),
               col=(which(methods_selected())+1),
               lty=1)
      }
    })
    
    output$pacf_Plot <- renderPlot({
      if(input$set_w){w_plot <- gwn()[["w1"]]}else{w_plot <- gwn()[["w0"]]}
      plot(pacf(w_plot,lag.max=10,plot=F),type="n",main="partielle Autokorrelation",ylim=c(-1,1))
      if(any(methods_selected())){
        for(met in which(methods_selected())){
          lines(pacf(methods_results()[[met]],lag.max=10,plot=F)$acf,col=met+1)
        }
        legend("topright",legend=c(names(methods_selected())[which(methods_selected())]),
               col=(which(methods_selected())+1),
               lty=1)
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
