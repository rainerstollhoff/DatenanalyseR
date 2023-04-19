#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

simulate_garch <- function(w=NULL,omega,alpha,beta,n){
  ##Zufallsprozess
  if(is.null(w)) w <- rnorm(n=n)
  ## Verschiedene GARCH-Prozesse
  p <- length(alpha)
  q <- length(beta)
  x <- numeric(n)
  sigma <- rep(1,n) 
  for(t in (max(p,q)+1):n){
    sigma[t] <- sqrt(omega+sum(beta*sigma[t-(1:q)]^2)+sum(alpha*x[t-(1:p)]^2))
    x[t] <- w[t]*sigma[t] 
  }
  return(list("w"=w,"x"=x,"sigma"=sigma))
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GARCH-Prozesse"),

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
                        min = -20,
                        max = 20,
                        value = c(-2,2)),
            checkboxInput('set_type',"Prozesse als Linie anzeigen"),
            
            checkboxInput('set_w',"Feste Zufallskomponente für alle Prozesse"),
          
            checkboxInput('set_garch1',"GARCH-Prozess hinzufügen"),
            conditionalPanel(
              condition = "input.set_garch1 == true",
              numericInput('garch1_omega', 'Geben Sie den Koeffizienten Omega an (mit Komma als Dezimaltrennzeichen)', "0.4"),
              textInput('garch1_alpha', 'Geben Sie die Koeffizienten Alpha an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.3,0.1"),
              textInput('garch1_beta', 'Geben Sie die Koeffizienten Beta an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.2,0.1,0.1"),
            ),
            checkboxInput('set_garch2',"GARCH-Prozess hinzufügen"),
            conditionalPanel(
              condition = "input.set_garch2 == true",
              numericInput('garch2_omega', 'Geben Sie den Koeffizienten Omega an (mit Komma als Dezimaltrennzeichen)', "0.4"),
              textInput('garch2_alpha', 'Geben Sie die Koeffizienten Alpha an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.3,0.1"),
              textInput('garch2_beta', 'Geben Sie die Koeffizienten Beta an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.2,0.1,0.1"),
            ),
            checkboxInput('set_garch3',"GARCH-Prozess hinzufügen"),
            conditionalPanel(
              condition = "input.set_garch3 == true",
              numericInput('garch3_omega', 'Geben Sie den Koeffizienten Omega an (mit Komma als Dezimaltrennzeichen)', "0.4"),
              textInput('garch3_alpha', 'Geben Sie die Koeffizienten Alpha an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.3,0.1"),
              textInput('garch3_beta', 'Geben Sie die Koeffizienten Beta an (mit Komma getrennt und Punkt als Dezimaltrennzeichen)', "0.2,0.1,0.1"),
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          titlePanel("Simulierte Prozesse"), 
          plotOutput("garch_Plot")
          )
      )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
    methods_selected <- reactive({
      c(
        "GARCH1" = input$set_garch1, 
        "GARCH2" = input$set_garch2, 
        "GARCH3" = input$set_garch3
      )
    })
    
    gwn <- reactive({
      w0 <- rnorm(n=input$set_n,sd = 1)
      w1 <- w2 <- w3 <- NULL
      if(input$set_w) w1 <- w2 <- w3 <- rnorm(n=input$set_n)
      list("w0"=w0,"w1"=w1,"w2"=w2,"w3"=w3)
    })
    
    methods_results <-  reactive({
      list(
        "GARCH1" = simulate_garch(w=gwn()[["w1"]],
                                  omega=input$garch1_omega,
                                  alpha=as.numeric(unlist(strsplit(input$garch1_alpha,","))),
                                  beta=as.numeric(unlist(strsplit(input$garch1_beta,","))),
                                  n=input$set_n),
        "GARCH2" = simulate_garch(w=gwn()[["w2"]],
                                  omega=input$garch2_omega,
                                  alpha=as.numeric(unlist(strsplit(input$garch2_alpha,","))),
                                  beta=as.numeric(unlist(strsplit(input$garch2_beta,","))),
                                  n=input$set_n),
        "GARCH3" = simulate_garch(w=gwn()[["w3"]],
                                  omega=input$garch3_omega,
                                  alpha=as.numeric(unlist(strsplit(input$garch3_alpha,","))),
                                  beta=as.numeric(unlist(strsplit(input$garch3_beta,","))),
                                  n=input$set_n)
      )
    })
      
    output$garch_Plot <- renderPlot({
      if(input$set_w){w_plot <- gwn()[["w1"]]}else{w_plot <- gwn()[["w0"]]}
      plot(w_plot,
           ylab="x bzw. w",
           xlab="t",
           ylim=input$set_ylim,
           main="Darstellung der Stochastischen Prozesse")
      
      if(any(methods_selected())){
        met.sel <- which(methods_selected())
        for(met in met.sel){
          lines(methods_results()[[met]]$x,col=met+1,type=ifelse(input$set_type,"l","p"))
          lines(methods_results()[[met]]$sigma,col=met+1,lty=2)
          
        }
        legend("topleft",legend=c("Weisses Rauschen",
                                  paste(names(methods_selected())[met.sel]," - X"),
                                  paste(names(methods_selected())[met.sel]," - Sigma")),
               col=c(1,met.sel+1,met.sel+1),
               lty=c(rep(1,1+length(met.sel)),rep(2,length(met.sel))),
               ncol = length(met.sel))
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
