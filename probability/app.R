# In dgamma, s = scale = beta, a = shape = alpha

library(shiny)
library(ggplot2)


ui <- fluidPage(
  # Add mathjax for latex support
  withMathJax(),
  
  # Title
  titlePanel("Probability Distributions"),
  
  
  # Contains dropdown menus
  sidebarLayout(
    
    # The drop down menu for probability distributions
    sidebarPanel(
      selectInput("distribution",
                  "Distribution: ",
                  choices = c("Gamma", "Beta", "Normal")),
      
      conditionalPanel(
        condition = "input.distribution == 'Gamma'",
        
        uiOutput("gMeanText"),
        
        uiOutput("gVarText"),
        
        sliderInput("gAlpha", "Alpha: ", 
                    min = 0, max = 10, step = 0.2,
                    value = 1),
        
        sliderInput("gBeta", "Beta: ",
                    min = 0, max = 10, step = 0.2,
                    value = 1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'Beta'",
        
        uiOutput("bMeanText"),
        
        uiOutput("bVarText"),
        
        sliderInput("bAlpha", "Alpha: ", 
                    min = 0, max = 10, step = 0.2,
                    value = 1),
        
        sliderInput("bBeta", "Beta: ",
                    min = 0, max = 10, step = 0.2,
                    value = 1)
        
      ),
      conditionalPanel(
        condition = "input.distribution == 'Normal'",
        
        
        uiOutput("nMeanText"),
        
        uiOutput("nVarText"),
        
        
        sliderInput("mu", "Mu: ", 
                    min = -10, max = 10, step = 0.2,
                    value = 1),
        
        sliderInput("sigmasquared", "Sigma Squared: ",
                    min = 0, max = 10, step = 0.2,
                    value = 1)
        
      )
      
    ),
    
    mainPanel(
      plotOutput("graph")
    ),
    
    
    
  ),
  
)

server <- function(input, output) {
  
  # Fill in the plot space
  
  
  output$graph <- renderPlot({
    
    # Gamma distribution ===============================================
    if(input$distribution == "Gamma"){
      
      # Get values for mean and var
      mu = reactive(input$gAlpha * input$gBeta)
      sigmasquared = reactive(input$gAlpha * input$gBeta ^ 2)
      
      # Output formulas and mean/var
      output$gMeanText <- renderUI({
        withMathJax(paste("$$E(X) = \\alpha\\beta = ", toString(mu()), "$$"))
      })
      
      output$gVarText <- renderUI({
        withMathJax(paste("$$V(X) = \\alpha\\beta^2 = ", toString(sigmasquared()), "$$"))
      })
      
      
      # Get values and plot
      g <- dgamma(0:100, shape = input$gAlpha, scale = input$gBeta)
      df <- data.frame(x = 0:100, PDF = g)
      ggplot(df, aes(x = x, y = PDF)) + geom_line()
      
    }
    
    # Beta distribution ===============================================
    else if(input$distribution == "Beta"){
      mu = reactive(input$bAlpha / (input$bBeta + input$bAlpha))
      sigmasquared = reactive({input$bAlpha * input$bBeta / (input$bBeta + input$bAlpha)^2 / (input$bAlpha + input$bBeta + 1) })
      
      output$bMeanText <- renderUI({
        withMathJax(paste("$$E(X) = \\frac{\\alpha}{\\alpha + \\beta} = ", toString(round(mu(),3)), "$$"))
      })
      
      output$bVarText <- renderUI({
        withMathJax(paste("$$V(X) = \\frac{\\alpha\\beta}{(\\alpha + \\beta)^2 (\\alpha + \\beta + 1)} = ", toString(round(sigmasquared(),3)), "$$"))
      })
      
      
      
      
      ind <- seq(from = 0, to = 1, length.out = 100)
      g <- dbeta(ind, shape1 = input$bAlpha, shape2 = input$bBeta)
      df <- data.frame(x = ind, PDF = g)
      
      ggplot(df, aes(x = x, y = PDF)) + geom_line()
      
    }
    else if(input$distribution == "Normal"){
      
      
      # Output formulas and mean/var
      output$nMeanText <- renderUI({
        withMathJax(paste("$$E(X) = \\mu = ", toString(input$mu), "$$"))
      })
      
      output$nVarText <- renderUI({
        withMathJax(paste("$$V(X) = \\sigma^2 = ", toString(input$sigmasquared), "$$"))
      })
      
      
      ind <- seq(from = -25, to = 25, length.out = 100)
      g <- dnorm(ind, mean = input$mu, sd = sqrt(input$sigmasquared))
      df <- data.frame(x = ind, PDF = g)
      
      ggplot(df, aes(x = x, y = PDF)) + geom_line()
    }
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)