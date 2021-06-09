library(shiny)
library(fpp2)

ui <- fluidPage(
  
  titlePanel("Time Series Analysis"),
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      radioButtons("datasetType", "Load Dataset", choices = c("Built-In", "Upload")),
      
      
      conditionalPanel(condition = "input.datasetType == 'Built-In'", 
                       selectInput("dataChoice", "Choose a Dataset", choices = c("Bituminous Coal", "Gasoline", "Australian Beer", "Woollen Yarn Production", "Google Stock Prices"))
      ),
      
      
      conditionalPanel(
        condition = "input.datasetType == 'Upload'",
        fileInput("file", "Upload Dataset", multiple = FALSE, buttonLabel = "Open"),
        numericInput("freq", "Enter data frequency",
                     min = 1, value = 1, max = 52, step = 1, width = 120)
        
      ),
      
      
      
      checkboxGroupInput("models", "Select Models", c("ARIMA", "ETS", "TBATS")),
      
      radioButtons("box", "Use Box-Cox Transform", c("None", "Optimal", "Manual")),
      
      conditionalPanel(
        condition = "input.box == 'Manual'",
        
        sliderInput("lambda", "Select Lambda Value", min = -2, max = 5, step = 0.1, value = 0)
      ),
      
      sliderInput("h", "Select Forecast Period", min = 1, max = 30, step = 1, value = 10)
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graph", plotOutput("graph"), selectInput("tableChoice", "Choose Forecast Table", choices = ""),tableOutput("forecastTable")),
        tabPanel("ACF/PACF", plotOutput("acf"), plotOutput("pacf")),
        
        tabPanel("Residuals",
                 selectInput("residModel", "Choose Model", choices =""),
                 plotOutput("resid"),
                 textOutput("boxTest")
                 
        )
      )
      
    )
    
  )
)

server <- function(input, output, session) {
  
  
  # Dataset management =====================================================
  showPI <- reactive(length(input$models) <= 1)
  
  dataset <- reactive({
    if(input$datasetType == "Built-In")
    {
      if(input$dataChoice == "Bituminous Coal")
        dataset <- bicoal
      else if(input$dataChoice == "Gasoline")
        dataset <- gas
      else if(input$dataChoice == "Australian Beer")
        dataset <- ausbeer
      else if(input$dataChoice == "Woollen Yarn Production")
        dataset <- woolyrnq
      else if(input$dataChoice == "Google Stock Prices")
        dataset <- goog200
    }
    else if(input$datasetType == "Upload")
    {
      if(is.null(input$file)){return(ts(0))}
      dataset <- ts(scan(input$file$datapath), frequency = input$freq)
    }
    
    
    
    if(input$box == "None")
      dataset <- dataset
    else if(input$box == "Optimal")
      dataset <- BoxCox(dataset, BoxCox.lambda(dataset))
    else if(input$box == "Manual")
      dataset <- BoxCox(dataset, lambda = input$lambda)
  })
  
  # Models =====================================================
  arimaModel <- reactive(auto.arima(dataset()))
  ETSModel <- reactive(ets(dataset()))
  TBATSModel <- reactive(tbats(dataset()))
  
  
  # Graph tab =====================================================
  output$graph <- renderPlot({
    
    g <- autoplot(dataset())
    
    
    # Set title
    if(input$datasetType == "Built-In")
    {
      if(input$dataChoice == "Bituminous Coal")
        g <- g + ggtitle("Bituminous Coal Production in USA 1920-1968")
      else if(input$dataChoice == "Gasoline")
        g <- g + ggtitle("Montly Gas Production in Australia 1956-1995")
      else if(input$dataChoice == "Australian Beer")
        g <- g + ggtitle("Quarterly Beer Production in Australia (ML)1956:Q1-2010:Q2")
      else if(input$dataChoice == "Woollen Yarn Production")
        g <- g + ggtitle("Quarterly Woollen Yarn Production in Australia (Tonnes) Mar 1956 - Sep 1994")
      else if(input$dataChoice == "Google Stock Prices")
        g <- g + ggtitle("Closing GOOG Stock Prices for 200 Consecutive Trading Days, Starting 25 February 2013")
    }
    
    if("ARIMA" %in% input$models){
      #arimaModel <- auto.arima(dataset())
      
      g <- g + 
        autolayer(fitted(arimaModel()), series = "ARIMA") +
        autolayer(forecast(arimaModel(), h = input$h), series = "ARIMA", PI = showPI())
    }
    if("ETS" %in% input$models)
    {
      #ETSModel <- ets(dataset())
      
      g <- g +
        autolayer(fitted(ETSModel()), series = "ETS") +
        autolayer(forecast(ETSModel(), h = input$h), series = "ETS", PI = showPI())
    }
    
    if("TBATS" %in% input$models)
    {
      #TBATSModel <- tbats(dataset())
      
      g <- g +
        autolayer(fitted(TBATSModel()), series = "TBATS") +
        autolayer(forecast(TBATSModel(), h = input$h), series = "TBATS", PI = showPI())
    }
    
    g
  })
  
  observe(updateSelectInput(session, "tableChoice", choices = input$models))
  
  output$forecastTable <- renderTable({
    # arima only
    if(input$tableChoice == "ARIMA")
      forecast(arimaModel(), h = input$h)
    else if(input$tableChoice == "ETS")
      forecast(ETSModel(), h = input$h)
    else if(input$tableChoice == "TBATS")
      forecast(TBATSModel(), h = input$h)
  })
  
  # ACF/PACF tab =====================================================
  output$acf <- renderPlot({
    ggAcf(dataset())
  })
  output$pacf <- renderPlot({
    ggPacf(dataset())
  })
  
  # Residual tab =====================================================
  observe(updateSelectInput(session, "residModel", choices = input$models))
  
  output$resid <- renderPlot({
    if(input$residModel == "ARIMA")
    {
      checkresiduals(arimaModel())
    }
    else if(input$residModel == "ETS")
    {
      checkresiduals(ETSModel())
    }
    else if(input$residModel == "TBATS")
    {
      checkresiduals(TBATSModel())
    }
  })
  
  output$boxTest <- renderText({
    text <- 0
    
    if(input$residModel == "ARIMA")
    {
      text <- capture.output(Box.test(residuals(arimaModel()), type = "Ljung-Box"))
    }
    else if(input$residModel == "ETS")
    {
      text <- capture.output(Box.test(residuals(ETSModel()), type = "Ljung-Box"))
    }
    else if(input$residModel == "TBATS")
    {
      text <- capture.output(Box.test(residuals(TBATSModel()), type = "Ljung-Box"))
    }
    
    paste("Box-Ljung Test:\n",text[5])
  })
  
  
  
}

shinyApp(ui = ui, server = server)