# Install and load required packages
# install.packages(c("shiny", "quantmod"))

library(shiny)
library(quantmod)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Coca-Cola Stock Prices with Smoothing Spline"),

  titlePanel("Lambda"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", "Lambda:", min = 0, max = 2, value = 1, step = 0.0001),
    ),
    mainPanel(
      plotOutput("stockPlotlambda"),
    )
  ),

  titlePanel("Degrees of freedom"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("df", "Degrees of freedom (df):", min = 2, max = 150, value = 10, step = 1),
    ),
    mainPanel(
      plotOutput("stockPlotdf")
    )
  ),

  titlePanel("Smoothing Parameter"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("spar", "Smoothing Parameter (spar):", min = 0, max = 2, value = 0.5, step = 0.1),
    ),
    mainPanel(
      plotOutput("stockPlotspar")
    )
  ),

)

# Define the server for the Shiny app
server <- function(input, output) {
  # Download stock data using quantmod
  stock_symbol <- "KO"
  getSymbols(stock_symbol, src = "yahoo", from = "2023-01-01", to = Sys.Date())
  stock_data <- KO$KO.Close #Cl(get(stock_symbol))

  # Extract date and closing prices
  date <- index(stock_data)
  price <- as.numeric(stock_data)

  # Reactive function to update the plot based on the selected lambda
  output$stockPlotlambda <- renderPlot({
    lambda <- input$lambda
    smoothed <- smooth.spline(date, price, lambda = lambda)

    # Plot the original data and the fitted smoothing spline
    plot(date, price, main = "Coca-Cola Stock Price & Smoothing Spline", xlab = "Date", ylab = "Close Price", type = "l", col = "blue")
    lines(smoothed, col = "red", lwd = 2)

    # Add legend
    legend("bottomright", legend = c("Original Data", "Smoothing Spline"), col = c("blue", "red"), lty = 1, lwd = 2)
  })
  output$stockPlotdf <- renderPlot({
    df <- input$df
    smoothed <- smooth.spline(date, price, df = df)

    # Plot the original data and the fitted smoothing spline
    plot(date, price, main = "Coca-Cola Stock Price & Smoothing Spline", xlab = "Date", ylab = "Close Price", type = "l", col = "blue")
    lines(smoothed, col = "red", lwd = 2)

    # Add legend
    legend("bottomright", legend = c("Original Data", "Smoothing Spline"), col = c("blue", "red"), lty = 1, lwd = 2)
  })
  output$stockPlotspar <- renderPlot({
    spar <- input$spar
    smoothed <- smooth.spline(date, price, spar = spar)

    # Plot the original data and the fitted smoothing spline
    plot(date, price, main = "Coca-Cola Stock Price & Smoothing Spline", xlab = "Date", ylab = "Close Price", type = "l", col = "blue")
    lines(smoothed, col = "red", lwd = 2)

    # Add legend
    legend("bottomright", legend = c("Original Data", "Smoothing Spline"), col = c("blue", "red"), lty = 1, lwd = 2)
  })

}

# Run the Shiny app
shinyApp(ui, server)
