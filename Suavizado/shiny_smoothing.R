# Instala e importa los paquetes
if (!require("shiny")) install.packages("shiny")
if (!require("quantmod")) install.packages("quantmod")

library(shiny)
library(quantmod)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Visualizador de Precios de Acciones"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("stock", "Ingrese el símbolo del stock:", value = "GOOGL"),
      dateInput("start_date", "Seleccione la fecha de inicio:", value = today()-months(12)),
      dateInput("end_date", "Seleccione la fecha de fin:", value = today()),
      checkboxGroupInput(inputId = "type", label = "Seleccionar tipo de media movil:", 
                         choices = c("SMA", "EMA"), selected = c()),
      sliderInput("ma_period", "Periodo de la media móvil:", min = 5, max = 200, value = 21),
      actionButton("submit", "Actualizar Gráfico")
    ),
    mainPanel(
      plotlyOutput("stock_plot",height=800)
    )
  )
)

# Define el servidor
server <- function(input, output) {

  # server logic based on user input
  observeEvent(c(input$stock,input$start_date,input$end_date, input$type, input$ma_period, input$submit), {
    req(input$submit)
    stock_data <- getSymbols(input$stock, from = as.Date(input$start_date), to = as.Date(input$end_date), auto.assign = F)
    stock_data <- as.data.frame(stock_data)
    #Cambio los nombres de las columnas
    colnames(stock_data) <- unlist(lapply(strsplit(colnames(stock_data), "\\."), function(x){x[2]}))
    
    # Calcular la media móvil
    if(length(input$type) == 2){
      stock_data$sma <- SMA(Cl(stock_data), n = input$ma_period)
      stock_data$ema <- EMA(Cl(stock_data), n = input$ma_period)
      stock_data$time = rownames(stock_data) %>% as.POSIXct()
      stock_data = na.omit(stock_data)    
      
      output$stock_plot <- renderPlotly({
        print(
          
          ggplotly(ggplot(stock_data) +
                     geom_line(aes(x = time, y = Close, colour = "Precio"), linewidth = 1, linetype = "solid") +
                     geom_line(aes(x = time, y = sma, colour = paste("SMA", input$ma_period)), linewidth = 1, linetype = "dashed") +
                     geom_line(aes(x = time, y = ema, colour = paste("EMA", input$ma_period)), linewidth = 1, linetype = "dashed") +
                     labs(title = paste("Variación del Precio de", "GOOGL"),
                          x = "Fecha",
                          y = "Precio de Cierre") +
                     scale_colour_manual("", 
                                         breaks = c("Precio", paste("SMA", input$ma_period), paste("EMA", input$ma_period)),
                                         values = c("black", "blue", "red")) +
                     theme_minimal())
        )
      })
    } else if(length(input$type) == 1){
      
      if(input$type == "SMA"){
        stock_data$ma <- SMA(Cl(stock_data), n = input$ma_period)
        tcolor = "blue"
      }else{
        stock_data$ma <- EMA(Cl(stock_data), n = input$ma_period)
        tcolor = "red"
      }
      
      stock_data$ma <- SMA(Cl(stock_data), n = input$ma_period)
      stock_data$time = rownames(stock_data) %>% as.POSIXct()
      stock_data = na.omit(stock_data)    
      
      output$stock_plot <- renderPlotly({
        print(
          
          ggplotly(ggplot(stock_data) +
                     geom_line(aes(x = time, y = Close, colour = "Precio"), linewidth = 1, linetype = "solid") +
                     geom_line(aes(x = time, y = ma, colour = paste(input$type, input$ma_period)), linewidth = 1, linetype = "dashed") +
                     labs(title = paste("Variación del Precio de", "GOOGL"),
                          x = "Fecha",
                          y = "Precio de Cierre") +
                     scale_colour_manual("", 
                                         breaks = c("Precio", paste(input$type, input$ma_period)),
                                         values = c("black", tcolor)) +
                     theme_minimal())
        )
      }) 
    } else {
      stock_data$time = rownames(stock_data) %>% as.POSIXct()
      stock_data = na.omit(stock_data)    
      
      output$stock_plot <- renderPlotly({
        print(
          
          ggplotly(ggplot(stock_data) +
                     geom_line(aes(x = time, y = Close, colour = "Precio"), linewidth = 1, linetype = "solid") +
                     labs(title = paste("Variación del Precio de", "GOOGL"),
                          x = "Fecha",
                          y = "Precio de Cierre") +
                     scale_colour_manual("", 
                                         breaks = c("Precio"),
                                         values = c("black")) +
                     theme_minimal())
        )
      }) 
    }
  })
}
# Ejecutar la aplicación Shiny
shinyApp(ui, server)

if(FALSE){
  stock_data <- getSymbols("GOOGL", from = today() - month(12), to = today(), auto.assign = F)
  ma <- EMA(Cl(stock_data), n = 5)
  
  # Crear el gráfico con ggplot2
  ggplot() +
    geom_line(aes(x = index(stock_data), y = Cl(stock_data), colour = "Precio"), size = 1, linetype = "solid") +
    geom_line(aes(x = index(stock_data), y = ma, colour = paste("MA", input$ma_period)), size = 1, linetype = "dashed") +
    labs(title = paste("Variación del Precio de", "GOOGL"),
         x = "Fecha",
         y = "Precio de Cierre") +
    scale_colour_manual("", 
                        breaks = c("Precio", paste("MA", input$ma_period)),
                        values = c("blue", "red")) +
    theme_minimal()
}


