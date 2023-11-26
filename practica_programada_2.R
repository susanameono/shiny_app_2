library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)
library(shinyjs)


ui <- dashboardPage(
  dashboardHeader(title = "LIBERTADES MUNDIALES",
                  titleWidth = 300),
  
  dashboardSidebar(width = 300,
                   
                   sidebarMenu(
                     selectInput("pais", "Seleccione un País:", choices = NULL),
                     sliderInput("anyo", "Seleccione un año:", min = min(datos_libertad$anio), max = max(datos_libertad$anio), value = c(2008, 2016)),
                     radioButtons("visualizacion", "Elija Visualización", choices = c("Puntaje", "Ranking")),
                     downloadButton("descargarDatos", "Descargar Datos", class = "btn-white btn-dark"),  # Cambiado a btn-white y btn-dark
                     useShinyjs()
                   )
  ),
  
  dashboardBody(
    
    tabsetPanel(  
      
      tabPanel(tabName = "humana", 
               h4("Libertad Humana"),
               column(12, plotlyOutput("linea1"), textOutput("pais")),
      ),
      
      tabPanel(tabName = "personal", 
               h4("Libertad Personal"),
               column(12, plotlyOutput("linea2"))
      ),
      
      tabPanel(tabName = "economica", 
               h4("Libertad Económica"),
               column(12, plotlyOutput("linea3"))
      )
    )
  )
)
server <- function(input, output, session) {
  
  datos_libertad <- read_csv("datos/datos_libertad.csv")
  
  observe({ 
    updateSelectInput(session, "pais", choices = unique(datos_libertad$pais))
  })
  
  pais_anio <- reactive({
    datos_libertad %>%
      filter(pais %in% input$pais, anio >= input$anyo[1], anio <= input$anyo[2])
  })
  
  output$linea1 <- renderPlotly({
    aes_string <- if (input$visualizacion == "Puntaje") {
      "libertad_humana_puntaje"
    } else {
      "libertad_humana_ranking"
    }
    
    ggplot(pais_anio(), aes(x = anio, y = !!as.name(aes_string))) +
      geom_line() +
      scale_x_continuous(breaks = seq(input$anyo[1], input$anyo[2], by = 1)) +
      labs(title = paste("Evolución de la libertad humana de", input$pais),
           x = "Año",
           y = input$visualizacion) +
      theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
  })
  
  output$linea2 <- renderPlotly({
    aes_string <- if (input$visualizacion == "Puntaje") {
      "libertad_personal_puntaje"
    } else {
      "libertad_personal_ranking"
    }
    
    ggplot(pais_anio(), aes(x = anio, y = !!as.name(aes_string))) +
      geom_line() +
      scale_x_continuous(breaks = seq(input$anyo[1], input$anyo[2], by = 1)) +
      labs(title = paste("Evolución de la libertad personal de", input$pais),
           x = "Año",
           y = input$visualizacion) +
      theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
  })
  
  output$linea3 <- renderPlotly({
    aes_string <- if (input$visualizacion == "Puntaje") {
      "libertad_economica_puntaje"
    } else {
      "libertad_economica_ranking"
    }
    
    ggplot(pais_anio(), aes(x = anio, y = !!as.name(aes_string))) +
      geom_line() +
      scale_x_continuous(breaks = seq(input$anyo[1], input$anyo[2], by = 1)) +
      labs(title = paste("Evolución de la libertad económica de", input$pais),
           x = "Año",
           y = input$visualizacion) +
      theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
  })
  
  observe({
    if (nrow(pais_anio()) == 0) {
      shinyjs::disable("descargarDatos")
    } else {
      shinyjs::enable("descargarDatos")
    }
  })
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos_libertad_", input$pais, "_", input$anyo[1], "-", input$anyo[2], ".csv", sep = "")
    },
    content = function(file) {
      write_csv(pais_anio(), file)
    }
  )
}
shinyApp(ui = ui, server = server)