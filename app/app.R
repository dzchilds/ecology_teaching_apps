library(shiny)
library(popbio)
library(plotly)

# Define UI for the app
# Define UI for the app
ui <- fluidPage(
  titlePanel("Population Projection"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      sliderInput("s1", "Survival Probability (Juveniles):", value = 0.2, min = 0, max = 1, step = 0.05),
      sliderInput("s2", "Survival Probability (Adults):", value = 0.8, min = 0, max = 1, step = 0.05),
      sliderInput("f2", "Fecundity Rate (Adults):", value = 1.5, min = 0, max = 5, step = 0.1),
      sliderInput("years", "Projection Length:", value = 12, min = 1, max = 50),
      sliderInput("prop_j", "Initial Proportion of Juveniles", value = 0.9, min = 0, max = 1, step = 0.05),
      actionButton("reset", "Reset to Defaults"),
      br(), br(), hr(), br(),
      strong("Population Growth Rate (λ):"),
      textOutput("lambdaSummary")
    ),
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("totalPopPlot")),
        column(6, plotlyOutput("logTotalPopPlot"))
      ),
      fluidRow(
        column(6, plotlyOutput("popPlot")),
        column(6, plotlyOutput("stageStructurePlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {  
  
  observeEvent(input$reset, {
    updateNumericInput(session, "s1", value = 0.2)
    updateNumericInput(session, "s2", value = 0.8)
    updateNumericInput(session, "f2", value = 1.5)
    updateNumericInput(session, "prop_j", value = 0.9)
    updateNumericInput(session, "years", value = 12)
  })
  
  population_projection <- reactive({
    A <- matrix(c(0, input$f2, input$s1, input$s2), nrow = 2, byrow = TRUE)
    total_pop <- 100  
    initial_pop <- c(input$prop_j * total_pop, (1 - input$prop_j) * total_pop)
    projection <- pop.projection(A, initial_pop, iterations = input$years)
    data.frame(Year = seq.int(0, input$years - 1),  
               Juveniles = projection$stage.vectors[1, 1:input$years],
               Adults = projection$stage.vectors[2, 1:input$years],
               Total = projection$pop.sizes[1:input$years])
  })
  
  output$lambdaSummary <- renderText({
    m <- matrix(c(0, input$f2, input$s1, input$s2), nrow = 2, byrow = TRUE)
    lambda <- round(popbio::lambda(m), 2)
    status <- if (lambda < 1) {
      "The population is declining 📉"
    } else if (lambda == 1) {
      "The population is stable ⚖️"
    } else {
      "The population is growing 🚀"
    }
    paste("λ =", lambda, ":::", status)
  })
  
  output$totalPopPlot <- renderPlotly({
    data <- population_projection()
    plot_ly(data, x = ~Year, y = ~Total, type = "scatter", mode = "lines+markers",
            marker = list(size = 8), name = "Total Population") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Total Population Size")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$logTotalPopPlot <- renderPlotly({
    data <- population_projection()
    plot_ly(data, x = ~Year, y = ~Total, type = "scatter", mode = "lines+markers",
            marker = list(size = 8), name = "Total Population") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(type = "log", title = "Total Population Size (Log Scale)")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$stageStructurePlot <- renderPlotly({
    data <- population_projection()
    plot_ly(data, x = ~Year) %>%
      add_trace(y = ~ (data$Juveniles / data$Total) * 100, type = "scatter", mode = "lines+markers",
                name = "Juveniles", line = list(color = "blue")) %>%
      add_trace(y = ~ (data$Adults / data$Total) * 100, type = "scatter", mode = "lines+markers",
                name = "Adults", line = list(color = "red")) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Population Structure (%)")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$popPlot <- renderPlotly({
    data <- population_projection()
    plot_ly(data, x = ~Year) %>%
      add_trace(y = ~Juveniles, type = "scatter", mode = "lines+markers",
                name = "Juveniles", line = list(color = "blue")) %>%
      add_trace(y = ~Adults, type = "scatter", mode = "lines+markers",
                name = "Adults", line = list(color = "red")) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Population Structure (Size)")) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)
