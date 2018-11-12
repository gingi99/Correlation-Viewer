library(ggplot2)
library(shiny)
library(shinydashboard)

# -------------------------------------------------------------------
# Define UI 
ui <- dashboardPage(title = "Correlation Viewer",
  dashboardHeader(title = "Correlation Viewer"),
  dashboardSidebar(
    sidebarMenu(
      selectInput('correlation_type', "Correlation Type", choices = c("Pearson"), selected = "Pearson"),
      numericInput('correlation_coef', "Correlation Coef", 0.8, min = 0.01, max = 1.0, step = 0.01),
      numericInput('sample_size', "Sample Size", 1000, min = 100, max = 100000, step = 100),
      actionButton("btn_go", "Go"),

      column(12,hr()),
      numericInput('plot_alpha', "alpha", 0.5, min = 0.1, max = 1.0, step = 0.1),
      numericInput('plot_size', "size", 2, min = 0.1, max = 5.0, step = 0.1)
    )
  ),
  dashboardBody(
    tags$head(includeScript("google-analytics.js")),
    fluidRow(
      box(title = "Scatter Plot", width = 12, solidHeader = T, status = "primary", 
          plotOutput("scatter_plot"),
          downloadButton('downloadPlot','Download Plot')
      )
    )
  )
)

# -------------------------------------------------------------------
# Define server logic
server <- function(input, output) {
  
  print("Initialize Start")
  values <- reactiveValues()
  values$data <- data.frame(stringsAsFactors = F)
  values$plot <- NULL
  
  # --------------------------------
  # data load
  observeEvent(input$btn_go, {
    rho <- input$correlation_coef
    n <- input$sample_size
    x1 <- rnorm(n = n, mean = 0, sd = 1)
    x2 <- rnorm(n = n, mean = 0, sd = 1)
    x3 <- rnorm(n = n, mean = 0, sd = 1)
    y1 <- sqrt(rho) * x1 + sqrt(1 - rho) * x2
    y2 <- sqrt(rho) * x1 + sqrt(1 - rho) * x3
    values$data <- data.frame(x = y1, y = y2, stringsAsFactors = F)
  })
  
  # --------------------------------
  # scatter plot
  output$scatter_plot = renderPlot({
    if(NROW(values$data) > 0){
      values$plot <- ggplot(values$data, aes(x = x, y = y)) + 
        geom_point(alpha = input$plot_alpha, size = input$plot_size) + 
        stat_smooth(method = "lm", se = FALSE, colour = "black", size = 1) + 
        labs(title = paste0("Cor Coef:", round(cor(values$data$x, values$data$y), 5))) + theme_bw()
      values$plot
    }
  })
  
  # --------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste("plot",'.png',sep='')
    },
    content = function(file){
      ggsave(file, plot = values$plot)
    }
  )
}

# Run the application 
shinyApp(ui, server)
