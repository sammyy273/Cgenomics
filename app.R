library(shiny)
library(DBI)
library(RMySQL)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Cgenomics"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("cancer_type", "Enter Cancer Type:", value = "Ovarian Cancer"),
      actionButton("search", "Search"),
      verbatimTextOutput("availableCancerTypes") 
    ),
    
    mainPanel(
      plotOutput("cancerPlot"),
      verbatimTextOutput("queryResults")  
    )
  )
)

server <- function(input, output, session) {
  db <- dbConnect(RMySQL::MySQL(), 
                  dbname = 'cgenomics',  
                  host = 'localhost',
                  port = 3306,
                  user = 'root',
                  password = 'root')
  
  available_cancer_types <- dbGetQuery(db, "SELECT DISTINCT `Cancer Type` FROM cgenomics.cgenomics_csv")
  output$availableCancerTypes <- renderPrint({
    available_cancer_types
  })
  
  observeEvent(input$search, {
    query <- paste0("SELECT * FROM cgenomics.cgenomics_csv WHERE `Cancer Type` = '", input$cancer_type, "'")
    data <- tryCatch({
      dbGetQuery(db, query)
    }, error = function(e) {
      message("Error in query: ", e)
      return(data.frame())
    })
    
    output$queryResults <- renderPrint({
      if (nrow(data) == 0) {
        "No data found for the specified cancer type."
      } else {
        head(data)
      }
    })
    
    output$cancerPlot <- renderPlot({
      if (nrow(data) > 0) {
        ggplot(data, aes(x = `Diagnosis Age`, y = `Overall Survival (Months)`, size = `Mutation Count`, color = `Sex`)) +
          geom_point(alpha = 0.7) +
          facet_wrap(~ `Sample Type`) +
          labs(title = paste("Cancer Type:", input$cancer_type),
               x = "Diagnosis Age",
               y = "Overall Survival (Months)",
               size = "Mutation Count") +
          theme_minimal() +
          theme(legend.position = "bottom")
      }
    })
  })
  
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
}

shinyApp(ui = ui, server = server)