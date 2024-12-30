library(shiny)
library(DT)
library(rvest)

ui <- fluidPage(
  titlePanel("Web Scraping App: Scrape Any URL"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("url", "Enter Website URL:", value = "https://en.wikipedia.org/wiki/R_(programming_language)"),
      textInput("selector", "Enter CSS Selector for scraping:", value = "h1, h2, p"),
      actionButton("scrape", "Scrape Data"),
      br(),
      p("Enter a URL and a CSS selector, then click 'Scrape Data' to fetch data.")
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store scraped data
  scraped_data <- reactiveVal(data.frame(Content = character()))
  
  observeEvent(input$scrape, {
    req(input$url)  # Ensure URL input is not empty
    req(input$selector)  # Ensure CSS selector input is not empty
    
    # Validate the URL (must start with http:// or https://)
    if (!grepl("^https?://", input$url)) {
      showNotification("Please enter a valid URL with http:// or https://", type = "error")
      return()
    }
    
    # Scraping logic
    tryCatch({
      webpage <- read_html(input$url)
      
      # Scrape the content based on the user-defined CSS selector
      content <- webpage %>% 
        html_nodes(input$selector) %>% 
        html_text(trim = TRUE)
      
      # If content is empty, show a message and return
      if (length(content) == 0) {
        showNotification("No content found for the given selector.", type = "error")
        return()
      }
      
      # Update the reactive data
      scraped_data(data.frame(Content = content, stringsAsFactors = FALSE))
      
    }, error = function(e) {
      showNotification("Error scraping the website. Check the URL, site structure, or CSS selector.", type = "error")
    })
  })
  
  # Render data table reactively
  output$table <- renderDT({
    req(scraped_data())  # Ensure data is available
    datatable(scraped_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)
