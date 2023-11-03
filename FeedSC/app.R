library(shiny)
library(tidyverse)
library(reactable)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)

# Define a function to read the data
feed <- function() {
    data <- list()
    data[["feedipedia"]] <- readxl::read_excel("data/feedipedia_all.xlsx")
    data[["feedtable"]] <- readxl::read_excel("data/feedtable-all.xlsx")
    return(data)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FeedSC: Feed Similarity Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sidebarPanel size
            width = 2,
            selectInput(
                inputId = "db",
                label = "Select the database:",
                choices = c("feedipedia", "feedtable")
            ),
            uiOutput("feed_selected"),
            uiOutput("nutrients"),
            materialSwitch(
              inputId = "narm",
              label = "Include NA values", 
              value = FALSE,
              status = "primary"
            ),
            actionButton("action", label = "Calculate"),
            verbatimTextOutput("summary")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            width = 10,
            tabsetPanel(type = "tabs",
                tabPanel("Comparison", reactableOutput("result") %>% withSpinner(type = 8)),
                tabPanel("Feed Table", reactableOutput("table") %>% withSpinner(type = 8))
            )
        )
    )
)

# Define server logic required to draw a table
server <- function(input, output) {
    # Call the feed() function to get the data
    
  feed_data <- feed() # Load the data once
  
  # Create a reactive expression to select the data based on the database choice
  data <- reactive({
    if (input$db == "feedipedia") {
      feed_data[["feedipedia"]]
    } else {
      feed_data[["feedtable"]]
    }
  })
  
  df <- reactive({
    req(input$action)
    data() %>%
      select(feed) %>%
      bind_cols(data() %>%
                  select(-feed) %>%
                  scale() %>%
                  as_tibble()
      )
  })
  
  output$feed_selected <- renderUI({
        pickerInput(
            inputId = "feed_selected",
            label = "Select the feed:", 
            choices = data()$feed
        )
    })
    
    output$nutrients <- renderUI({
        selectInput(
            inputId = "nutrients",
            label = "Select the nutrient:", 
            choices = names(data())[-1],
            selected = names(data())[2:6],
            multiple = TRUE
        )
    })
    
    # output ----
    output$result <- renderReactable({
        
        feed_compare <- df() %>% select(feed, input$nutrients) %>%
            filter(feed == input$feed_selected) %>% select(-feed)
        
        if(input$narm == TRUE) {
          diff <- df() %>% select(input$nutrients) %>%
            apply(1, function(x) sum((x - feed_compare)^2, na.rm = TRUE))  
        } else {
          diff <- df() %>% select(input$nutrients) %>%
            apply(1, function(x) sum((x - feed_compare)^2, na.rm = FALSE))  
        }
        
        
        result <- data() %>% select(feed, input$nutrients) %>% mutate(distance = round(diff, 3)) %>% 
            arrange(distance) %>% 
            mutate(rank = row_number()) %>% 
            select(rank, feed, distance, everything())
        
        reactable(result,
                  filterable = TRUE, 
                  defaultPageSize = 30,
                  showPageSizeOptions = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  fullWidth = TRUE,
                  bordered = TRUE,
                  defaultColDef = colDef(minWidth = 120),
                  columns = list(feed = colDef(minWidth = 200))
                  )
        
    })
    
    # Render the table
    output$table <- renderReactable({
        data() %>%
            reactable(
                # selection = "multiple",
                filterable = TRUE, 
                defaultPageSize = 30,
                showPageSizeOptions = TRUE,
                striped = TRUE,
                highlight = TRUE,
                fullWidth = TRUE,
                bordered = TRUE,
                defaultColDef = colDef(minWidth = 120),
                columns = list(
                    feed = colDef(minWidth = 200)
                )
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
