

ui <- fluidPage(
  
  theme = shinytheme('united'),
  
  # App title ----
  titlePanel("Canoe Sprint Race Profile"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      tags$head(tags$style("#Summaryhead{font-weight: bold; color: red;
                             font-size: 40px;
                         }"
      )
      ),
      tags$head(tags$style("#Timehead{color: black;
                             font-size: 40px;
                         }"
      )
      ),
      tags$head(tags$style("#Splithead{color: black;
                             font-size: 40px;
                         }"
      )
      ),width=2,
      
      # Input: Select a file ----
      fileInput("file1", "Choose files", multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("goButton", "Go!"),
      
      selectInput("distance", "Distance:",
                  choices = c("500" = "Labelled_data_500",
                    "1000" = "Labelled_data_1000")),
      uiOutput("select_Class"),
      uiOutput("select_First_Name"),
      uiOutput("select_Last_Name"),
      uiOutput("select_Competition"),
      uiOutput("select_Phase")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      htmlOutput("Summaryhead"),
      textOutput("Summaryheadgap"),
      htmlOutput("Timehead"),
      dataTableOutput("table3"),
      plotOutput('ggplot'),
      textOutput("Summaryheadgap"),
      htmlOutput("Splithead"),
      dataTableOutput("table3split"),
      plotOutput('ggplotsplit'),
      #dataTableOutput("table2"),
      
      width=10)
    
    
    
    
    # Output: Data file ----
    
    
    
  )
)





# Create Shiny app ----
# shinyApp(ui, server)