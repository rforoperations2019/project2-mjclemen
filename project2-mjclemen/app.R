library(shiny)

# Define UI for application that will hold an interactive map as the central focus
ui <- fluidPage(
   
   # Application title
   titlePanel("Project2 - Mandel Clemente"),
   
   # Sidebar to place user input controls
   sidebarLayout(
      sidebarPanel(
      ),
      
      # Show a datable, 2 interactive charts/graphs, and an interactive map
      mainPanel(
        
      )
   )
)

# Define server logic required to draw the plots, datatable, and map
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

