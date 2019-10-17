library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(stringr)
library(tools)
library(rlist)
library(scales)
library(data.table)

council <- readOGR('https://services1.arcgis.com/YZCmUqbcsUpOKfj7/ArcGIS/rest/services/Council_Districts/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=')

get.water.features <- GET("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%0AFROM%20%22513290a6-2bac-4e41-8029-354cbda6a7b7%22")
water.features <- fromJSON(content(get.water.features, "text"))$result$records

# Clean the data --------------------------------------------------------------------------
# Convert all column titles to title case, remove "_", and fill in blank cells ------------
names(water.features) <- str_to_title(names(water.features))
names(water.features) <- gsub(x = names(water.features), pattern = "_", replacement = " ")
water.features$Make[water.features$Make == ""] <- "Unknown"
water.features$`Control type`[water.features$`Control type` == ""] <- "N/A"
water.features$Inactive[water.features$Inactive == "f"] <- "F"
water.features$Inactive[water.features$Inactive == ""] <- "T"


# Place application title in header of dashboard ------------------------------------------
app.header <- dashboardHeader(
  title = "Water Features Throughout Pittsburgh", titleWidth = 300
)

# Place user inputs and tab options in a sidebar to be displayed in dashboard
app.sidebar <- dashboardSidebar(
  
  # Change sidebar width to match the title width -----------------------------------------
  width = 300,
  
  # Create four tab options to place the datatable, the 3 valueboxes, and 3 plots
  # Also place user input controls below the tab options ----------------------------------
  sidebarMenu(id = "tabs",
              
              menuItem("Water Features Info", tabName = "datatable", icon = icon("fas fa-table")),
              menuItem("Map of Water Features", tabName = "water_map", icon = icon("fas fa-map-marked-alt")),
              menuItem("Neighborhood", tabName = "plot1", icon = icon("fas fa-id-card")),
              menuItem("Plot 2", tabName = "plot2", icon = icon("fas fa-dizzy")),

              # Select the makes of the water features to view -----------------------------
              checkboxGroupInput(inputId = "selected.make",
                                 label = "Select which Make(s) you would like to view:",
                                 choices = sort(unique(water.features$Make)),
                                 selected = c("Regular Fountain", "Murdock")),
              
              # Select what council district to view ---------------------------------------------------
              selectInput(inputId = "selected.council",
                           label = "Select which Council District you would like to view:",
                           choices = sort(unique(water.features$`Council district`)),
                           selected = "5"),
              
              # Select what feature types to view -------------------------------------------
              radioButtons(inputId = "selected.feature.type",
                          label = "Select which Feature Type(s) you would like to view:",
                          choices = sort(unique(water.features$`Feature type`)),
                          selected = c("Spray"))
  )
)

# Display 4 tabs: 1 containing the datatable, one a map, and the other 2 each containing a plot
app.body <- dashboardBody(
  
  theme = shinytheme("readable"),
  
  tabItems(
    tabItem(tabName = "datatable",
            fluidRow(
              # Show data table filtered based on user input --------------------------------
              box(title = "Selected Water Features Data",
                  dataTableOutput(outputId = "watertable"),
                  width = 12)
            )
    ),
    tabItem(tabName = "water_map",
            fluidRow(
              column(12
                     )
            )
    ),
    tabItem(tabName = "plot1",
            fluidRow(
              column(12
              )
            )
    ),
    tabItem(tabName = "plot2",
            fluidRow(
              column(12
              )
            )
    )
  )
)

# Define UI for application that creates a dashboard on journalist deaths since 1992
ui <- dashboardPage(
  header = app.header,
  sidebar = app.sidebar,
  body = app.body,
  skin = "black"
)

# Define server logic required to draw charts, datatables, and numeric based boxes
server <- function(input, output) {
  
  # Create subset of water features to account for user input. Specifically, make, council
  # district, and features of the water dataset ------------------------------------------
  waterSubset <- reactive({
    water.features <- subset(water.features,
                     Make %in% input$selected.make &
                       `Council district` %in% input$selected.council &
                       `Feature type` %in% input$selected.feature.type
    )
  })
  
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$watertable <- renderDataTable({
    datatable(data = waterSubset(), options = list(orderClasses = TRUE, autoWidth = FALSE, scrollX = TRUE,
                                                    pageLength = 5),
              class = 'cell-border stripe', rownames = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)