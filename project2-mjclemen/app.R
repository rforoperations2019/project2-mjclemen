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
library(rgdal)
library(httr)
library(jsonlite)
library(leaflet.extras)

council <- readOGR('https://services1.arcgis.com/YZCmUqbcsUpOKfj7/ArcGIS/rest/services/Council_Districts/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=')

get.water.features <- GET("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%0AFROM%20%22513290a6-2bac-4e41-8029-354cbda6a7b7%22")
water.features <- fromJSON(content(get.water.features, "text"))$result$records

# Clean the data --------------------------------------------------------------------------
# Convert all column titles to title case, remove "_", and fill in blank cells ------------
names(water.features) <- str_to_title(names(water.features))
names(water.features) <- gsub(x = names(water.features), pattern = "_", replacement = " ")
water.features$Make[is.na(water.features$Make)] <- "Unknown"
water.features$`Control type`[is.na(water.features$`Control type`)] <- "N/A"
water.features$Inactive[is.na(water.features$Inactive)] <- "True"
water.features$Make <- as.factor(water.features$Make)
water.features$`Control type` <- as.factor(water.features$`Control type`)
water.features$Ward <- as.factor(water.features$Ward)
water.features$Inactive <- as.factor(water.features$Inactive)
levels(water.features$Inactive)[levels(water.features$Inactive) == "FALSE"] <- "False"

icons <- awesomeIconList(
  Decorative = makeAwesomeIcon(icon = "water", library = "glyphicon", markerColor = "white", iconColor = "blue"),
  `Drinking Fountain` = makeAwesomeIcon(icon = "local_drink", library = "glyphicon"),
  Spray = makeAwesomeIcon(icon = "droplet", library = "glyphicon", markerColor = "blue", iconColor = "black")
)

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
              menuItem("Neighborhood", tabName = "neighborhood_count", icon = icon("fas fa-id-card")),
              menuItem("Control Type by Ward", tabName = "controls_by_ward", icon = icon("fas fa-dizzy")),

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
                          selected = c("Spray")),
              
              downloadButton("downloadWaterFeatures", "Download Raw Data of Water Features")
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
              column(12,
                     leafletOutput("water.leaflet")
                     )
            )
    ),
    tabItem(tabName = "neighborhood_count",
            fluidRow(
              column(12,
                     plotlyOutput(outputId = "barplot.neighborhoods")
              )
            )
    ),
    tabItem(tabName = "controls_by_ward",
            fluidRow(
              column(12,
                     plotlyOutput(outputId = "control.types.per.ward")
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
                     Make %in% input$selected.make) %>%
      filter(`Council district` == input$selected.council) %>%
      filter(`Feature type` == input$selected.feature.type)
  })
  
  councilUpdate <- reactive({
    # Build API Query with proper encodes
    newUrl <- paste0("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/ArcGIS/rest/services/Council_Districts/FeatureServer/0/query?where=Council+%3D+%27", input$selected.council, "%27&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=")
    council <- readOGR(newUrl)
  })
  
  # Display a data table that shows all of the journalist deaths from 1992 to 2019
  output$watertable <- renderDataTable({
    datatable(data = waterSubset(), options = list(orderClasses = TRUE, autoWidth = FALSE, scrollX = TRUE,
                                                    pageLength = 5),
              class = 'cell-border stripe', rownames = FALSE)
  })
  
  # Basic Map
  output$water.leaflet <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      setView(-79.978, 40.439, 12)
  })
  
  # Add the user's selected water feature to view on the map. Remove old markers
  observe({
    leafletProxy("water.leaflet", data = waterSubset()) %>%
      clearGroup(group = "featureTypes") %>%
      addAwesomeMarkers(icon = ~icons[`Feature type`], popup = ~paste0("<b>", Id, "</b>: ", `Feature type`), group = "featureTypes")
  })
  
  # Update the polygon layer, showing the selected council district. Remove old polygons
  observe({
    council <- councilUpdate()
    leafletProxy("water.leaflet", data = council) %>%
      clearGroup(group = "councilDistricts") %>%
      addPolygons(popup = ~paste0("<b>", COUNCIL, "</b>"), group = "councilDistricts", color = "green")
      #setView(lat = council$INTPTLAT10[1], lng = council$INTPTLON10[1], zoom = 12)
  })
  
  
  output$barplot.neighborhoods <- renderPlotly({
    ws <- waterSubset()
    req(nrow(ws) > 2)
    # Find the 10 nationalities with the most deaths to plot on barplot --------
    top.neighborhoods <- names(tail(sort(table(ws$Neighborhood)),10))
    ggplot(ws, aes(x = Neighborhood, fill = Inactive)) + geom_bar(color = "black") +
      scale_x_discrete(limits = top.neighborhoods) + scale_fill_brewer(palette = "Accent") +
      labs(x = "Neighborhood of Water Feature", y = "Number of Water Features",
           title = "Number of Water Features per Neighborhood")
  })
  
  # Plot the types of user controls throughout the wards in Pittsburgh -----------
  output$control.types.per.ward <- renderPlotly({
    # Read in the reactive subset ------------------------------------------------
    ws <- waterSubset()
    req(nrow(ws) > 3)
    
    ggplot(ws, aes(x = ws$Ward, y = ws$`Control Type`)) +
      geom_dotplot(binaxis='y',
                   stackdir='center',
                   dotsize = .5,
                   fill="blue") + 
      labs(x = "Ward of Water Features", y = "User Controls on Water Feature",
           title = "Frequency of User Control Types in wards Throughout Pittsburgh")
  })
  
  # Downloadable csv of water features data filtered by make, council district, and feature type.
  # Note -- filename and file type (csv) work in web browser, not RStudio. RStudio glitch from what I have read about it
  output$downloadWaterFeatures <- downloadHandler(
    filename = function() {
      paste("Water Features Throughout Pittsburgh with Your Filters",
            ".csv", sep = "")
    },
    content = function(file) {
      write.csv(waterSubset(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)