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

# FIRST API CALL: Grabbing Pittsburgh's council data to add polygons as a layer on leaflet map
council <- readOGR('https://services1.arcgis.com/YZCmUqbcsUpOKfj7/ArcGIS/rest/services/Council_Districts/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=')

# SECOND API CALL: Grabbing Pittsburgh's water features data to display to users in a map, datable, and two maps
get.water.features <- GET("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%0AFROM%20%22513290a6-2bac-4e41-8029-354cbda6a7b7%22")
water.features <- fromJSON(content(get.water.features, "text"))$result$records

# Clean the data --------------------------------------------------------------------------
# Convert column titles to title case, remove "_", fill in blank cells, remove unnecessary
# columns, and convert some columns to factors to recognize categories --------------------
names(water.features) <- gsub(x = names(water.features), pattern = "_", replacement = " ")
names(water.features) <- str_to_title(names(water.features))
water.features$Make[is.na(water.features$Make)] <- "Unknown"
water.features$`Control Type`[is.na(water.features$`Control Type`)] <- "N/A"
water.features$Inactive[is.na(water.features$Inactive)] <- "Inactive"
water.features$Make <- as.factor(water.features$Make)
water.features$`Control Type` <- as.factor(water.features$`Control Type`)
water.features$Ward <- as.factor(water.features$Ward)
water.features$Inactive <- as.factor(water.features$Inactive)
levels(water.features$Inactive)[levels(water.features$Inactive) == "FALSE"] <- "Active"
colnames(water.features)[colnames(water.features) == "Inactive"] <- "Status"
water.features <- select(water.features, -c(" Full Text", " Id"))

# Make icons to appear as markers on leaflet map. Will show different images based on user's
# selected water feature type
icons <- awesomeIconList(
  Decorative = makeAwesomeIcon(icon = "fire", library = "glyphicon", markerColor = "white", iconColor = "steelblue"),
  `Drinking Fountain` = makeAwesomeIcon(icon = "coffee", library = "fa", markerColor = 'white', iconColor = "steelblue"),
  Spray = makeAwesomeIcon(icon = 'tint', library = 'fa', markerColor = "white", iconColor = "steelblue")
)

# Place application title in header of dashboard ------------------------------------------
app.header <- dashboardHeader(
  title = "Pittsburgh Water Features", titleWidth = 300
)

# Place user inputs and tab options in a sidebar to be displayed in dashboard
app.sidebar <- dashboardSidebar(
  
  # Change sidebar width to match the title width -----------------------------------------
  width = 300,
  
  # Create four tab options to place the datatable, map, and 2 plots
  # Also place user input controls below the tab options ----------------------------------
  sidebarMenu(id = "tabs",
              
              menuItem("Map of Water Features", tabName = "water_map", icon = icon("map-marker", lib = "glyphicon")),
              menuItem("Water Features Info", tabName = "datatable", icon = icon("table", lib = "font-awesome")),
              menuItem("Neighborhood", tabName = "neighborhood_count", icon = icon("home", lib = "glyphicon")),
              menuItem("Control Type by Ward", tabName = "controls_by_ward", icon = icon("gamepad", lib= "font-awesome")),

              # Select the Makes of the water features to view -----------------------------
              checkboxGroupInput(inputId = "selected.make",
                                 label = "Select which Make(s) of Water Features you would like to view:",
                                 choices = sort(unique(water.features$Make)),
                                 selected = c("Regular Fountain", "Murdock")),
              
              # Select what Council District to view ---------------------------------------------------
              selectInput(inputId = "selected.council",
                           label = "Select which Council District you would like to view:",
                           choices = sort(unique(water.features$`Council District`)),
                           selected = "5"),
              
              # Select what Feature Types to view -------------------------------------------
              radioButtons(inputId = "selected.feature.type",
                          label = "Select which Water Feature Type(s) you would like to view:",
                          choices = sort(unique(water.features$`Feature Type`)),
                          selected = c("Drinking Fountain")),
              
              downloadButton("downloadWaterFeatures", "Download Filtered 'Water Features' Data", class = "butt"),
              # Changing color of download button to better show up against background
              tags$head(tags$style(".butt{color: black !important;}"))
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

# Define UI for application that creates a dashboard on water features in Pittsburgh
ui <- dashboardPage(
  header = app.header,
  sidebar = app.sidebar,
  body = app.body,
  skin = "black"
)

# Define server logic required to draw 2 charts, datatable, and map
server <- function(input, output) {
  
  # Create subset of water features to account for user input. Specifically, the make, council
  # district, and features of the water dataset ----------------------------------------------
  waterSubset <- reactive({
    water.features <- subset(water.features,
                     Make %in% input$selected.make) %>%
      filter(`Council District` == input$selected.council) %>%
      filter(`Feature Type` == input$selected.feature.type)
  })
  
  # Perform updated API call, based on user's selected council district
  councilUpdate <- reactive({
    # Build API Query with proper encodes (provided by Insomnia)
    newUrl <- paste0("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/ArcGIS/rest/services/Council_Districts/FeatureServer/0/query?where=Council%20=%20", input$selected.council, "&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=")
    
    # Change projection after doing new API call, based on user's selection of council district
    council <- readOGR(newUrl) 
    council <- council %>%
      spTransform(CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

    return(council)
  })
  
  # Display a data table that shows all of water features in Pittsburgh
  output$watertable <- renderDataTable({
    datatable(data = waterSubset(), options = list(orderClasses = TRUE, autoWidth = FALSE, scrollX = TRUE,
                                                    pageLength = 5),
              class = 'cell-border stripe', rownames = FALSE)
  })
  
  # Basic Map -- chosen basemap and set the view to show a certain part of pittsburgh
  output$water.leaflet <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$Esri.NatGeoWorldMap) %>%
      setView(-79.978, 40.439, 12)
  })
  
  # Add the user's selected water feature to view on the map. Remove old markers
  observe({
    leafletProxy("water.leaflet", data = waterSubset()) %>%
      clearGroup(group = "featureTypes") %>%
      addAwesomeMarkers(icon = ~icons[`Feature Type`],
                        popup = ~paste0("<b>", "Located At", "</b>: ", Name),
                        group = "featureTypes")
  })
  
  # Update the polygon layer, showing the selected council district. Remove old polygons
  observe({
    leafletProxy("water.leaflet", data = councilUpdate()) %>%
      clearGroup(group = "councilDistricts") %>%
      addPolygons(popup = ~paste0("<b>", COUNCIL, "</b>"), group = "councilDistricts", color = "red")
      #setView(lat = council$INTPTLAT10[1], lng = council$INTPTLON10[1], zoom = 12)
  })
  
  # Plot the number of water features in given neighborhoods
  output$barplot.neighborhoods <- renderPlotly({
    ws <- waterSubset()
    # Ensure there is at least one row of data to plot ---------------------------
    req(nrow(ws) > 0)
    # Find the 10 neighborhoods with the most water features to plot on barplot
    top.neighborhoods <- names(tail(sort(table(ws$Neighborhood)),10))
    ggplot(ws, aes(x = Neighborhood, fill = Status)) + geom_bar() +
      scale_x_discrete(limits = top.neighborhoods) + scale_fill_manual("Status:",
                                                                       values = c("Active" = "steelblue", "Inactive" = "red")) + 
      labs(x = "Neighborhood of Water Feature", y = "Number of Water Features",
           title = "Number of Water Features per Neighborhood")
  })
  
  # Plot the types of user controls throughout the wards in Pittsburgh -----------
  output$control.types.per.ward <- renderPlotly({
    # Read in the reactive subset ------------------------------------------------
    ws <- waterSubset()
    # Ensure there is at least one row of data to plot ---------------------------
    req(nrow(ws) > 0)
    ggplot(ws, aes(x = Ward, y = `Control Type`)) +
      geom_point(col = "steelblue", size = 3, position = "jitter", alpha = 0.7) + 
      labs(x = "Ward of Water Features", y = "User Controls on Water Feature",
           title = "Types of Water Feature Controls in Wards Throughout Pittsburgh")
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