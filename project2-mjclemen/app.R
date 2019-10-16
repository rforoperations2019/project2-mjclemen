library(shiny)

wards <- readOGR('https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/PGHWards/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=')

get.water.features <- GET("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%0AFROM%20%22513290a6-2bac-4e41-8029-354cbda6a7b7%22")
water.features <- fromJSON(content(get.water.features, "text"))$result$records
cat(water.features$neighborhood)

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

