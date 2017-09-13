# Fu yu(28326927)
# Zhongce Ji(28551884)

library(shiny)
library(leaflet)
library(ggmap)
library(d3heatmap)
# Change the directory of data in personal computer
Mortality_Data <- read.table("/Users/Tim/Desktop/Spring17/STAT597A/597_Final_Proj/smsa.dat", header = TRUE, na.strings = ".")
Mortality_Data <- na.omit(Mortality_Data)
CityName <- Mortality_Data$City
CityName <- as.character(CityName)
get_lat_long <- function(names){
  lon_lat <- geocode(names)
  result <- list(City = names, lon = lon_lat$lon, lat = lon_lat$lat)
  return(result)
}

GeoCities <- sapply(CityName, get_lat_long)
GeoCities <- t(GeoCities)
GeoCities <- as.data.frame(GeoCities)
for (i in 1:dim(GeoCities)[2])
  GeoCities[,i] <- unlist(GeoCities[,i])
GeoCities <- cbind(GeoCities, Mortality = Mortality_Data$Mortality)
# Change the directory of data in personal computer
write.csv(GeoCities, file = "/Users/Tim/Desktop/Spring17/STAT597A/597_Final_Proj/geocities.csv")

heatMapdata <- Mortality_Data[,c(2:17)]
row.names(heatMapdata) <- CityName


ui <- fluidPage(
  tags$h2("Heat Map for mortality in each city", style = "color:balck"),
  tags$div(class = "hr", style = " border-top: 1px solid #8c8b8b; margin-top:10px; margin-bottom:10px;"),
  leafletOutput("HeatMap1"),
  tags$h2("Table for each variable", style = "color:balck"),
  tags$div(class = "hr", style = " border-top: 1px solid #8c8b8b; margin-top:10px; margin-bottom:10px;"),
  d3heatmapOutput("HeatMap2")
)

server <- function(input, output, session) {
  
  output$HeatMap1 <- renderLeaflet({
    # Change the directory of data in personal computer
    GeoData <- read.csv("/Users/Tim/Desktop/Spring17/STAT597A/597_Final_Proj/geocities.csv")
    leaflet(GeoData) %>% addTiles() %>%
      addCircles(lng = ~lon, lat = ~lat, weight = 1,
                 radius = ~sqrt(Mortality) * 1000, popup = ~City
      )
  })
  
  output$HeatMap2 <- renderD3heatmap({
    d3heatmap(heatMapdata, scale = "column", colors = "Spectral")
  })
}

shinyApp(ui, server)