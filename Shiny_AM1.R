library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(shiny)
#install.packages("leaflet")
library(leaflet)

#Read the data
data <- read_csv('Vehicle_Accident_Data.csv')

#Extract dates from the Crash Date/Time field
data$Date <- as.Date(data$`Crash Date/Time`, format = "%m/%d/%Y" )

#Extract time of accident from Crash Date/Time field
data$Time <-  gsub(".* ","", data$`Crash Date/Time`)

#Combine Street features to get the Street Name 
data$StreetMerged <- paste(data$`Street Prefix`, data$`Street Name`, data$`Street Suffix`, sep = " ")
data$Street <- gsub(pattern = "NA ", replacement = "",x = data$StreetMerged)
data$Street <- gsub(pattern = " NA", replacement = "",x = data$Street)

#Combine Intersecting Street features to get the Intersecting Street Name
data$'Intersecting Street' <- paste(data$`Intersecting Street Prefix`, data$`Intersecting Street Name`, data$`Intersecting Street Suffix`, sep = " ")
data$'Intersecting Street' <- gsub(pattern = "NA ", replacement = "",x = data$'Intersecting Street')
data$'Intersecting Street' <- gsub(pattern = " NA", replacement = "",x = data$'Intersecting Street')

#Extract latitude & longitude
datf <- separate(data, Location, into = c("lat", "long"), sep = ", ")
data$lat <- gsub("\\(", "", datf$lat)
data$long <- gsub("\\)", "", datf$long)

#Convert latitude & longitude from character to float
data$lat <- as.numeric(data$lat)
data$long <- as.numeric(data$long)

#Remove Incosistencies in Report Number
data$`Report Number` <- gsub("1800","18-", data$`Report Number`)
data$`Report Number` <- gsub("-009", "-9", data$`Report Number`)
data$`Report Number` <- gsub("-09", "-9", data$`Report Number`)
data$`Report Number` <- gsub("-HR", "", data$`Report Number`)

#drop unnecessary columns as new columns have been created
data$`Street Prefix` <- NULL
data$`Street Name` <- NULL
data$`Street Suffix` <- NULL
data$Location <- NULL
data$`Crash Date/Time`<- NULL
data$StreetMerged <- NULL
data$`Intersecting Street Prefix` <- NULL
data$`Intersecting Street Name` <- NULL
data$`Intersecting Street Suffix` <- NULL

#Remove records with 0 latitude & longitude as they are wrong geographical co-ordinates for the accident locations
#Also remove a record that has got co-ordinates in Mexico with Latitude 25.9192 and longitude -100.0076
newdata <- subset(x = data, data$long != 0.00000 & data$long != -100.0076 )

df <- as.data.frame(newdata)


#UI code for Shiny App
ui <- fluidPage(
  titlePanel("Vehicular Accident Analysis"),
  sidebarLayout(
    sidebarPanel(
      
      p("Select values for the three filters listed below:"),
      
      br(),
      
      #Select analysis variable for the map
      selectInput(inputId ="x", label = "Is it a Hit and Run?", 
                  choices = c("TRUE","FALSE"),
                  selected = "FALSE"),
      
      selectInput(inputId ="y", label = "Is there a Fatality?", 
                  choices = c("TRUE","FALSE"),
                  selected = "FALSE"),
      
      dateInput(inputId = "d", label = "Select a date", 
                min = "2018-10-03", max = "2018-11-01", startview = 'month'
                , value = "2018-10-03"),
      
      br(),
      
      p("1. A map and a data table will be generated based on the selections made."),
      
      p("2. Markers on the map indicate locations where accidents have been recorded."),
      
      p(em(strong("Hover")), " over the marker and you can see the report number of the accident."),
      
      p(em(strong("Click")), " on the marker and you can see corresponding details like Time of Accident, Fatality and Hit and Run"),
      
      p("You can also use the", em(strong("Search")), " bar in the Data Table to look for any record in the output generated.")
      
    ),
    
    #Output should appear in main panel
    mainPanel(
      h3("MAP", align = "center"),
      leafletOutput(outputId = "map"),
      h3("DATA TABLE", align = "center"),
      dataTableOutput(outputId = "table"))
  )
)

#Server code for Shiny App
server <- function(input,output){
  
  output$table <- renderDataTable({
    as.data.frame(subset(x = df, df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d,
                         select = c('Report Number','Street','Intersecting Street')))})
  
  output$map <- renderLeaflet({
    leaflet(data = as.data.frame(subset(x = df,df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d)))%>%
      addTiles() %>% 
      addMarkers(data = as.data.frame(subset(x = df,df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d)), ~long, ~lat,
                 popup = ~paste("<b>Report # </b>", subset(x = df,df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d)$'Report Number',
                                "<br/>", "<b> Time - </b>", subset(x = df,df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d)$'Time',
                                "<br/>", "<b> Fatality - </b>", subset(x = df,df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d)$'Fatality',
                                "<br/>", "<b> Hit and Run - </b>", subset(x = df,df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d)$'Hit And Run'),
                 label = paste("Report # ", subset(x = df,df$`Hit And Run` == input$x & df$Fatality == input$y & df$Date == input$d)$'Report Number'))
  })
  
}

#Call the App
shinyApp(ui = ui, server = server)