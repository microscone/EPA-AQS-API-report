#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load R packages
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(gmodels)
library(knitr)
library(kableExtra)
library(leaflet)
library(data.table)
library(htmlwidgets)
library(htmltools)
library(readxl)
library(geojsonsf)
library(sp)
library(sf)
library(RCurl)
library(leafpop)
library(crosstalk)
library(leafsync)
library(shiny)
library(shinythemes)
library(DT)

WD <- getwd()

#load the previously prepared data frame
load('US_2020DV.RData')

#duplicate data frame is made for 2nd map
US_2020DV_2 <- US_2020DV

#list of names to use as filter values
ozoneListNames <- c('60 or below','61-65','66-70','71-75','76-80','Above 80')

#date of yesterday for inclusion in map title
yesterday <- substr(Sys.Date()-1,6,10)   

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                #Primary nav bar title
                navbarPage(
                    HTML("<center>Analysis of 2020 Ambient Monitor Data</center>"),
                    
                    id = "tab_being_displayed", # will set input$tab_being_displayed
                    
                    # First tab
                    tabPanel("Design Value",
                             
                             # Main Panel to include the map
                             mainPanel(
                                 #Primary header
                                 h3(HTML("<center>2020 Preliminary Design Values (ppb)</center>")),
                                 
                                 #secondary header
                                 h5(HTML(paste0("<center>(05-01 through ",yesterday,")</center>"))),
                                 
                                 #leaflet map is inserted in main panel
                                 leafletOutput('mapDV', width = "auto", height = 500),
                                 
                                 #data table is inserted below map, in main panel
                                 dataTableOutput("table1",width = "100%", height = "auto")
                             ), # mainPanel end
                              
                             # Sidebar panel, next to main panel
                             sidebarPanel(
                                    
                                    #empty "paragraph" space
                                    p(""),
                                    
                                    #checkbox filter, populated from list, default is all are checked
                                    checkboxGroupInput("dv",
                                                       "Design Value Filter:",
                                                       ozoneListNames,
                                                       ozoneListNames,
                                                       inline = FALSE)
                              ) # sidebarPanel end
                             
                    ), # Navbar 1 end
                    
                    # Second tab
                    tabPanel("4th Max",
                             
                             mainPanel(
                               h3(HTML("<center>2020 4th Max Values (ppb)</center>")),
                               h5(HTML(paste0("<center>(05-01 through ",yesterday,")</center>"))),
                               leafletOutput('map4th', width = "auto", height = 500),
                               dataTableOutput("table2",width = "100%", height = "auto")
                             ), # mainPanel end
                             
                             sidebarPanel(
                                   p(""),
                                   checkboxGroupInput("max4",
                                                      "4th Max Filter:",
                                                      ozoneListNames,
                                                      ozoneListNames,
                                                      inline = FALSE)
                             ) # sidebarPanel end
                             
                    ), # Navbar 2 end
                    selected = "Design Value"
            ) # navbarPage end
) # fluidPage end



# Define server function  
server <- function(input, output) {
    
    #color pallette is set
    monitorCol <- colorFactor(c('blue','green','yellow','orange','red','purple'), 
                              domain = c('60 or below','61-65','66-70','71-75','76-80','Above 80'))
    
    # LINES RELATED TO THE NAA POLYGON ARE COMMENTED OUT AS THEY CAUSE THE MAPS TO LOAD VERY SLOWLY.
    # HOPEFULLY THE POLYGONS CAN BE RE ADDED VIA JAVASCRIPT CODE, WHICH WILL MAKE A CALL TO THE EPA REST SERVER
    # AT https://gispub.epa.gov/arcgis/rest/services/OAR_OAQPS/NonattainmentAreas/MapServer/2 
    
    # # reading GeoJSON file for NAA boundaries
    # US_2015_O3_NAA <- geojson_sf("US_2015_ozone_NAA_4326.geojson")
    # 
    # #unique list of NAAs and the design value site for each, based upon highes draft 2020 DV
    # NAA_DV_Site <- US_2020DV %>%
    #   select(Monitor_ID,SiteName,NAA_Name,Draft_DV_18_20) %>%
    #   filter(!is.na(NAA_Name)) %>%
    #   group_by(NAA_Name) %>%
    #   arrange(desc(Draft_DV_18_20)) %>%
    #   slice(1)
    # 
    # #design value data of NAA_DV_Site is merged with the spatial data of the NAA polygons, to create a new layer
    # new_NAA_Polygons <- merge(US_2015_O3_NAA,NAA_DV_Site,by.x='area_name',by.y='NAA_Name')
    # 
    # 
    # #creates a orange color palette for the various NAA classifications
    # naa_pal <- colorFactor(
    #   palette = "Oranges",
    #   domain = US_2015_O3_NAA$classification)
    # 
    # # #a "SharedData" object is created
    # # #This creates a common data input for all interactive objects such as UI elements and compatible widgets.
    # designValue_sd<- SharedData$new(US_2020DV)
    # 
    # max4th_SD <- SharedData$new(US_2020DV)
    
    # output object for the DV map, using the renderLeaflet function
    output$mapDV <- renderLeaflet({
      
        leaflet() %>% 
        
        # the zoom and center is set
        setView(-77, 39, zoom = 5) %>%
        
        #a basemap is added
        addProviderTiles(providers$CartoDB.Positron) %>%
        
# 
#         # a polygon layer is added for the US NAA object and its outline color and opacity are set
#         addPolygons(data=new_NAA_Polygons,
#                     stroke = TRUE,
#                     weight = 2,
#                     color = "black",
#                     smoothFactor = 0.2,
#                     fillOpacity = .8,
#                     fillColor = ~naa_pal(classification),
#                     popup = as.character(paste0(new_NAA_Polygons$area_name," NAA","<br>",
#                                                 "Classification: ",new_NAA_Polygons$classification,"<br>",
#                                                 "Design Value Site Name: ",new_NAA_Polygons$SiteName,"<br>",
#                                                 "Design Value Site ID: ",new_NAA_Polygons$Monitor_ID,"<br>",
#                                                 "Design Value Site 2020 DV: ",new_NAA_Polygons$Draft_DV_18_20))) %>%
        

        # add legend for the ozone values
        addLegend('bottomleft', pal = monitorCol, values = US_2020DV$O3_2020_4thMax,
                   title = '2020 Draft O3 DV (ppb)',opacity = 1) 
        # %>%
        # 
        # # add legend for the NAA classifications
        # addLegend('bottomright', pal = naa_pal, values = US_2015_O3_NAA$classification,
        #           title = '2015 Ozone NAA Classifications',opacity = 1)
    })
    
    # output object for the 4th max map, using the renderLeaflet function
    output$map4th <- renderLeaflet({
      leaflet() %>%

        # the zoom and center is set
        setView(-77, 39, zoom = 5) %>%

        #a basemap is added
        addProviderTiles(providers$CartoDB.Positron) %>%

        # # a polygon layer is added for the US NAA object and its outline color and opacity are set
        # addPolygons(data=new_NAA_Polygons,
        #             stroke = TRUE,
        #             weight = 2,
        #             color = "black",
        #             smoothFactor = 0.2,
        #             fillOpacity = .8,
        #             fillColor = ~naa_pal(classification),
        #             popup = as.character(paste0(new_NAA_Polygons$area_name," NAA","<br>",
        #                                         "Classification: ",new_NAA_Polygons$classification,"<br>",
        #                                         "Design Value Site Name: ",new_NAA_Polygons$SiteName,"<br>",
        #                                         "Design Value Site ID: ",new_NAA_Polygons$Monitor_ID,"<br>",
        #                                         "Design Value Site 2020 DV: ",new_NAA_Polygons$Draft_DV_18_20))) %>%


        # add legend for the ozone values
        addLegend('bottomleft', pal = monitorCol, values = US_2020DV$O3_2020_4thMax,
                  title = '2020 4th Max (ppb)',opacity = 1) 
        # %>%
        # 
        #   # add legend for the NAA classifications
        #   addLegend('bottomright', pal = naa_pal, values = US_2015_O3_NAA$classification,
        #             title = '2015 Ozone NAA Classifications',opacity = 1)
    })

    # a reactive data filter is applied to the primary data frame, using the checkbox values for the DV map as the filter values
    mydata_filtered1 <- reactive(US_2020DV[US_2020DV$O3_NAAQS_Attainment %in% input$dv, ])
    
    # a 2nd reactive filter for the 4th max map
    mydata_filtered2 <- reactive(US_2020DV_2[US_2020DV_2$O3_2020_4thMax %in% input$max4, ])
    
    # the observe event updates the map when the checkbox filter is updated
    # this one is for the DV map
    observeEvent(input$dv, {
      
      # a popup value is declared outside of the Leaflet addCircleMarkers function
      # this popup is for the monitors and the design value data
      mon_popup1 <- paste0("Site Name: ",mydata_filtered1()$SiteName,"<br>",
                         "AQS ID: ",mydata_filtered1()$Monitor_ID,"<br>",
                         "2020 Draft DV: ",mydata_filtered1()$Draft_DV_18_20," ppb")
      
      # lefletProxy is used to add the markers and update them based upon the checkbox filter, for the DV map
      leafletProxy("mapDV", data = mydata_filtered1()) %>%
        clearMarkers() %>%
        addCircleMarkers(~Longitude,
                         ~Latitude,
                         popup = mon_popup1,
                         label = ~SiteName,
                         labelOptions = labelOptions(textsize = "15px"),
                         color = ~monitorCol(O3_NAAQS_Attainment),
                         radius = 4,
                         stroke = F, fillOpacity = 1)
    })
    
    # the observe event updates the map when the checkbox filter is updated
    # this one is for the 4th max map
    observeEvent(input$max4, {
      
      # this TRIES to solve an issue where the map on the 2nd tab does not initially render the markers
      req(input$tab_being_displayed == "4th Max") # Only display if tab is "4th Max"
      
      # a pop for the monitors and the 4th max data
      mon_popup2 <- paste0("Site Name: ",mydata_filtered2()$SiteName,"<br>",
                           "AQS ID: ",mydata_filtered2()$Monitor_ID,"<br>",
                           "# Days >70ppb: ",mydata_filtered2()$`days_>70ppb`,"<br>",
                           "2020 4th Max: ",mydata_filtered2()$`2020_Max`," ppb","<br>",
                           "2020 4th Max: ",mydata_filtered2()$`2020_2nd_High`," ppb","<br>",
                           "2020 4th Max: ",mydata_filtered2()$`2020_3rd_High`," ppb","<br>",
                           "2020 4th Max: ",mydata_filtered2()$`2020_4th_High`," ppb")
      
      # leafletProxy for the 4th max map
      leafletProxy("map4th", data = mydata_filtered2()) %>%
        clearMarkers() %>%
        addCircleMarkers(~Longitude,
                         ~Latitude,
                         popup = mon_popup2,
                         label = ~SiteName,
                         labelOptions = labelOptions(textsize = "15px"),
                         color = ~monitorCol(O3_2020_4thMax),
                         radius = 4,
                         stroke = F, fillOpacity = 1)
    })
    
    # output object for the DV map to render the data table, using the checkbox values as filter data
    output$table1 <- renderDataTable({
      datatable(mydata_filtered1(),selection="single",rownames = F)
    })
    
    # output object for the 4th max map to render the data table on that tab, using its checkbox values as filter data
        output$table2 <- renderDataTable({
      datatable(mydata_filtered2(),selection="single",rownames = F)
    })
    
   
} # server end


# Create Shiny object
shinyApp(ui = ui, server = server)