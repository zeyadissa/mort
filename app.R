library(sf)
library(geojsonsf)
library(shinyBS)
library(leaflet)
library(scales)
library(lubridate)
library(dplyr)
library(htmltools)
library(shinythemes)
library(shiny)
library(httr)
library(colourpicker)
library(jsonlite)
library(shinyWidgets)
library(fontawesome)
library(leaflet.extras)
library(tidyverse)

# Global Variables --------------------------------------------------------

#These are the geo-boundaries from the ONS files
LSOA_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_2011_Boundaries_Super_Generalised_Clipped_BSC_EW_V4/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
LAD_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2023_UK_BUC_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
LOOKUP_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA11_SICBL22_ICB22_LAD22_EN_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
ICB_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
WARDS_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2022_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

#Possible input keys for the UI
OP_TYPE <- c('Standard','Real','Pirate')
OP_PAL <- c('Continous','Categorical','String','Custom')
OP_COLS <- c('LAD','LSOA','ICB','Wards','Places')

#These are the controls for the zoom: they are needed to set the boundary to UK
THEME <- 'flatly'

#These read the boundaries from the ONS directly
icb <- sf::st_read(ICB_URL) %>%
  rename('ICB'='ICB23CD')
lsoa <- sf::st_read(LSOA_URL) %>%
  rename('LSOA'='LSOA11CD')
lad <- sf::st_read(LAD_URL) %>%
  rename('LAD'='LAD23CD')
wards <- sf::st_read(WARDS_URL) %>%
  rename('Wards'='WD22CD')

lookup <- read.csv('lookup.csv') %>% select(one_of(OP_COLS),ICB22NM)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML(".col-sm-8 {
        border: 2px solid #486178;
        padding-left: 0px; 
        padding-right: 0px;
      }"))
  ),
  #Determines theme
  theme = shinytheme(THEME),
  #Navigation page
  navbarPage(
    "MORT",
    tabPanel('Geography Lookups',
             fluid = T,
             icon=icon('house'),
             sidebarLayout(
               sidebarPanel(
                 titlePanel(h4(strong('GEOGRAPHY LOOKUPS'),icon('house',class='about-icon fa-pull-left'))),
                 em('MORT works by identifying geography codes and plotting them on a map tile. To get this to work, you need to use the right geography boundaries. In this page, you can select the geography mappings you want and download them for use'),
                 hr(),
                 awesomeCheckboxGroup(
                   inputId = "SELECT_COLUMNS",
                   label = "Select Columns", 
                   choices = OP_COLS,
                   selected = OP_COLS,
                   inline = TRUE, 
                   status = "danger",
                 ),
                 multiInput(
                   inputId='FILTER_ICB',
                   label = 'Filter ICB(s)',
                   choices = icb$ICB23NM,
                   selected="NHS Cheshire and Merseyside Integrated Care Board",
                   width = '100%'),
                 hr(),
                 downloadButton("downloadData",
                                "Download",
                                style = "width:100%;")),
               mainPanel(tableOutput('main_table'),style='border: 0px'))),
    tabPanel('Map Maker',
             fluid = T,
             icon = icon('adjust'),
             sidebarLayout(
               sidebarPanel(
                 titlePanel(h4(strong('MAP MAKER'),icon('adjust',class='about-icon fa-pull-left'))),
                 em('This allows you to create simple maps based on ICB boundaries, with the option to show Ward, LAD, LSOA and (later) Place boundaries for each ICB. Simply select the ICBs you want and click the boundary layer button on the map to select what you want viewed. You can also edit the colour in the sidebar panel on the left. The intention is that this can be used to quickly and easily get every possible health boundary permutation that could be needed and be plopped up on a slide or report'),
                 hr(),
                 multiInput(
                   inputId='ICB_SELECT',
                   label = 'Select which ICB(s) you want shown',
                   choices = icb$ICB23NM,
                   selected="",
                   width = '120%'),
                 hr(),
                 shiny::actionButton(
                   inputId='FIT_BOUNDS_PRESET',
                   label = 'Fit to bounds',
                   status = 'primary',
                   width = '100%'),
                 hr(),
                 colourpicker::colourInput(
                   'ICB_COLOUR',
                   'Choose fill colour',
                   '#26a5b8')),
               mainPanel(leafletOutput('main_map_preset',height='85vh',width='100%')))),
    #MAP MAKER TABSET PANEL
    tabPanel('Custom Map',
             fluid = T,
             icon = icon('map'),
             sidebarLayout(
               sidebarPanel(
                 style = "height: 90vh; overflow-y: auto;", 
                 titlePanel(h4(strong('CUSTOM MAP'),icon('map',class='about-icon fa-pull-left'))),
                 em('This is the most versatile of the available functionalities and is designed to allow you to input in your data for it to be plotted. All this tool needs is a simple .csv file with the first column being your geography codes, the second the value associated with it (For eg: ICB X has a Y score of of 10%). The tool will then automatically identify what boundary that is, align it to the ONS geoboundaries, and plot it for you. You are also able to edit the visuals of the outputs as needed as well as input in a markers file with the longitude and lattitudes to have markers plop up on the map.'),
                 hr(),
                 fluidRow(column(3,
                                 numericInput(
                                   inputId='RADIUS',
                                   label = 'Marker Radius',
                                   value = 1,
                                   min = 1,
                                   max = 25,
                                   step = 1,
                                   width = '120%'),
                                 colourpicker::colourInput(
                                   'MARKER_COLOUR',
                                   'Marker Colour',
                                   'black'),
                                 br(),
                                 shinyWidgets::prettySwitch(
                                   inputId='MARKER_TYPE',
                                   label = 'Fixed marker color?',
                                   status = 'primary',
                                   slim = T),
                                 shinyWidgets::prettySwitch(
                                   inputId='SIZE_TYPE',
                                   label = 'Fixed marker size?',
                                   status = 'primary',
                                   slim = T),),
                          column(6,offset=2,
                                 shinyWidgets::pickerInput(
                                   inputId='PALETTE',
                                   label = 'Select Palette Type',
                                   choices = OP_PAL),
                                 shiny::numericInput(
                                   inputId = 'BINS',
                                   label = 'Select number of bins',
                                   value = 5,
                                   min = 0,
                                   max = 10,
                                   step = 1),
                                 colourpicker::colourInput(
                                   'PRIMARY_COLOUR',
                                   'Primary Colour',
                                   '#26a5b8'),
                                 colourpicker::colourInput(
                                   'SECONDARY_COLOUR',
                                   'Secondary Colour',
                                   '#DD0075'))),
                 numericInput(
                   inputId='OPACITY',
                   label = 'Polygon opacity',
                   value = 0.75,
                   min = 0,
                   max = 1,
                   step = 0.1,
                   width = '100%'),
                 shiny::textInput(
                   inputId = 'LEGEND_TITLE',
                   label = 'Legend title',
                   value = 'Insert legend title',
                   width = '100%'),
                 hr(),
                 shiny::fileInput(
                   inputId = 'UPLOAD_DATA',
                   label = 'Upload Data',
                   accept = '.csv'),
                 shiny::fileInput(
                   inputId = 'UPLOAD_MARKERS',
                   label = 'Upload Markers',
                   accept = '.csv'),
                 shiny::actionButton(
                   inputId='FIT_BOUNDS',
                   label = 'Fit to bounds',
                   status = 'primary',
                   width = '100%'),
                 hr(),
                 shiny::actionButton(
                   inputId = 'GENERATE',
                   label = 'Generate Map',
                   width = '100%',
                   icon = icon('play'))),
             mainPanel(leafletOutput('main_map',height='85vh',width='100%')))),
             tabPanel('FAQs',
                      fluid = T,
                      icon = icon('question'),
                      fluidRow(
                        titlePanel(h4(strong('Frequently Asked Questions (FAQs)'))),
                        hr(),
                        h5(strong('What can this application do?')),
                        h5('You can use this to look-up national NHS health and admistrative boundaries, create pre-set maps of those boundaries, and create custom maps using little information. Basically, if you want a map, use this tool. There are future plans to include catchment area finders using a weighted Voronoi method, as well as some other neat stuff. But right now there is not much free development time'),
                        hr(),
                        h5(strong('It wont let me submit a map!')),
                        h5('There are often three problems. First is you may be using an incorrect geography. Check that the geography codes are all supporter (See supported geography codes in the Geography Lookups page). The second is that the data types do not match. So for example, if you are trying to plot names of ICBs and you select a variable type to be Numeric or Categorical, it will naturally throw an error. Finally you could simply be uploading a file in the wrong format. Currently this only accepts .csv files'),
                        hr(),
                        h5(strong('I am having issues uploading markers')),
                        h5('Note that there are certain templates marker date must be inputted. The first column needs to be the marker name or label, second is longitude, and third is latitude. The fourth, optional column, needs to be the value or name associated with the marker'),
                        hr(),
                        h5(strong('I have a problem, who do I contact?')),
                        h5('Feel free to contact me at zeyad.issa@carnallfarrar.com'),
                        hr(),
                        h5(strong('I want a new feature or geography added')),
                        h5('See above! More than happy to sit down and discuss future additions'),
                        style = 'align: centre;padding-left:10px')
                      )))
    
# SERVER ------------------------------------------------------------------

server <- function(input,output,session){
  
  #PRESET MAPS
  output$main_map_preset <-
    renderLeaflet({
      leaflet::leaflet()%>%
        addMapPane('background_map',zIndex=410) %>%
        addMapPane('polygons',zIndex=420) %>%
        addMapPane('lsoa_layer',zIndex=430) %>%
        addMapPane('lad_layer',zIndex=430) %>%
        addMapPane('icb_layer',zIndex=430) %>%
        addMapPane('wards_layer',zIndex=430) %>%
        addMapPane('labels',zIndex=440) %>%
        addMapPane('markers',zIndex=450) %>%
        setView(lng=0,lat=51.4769, zoom = 7) %>%
        addProviderTiles(
          providers$CartoDB.PositronNoLabels,
          group = 'Standard',
          options=c(pathOptions(pane='background_map'))) %>%
        addProviderTiles(
          providers$Esri.WorldImagery,
          group = 'Real',
          options=c(pathOptions(pane='background_map'))) %>%
        addProviderTiles(
          providers$Stadia.StamenWatercolor,
          group = 'Pirate',
          options=c(pathOptions(pane='background_map'))) %>%
        addProviderTiles(
          providers$CartoDB.PositronOnlyLabels,
          group = 'Show labels',
          options= pathOptions(pane='labels')) %>%
        addLayersControl(
          baseGroups = c('Standard','Real','Pirate'),
          overlayGroups =c('Show ICB bounds','Show LSOA bounds','Show LAD bounds','Show labels','Show wards bounds','Show markers'),
          position = 'bottomright',
          options = layersControlOptions(collapsed = T)) %>%
        
        hideGroup(c('Show ICB bounds','Show LSOA bounds','Show LAD bounds','Show wards bounds','Show labels'))
    })
  
  icb_preset <- reactive({
    icb_preset <- icb %>%
      filter(ICB23NM %in% input$ICB_SELECT)
  })
  
  lsoa_preset <- reactive({
    lsoa_preset <- lsoa %>%
      filter(LSOA %in% filter(lookup, ICB22NM %in% input$ICB_SELECT)$LSOA)
  })
  
  lad_preset <- reactive({
    lad_preset <- lad %>%
      filter(LAD %in% filter(lookup, ICB22NM %in% input$ICB_SELECT)$LAD)
  })
  
  ward_preset <- reactive({
    ward_preset <- wards %>%
      filter(Wards %in% filter(lookup, ICB22NM %in% input$ICB_SELECT)$Wards)
  })
  
  observe({
    #Markers
    leafletProxy("main_map_preset") %>%
      clearShapes() %>%
      addPolygons(
        data=icb_preset(),
        fillOpacity = 0.65,
        group = 'final_map',
        opacity = 0.85,
        color = input$ICB_COLOUR,
        weight = 0,
        options = pathOptions(pane = 'polygons')) %>%
      addPolygons(
        data = icb_preset(),
        fillColor = NA,
        fillOpacity = 0,
        weight = 1.5,
        group = 'Show ICB bounds',
        color = 'black',
        options = pathOptions(pane = 'icb_layer')) %>%
    addPolygons(
      data = lad_preset(),
      fillColor = NA,
      fillOpacity = 0,
      weight = 0.75,
      group = 'Show LAD bounds',
      color = 'black',
      options = pathOptions(pane = 'lad_layer')) %>%
    addPolygons(
      data = lsoa_preset(),
      fillColor = NA,
      fillOpacity = 0,
      weight = 0.25,
      group = 'Show LSOA bounds',
      color = 'black',
      options = pathOptions(pane = 'lsoa_layer')) %>%
    addPolygons(
      data = ward_preset(),
      fillColor = NA,
      fillOpacity = 0,
      weight = 0.5,
      group = 'Show wards bounds',
      color = 'black',
      options = pathOptions(pane = 'wards_layer'))
  })

  #CUSTOM MAPS
  #This gets us the raw data needed for the plotting
  raw_data <- reactive({
    req(input$UPLOAD_DATA$datapath)
    raw_data <- read.csv(input$UPLOAD_DATA$datapath) %>%
      dplyr::rename('loc'=1,
                    'value'=2) %>%
      dplyr::distinct() %>%
      dplyr::filter(substring(loc,1,3) %in% c(substring(lsoa$LSOA,1,3),substring(lad$LAD,1,3),substring(wards$Wards,1,3),substring(icb$ICB,1,3)))
    return(raw_data)
  }) %>%
    bindEvent(input$GENERATE)
  
  #Type of geography identified. Currently only support three.
  geo_type <- reactive({
    req(input$UPLOAD_DATA$datapath)
    if(unique(substring(raw_data()[[1]],1,3)) %in% substring(lsoa$LSOA,1,3)){
      geo_type <- 'LSOA'
    } else if(unique(substring(raw_data()[[1]],1,3)) %in% substring(icb$ICB,1,3)){
      geo_type <- 'ICB'
    } else if(unique(substring(raw_data()[[1]],1,3)) %in% substring(lad$LAD,1,3)){
      geo_type <- 'LAD'
    } else if(unique(substring(raw_data()[[1]],1,3)) %in% substring(wards$Wards,1,3)){
      geo_type <- 'Wards'
    } else {
      geo_type <- 'UNSUPPORTED GEOGRAPHY'
    }
  })
  
  #This is the final map - dependant on raw_data()
  final_map <- reactive({
    req(input$UPLOAD_DATA$datapath)
    #LSOA MAPPING 
    if(geo_type() == 'LSOA'){
      #Map
      final_map <- lsoa %>%
        dplyr::filter(LSOA %in% raw_data()$loc) %>%
        dplyr::left_join(.,raw_data(),by=c('LSOA'='loc'))
      return(final_map)
    #ICB MAPPING 
    } else if(geo_type() == 'ICB'){
      #Map
      final_map <- icb %>%
        dplyr::filter(ICB %in% raw_data()$loc) %>%
        dplyr::left_join(.,raw_data(),by=c('ICB'='loc'))
      return(final_map)
    #LAD MAPPING 
    } else if(geo_type() == 'LAD'){
      #Map
      final_map <- lad %>%
        dplyr::filter(LAD %in% raw_data()$loc) %>%
        dplyr::left_join(.,raw_data(),by=c('LAD'='loc'))
      return(final_map)
    } else if (geo_type() == 'Wards'){
      final_map <- wards %>%
        dplyr::filter(Wards %in% raw_data()$loc) %>%
        dplyr::left_join(.,raw_data(),by=c('Wards'='loc'))
    } else {warning('UNSUPPORTED GEO TYPE')}
  })
  
  lookup_fil <- reactive({
    req(input$UPLOAD_DATA$datapath)
    #LSOA MAPPING 
    if(geo_type() == 'LSOA'){
      #Map
      #Lookups
      lookup_fil <- lookup %>%
        dplyr::filter(LSOA %in% raw_data()$loc)
      #ICB MAPPING 
    } else if(geo_type() == 'ICB'){
      #Map
      #Lookups
      lookup_fil <- lookup %>%
        dplyr::filter(ICB %in% raw_data()$loc)
      #LAD MAPPING 
    } else if(geo_type() == 'LAD'){
      #Map
      #Lookups
      lookup_fil <- lookup %>%
        dplyr::filter(LAD %in% raw_data()$loc)
    } else if(geo_type() == 'Wards'){
      #Map
      #Lookups
      lookup_fil <- lookup %>%
        dplyr::filter(Wards %in% raw_data()$loc)
    } else {
      lookup_fil <- lookup
    }
  })
  
  lsoa_fil <- reactive({
    req(input$UPLOAD_DATA$datapath)
    lsoa_fil <- lsoa %>%
      dplyr::filter(LSOA %in% lookup_fil()$LSOA)
    return(lsoa_fil)
    })
  
  icb_fil <- reactive({
    req(input$UPLOAD_DATA$datapath)
    icb_fil <- icb %>%
      dplyr::filter(ICB %in% lookup_fil()$ICB)
    return(icb_fil)
  }) 
  
  lad_fil <- reactive({
    req(input$UPLOAD_DATA$datapath)
    lad_fil <- lad %>%
      dplyr::filter(LAD %in% lookup_fil()$LAD)
    return(lad_fil)
  })
  
  wards_fil <- reactive({
    req(input$UPLOAD_DATA$datapath)
    wards_fil <- wards %>%
      dplyr::filter(Wards %in% lookup_fil()$Wards)
    return(wards_fil)
  })
  
  pal <- reactive({
    
    req(input$UPLOAD_DATA$datapath)
    
    {
      tryCatch({
        return(
          if(input$PALETTE == 'Categorical'){
            pal <- leaflet::colorBin(
              palette = c(input$PRIMARY_COLOUR,'white',input$SECONDARY_COLOUR),
              domain=as.numeric(final_map()$value),
              na.color=NA,
              bins=input$BINS)
            return(pal)
          } else if(input$PALETTE == 'Continous') {
            pal <- leaflet::colorNumeric(
              palette = c(input$PRIMARY_COLOUR,'white',input$SECONDARY_COLOUR),
              domain=as.numeric(final_map()$value),
              na.color=NA)
            return(pal)
          } else if(input$PALETTE == 'Custom') {
            pal <- leaflet::colorFactor(
              #TEST THIS OUT
              palette = as.character(final_map()$value),
              domain = as.character(final_map()$value),
              ordered = T)
            return(pal)
          } else if(input$PALETTE == 'String'){
            pal <- leaflet::colorFactor(
              palette = viridis_pal()(20),
              domain = as.character(final_map()$value)
            )}
          )},
      error = function(e) {
        message('There is something wrong with the markers file you uploaded.', e)
        sendSweetAlert(
          session = session,
          title = 'Error',
          text="There is something wrong with the data file you uploaded. MORT cannot coerce types (eg: 'NAME' is a string and cannot be turned into numeric) please adjust the file or the palette type accordingly",
          type = 'error')
        return(NULL)
      })
    }
    }) %>%
    bindEvent(input$GENERATE)
  
  #CREATE MARKERS
  marker_data <- reactive({
    req(input$UPLOAD_MARKERS$datapath)
    
    marker_data <- read.csv(input$UPLOAD_MARKERS$datapath) %>%
      dplyr::rename('name'=1,
                    'lng'=2,
                    'lat'=3) %>%
      dplyr::mutate(safety = 1) %>%
      dplyr::distinct() 
    return(marker_data)
  }) %>%
    bindEvent(input$GENERATE)

  #MAIN MAP
  output$main_map <-
    renderLeaflet({
      leaflet::leaflet()%>%
        addMapPane('background_map',zIndex=410) %>%
        addMapPane('polygons',zIndex=420) %>%
        addMapPane('lsoa_layer',zIndex=430) %>%
        addMapPane('lad_layer',zIndex=430) %>%
        addMapPane('icb_layer',zIndex=430) %>%
        addMapPane('wards_layer',zIndex=430) %>%
        addMapPane('labels',zIndex=440) %>%
        addMapPane('markers',zIndex=450) %>%
        setView(lng=0,lat=51.4769, zoom = 7) %>%
        addProviderTiles(
          providers$CartoDB.PositronNoLabels,
          group = 'Standard',
          options=c(pathOptions(pane='background_map'))) %>%
        addProviderTiles(
          providers$Esri.WorldImagery,
          group = 'Real',
          options=c(pathOptions(pane='background_map'))) %>%
        addProviderTiles(
          providers$Stadia.StamenWatercolor,
          group = 'Pirate',
          options=c(pathOptions(pane='background_map'))) %>%
        addProviderTiles(
          providers$CartoDB.PositronOnlyLabels,
          group = 'Show labels',
          options= pathOptions(pane='labels')) %>%
        addLayersControl(
          baseGroups = c('Standard','Real','Pirate'),
          overlayGroups =c('Show ICB bounds','Show LSOA bounds','Show LAD bounds','Show labels','Show ward bounds','Show markers'),
          position = 'bottomright',
          options = layersControlOptions(collapsed = T)) %>%
        hideGroup(c('Show ICB bounds','Show LSOA bounds','Show LAD bounds','Show ward bounds','Show labels'))
      })
  
  observe({
    
    req(input$UPLOAD_MARKERS$datapath)
    
    {
      tryCatch({
        return(
          #Markers
          leafletProxy("main_map") %>%
            clearGroup('Show markers') %>%
            addCircles(
              data=marker_data(),
              lng = ~lng,
              lat = ~lat,
              label=~name,
              opacity = 1,
              radius = input$RADIUS,
              weight = input$RADIUS,
              fillColor = input$MARKER_COLOUR,
              color = input$MARKER_COLOUR,
              group = 'Show markers',
              options=pathOptions(pane='markers'))
          )},
        error = function(e) {
          message('There is something wrong with the markers file you uploaded.', e)
          sendSweetAlert(
            session = session,
            title = 'Error',
            text= "There is something wrong with the markers file you uploaded",
            type = 'error')
          return(NULL)
        })
      }
  }) %>%
    bindEvent(input$GENERATE)

  #DOWNLOADS
  output$downloadData <- downloadHandler(
    filename = 'lookup.csv',
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(download_table(), file)
    }
  )

  # CREATE UNDERLYING LAYER
  observe({

    req(input$UPLOAD_DATA$datapath)

    {
      tryCatch({
        
        return(
          c(
          leafletProxy('main_map') %>%
            clearControls()  %>%
            clearGroup(c('final_map','Show ICB bounds','Show LSOA bounds','Show LAD bounds','Show ward bounds')),
          

          #Observe the map if it changes
          leafletProxy("main_map") %>%
            addPolygons(
              data=final_map(),
              smoothFactor = 0,
              fillOpacity = input$OPACITY,
              fillColor = ~pal()(value),
              stroke = 0,
              group = 'final_map',
              opacity = 0.85,
              color = 'black',
              weight = 1,
              options = pathOptions(pane = 'polygons')),
          
          #Legend
          leafletProxy('main_map') %>%
            addLegend(
              data=final_map(),
              position='topright',
              pal=pal(),
              values=~value,
              opacity = 1,
              title = input$LEGEND_TITLE),
          
          leafletProxy('main_map') %>%
            addPolygons(
              data=lsoa_fil(),
              fillOpacity = 0,
              opacity = 1,
              weight =0.25,
              color = 'black',
              options = pathOptions(pane='lsoa_layer'),
              group = 'Show LSOA bounds') %>%
            addPolygons(
              data=lad_fil(),
              fillOpacity = 0,
              opacity = 1,
              weight =0.75,
              color = 'black',
              options = pathOptions(pane='lad_layer'),
              group ='Show LAD bounds') %>%
            addPolygons(
              data=icb_fil(),
              fillOpacity = 0,
              opacity = 1,
              weight =1.5,
              color = 'black',
              options = pathOptions(pane='icb_layer'),
              group = 'Show ICB bounds') %>%
            addPolygons(
              data=wards_fil(),
              fillOpacity = 0,
              opacity = 1,
              weight =0.5,
              color = 'black',
              options = pathOptions(pane='wards_layer'),
              group = 'Show ward bounds'))
          )},
          error = function(e) {
            message('There is something wrong with the markers file you uploaded.', e)
            sendSweetAlert(
              session = session,
              title = 'Error',
              text="There is something wrong with the data file you uploaded. Please make sure the file contains supported geographies and is in an appropraite format",
              type = 'error')
            return(NULL)
          })
      }
  }) %>%
    bindEvent(input$GENERATE)
  
  lookup_table <- reactive({
    lookup[sample(1:nrow(lookup)), ] %>%
      dplyr::select(one_of(input$SELECT_COLUMNS),ICB22NM) %>%
      dplyr::filter(ICB22NM %in% input$FILTER_ICB) %>%
      dplyr::distinct() %>%
      head(20) 
      
  })
  
  download_table <- reactive({
    lookup %>%
      dplyr::select(one_of(input$SELECT_COLUMNS),ICB22NM) %>%
      dplyr::filter(ICB22NM %in% input$FILTER_ICB) %>%
      dplyr::distinct()
  })
  
  output$main_table <-
    renderTable({
      lookup_table()},
      width = '100%'
    )
  
  # CREATE UNDERLYING LAYER
  observe({
    req(input$UPLOAD_DATA$datapath)
    
    {
      tryCatch({
        return(
          #Markers
          #Generates bounds
          c(bounds <-
            final_map() %>%
            st_bbox() %>%
            as.character,
          #Observe the map if it changes
          #Proxy updates the polygons if new thing is added
          leafletProxy("main_map") %>%
            fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]))
        )},
        error = function(e) {
          message('There is something wrong with the data file you uploaded.', e)
          sendSweetAlert(
            session = session,
            title = 'Error',
            text= "There is something wrong with the data file you uploaded",
            type = 'error')
          return(NULL)
        })
    }
  }) %>%
    bindEvent(input$FIT_BOUNDS)
  
  # CREATE UNDERLYING LAYER
  observe({
    {
      tryCatch({
        return(
          #Markers
          #Generates bounds
          c(bounds <-
              icb_preset() %>%
              st_bbox() %>%
              as.character,
            #Observe the map if it changes
            #Proxy updates the polygons if new thing is added
            leafletProxy("main_map_preset") %>%
              fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]))
        )},
        error = function(e) {
          message('There is something wrong with the data file you uploaded.', e)
          sendSweetAlert(
            session = session,
            title = 'Error',
            text= "There is something wrong with the data file you uploaded",
            type = 'error')
          return(NULL)
        })
    }
  }) %>%
    bindEvent(input$FIT_BOUNDS_PRESET)
  
}
  
shinyApp(ui=ui,server=server)

