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
library(leaflet.extras)

# Global Variables --------------------------------------------------------

source('const/glob.R')
source('ui/custom_map_ui.R')
source('ui/faq_ui.R')
source('ui/geography_ui.R')
source('ui/markers_ui.r')

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
             geography_ui()),
    #MAP MAKER TABSET PANEL
    tabPanel('Custom Map',
             fluid = T,
             icon = icon('map'),
             custom_map_ui()),
    tabPanel('FAQs',
             fluid = T,
             icon = icon('question'),
             faq_ui())))

# SERVER ------------------------------------------------------------------

server <- function(input,output,session){
  
  #OUTPUT 1: LOOKUP DOWNLOAD
  output$downloadData <- downloadHandler(
    filename = 'lookup.csv',
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(download_table(), file,row.names=F)
    }
  )
  

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
  
  #OUTPUT 3: CUSTOM MAP0S
  
  #Custom map data
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
    if(all(unique(substring(raw_data()[[1]],1,3)) %in% unique(substring(lsoa$LSOA,1,3)))){
      geo_type <- 'LSOA'
    } else if(all(unique(substring(raw_data()[[1]],1,3)) %in% unique(substring(icb$ICB,1,3)))){
      geo_type <- 'ICB'
    } else if(all(unique(substring(raw_data()[[1]],1,3)) %in% unique(substring(lad$LAD,1,3)))){
      geo_type <- 'LAD'
    } else if(all(unique(substring(raw_data()[[1]],1,3)) %in% unique(substring(wards$Wards,1,3)))){
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
  
  col_pal <- reactive({
    if(input$COL_NUM=='Two colours'){
      c(input$PRIMARY_COLOUR,input$SECONDARY_COLOUR)
    } else {
      c(input$PRIMARY_COLOUR,input$MID_COLOUR,input$SECONDARY_COLOUR)
    }
  })
  
  pal_domain <- reactive({
    if(input$FORCE_MAX == T){
      c(input$DOMAIN_RANGE)
    } else {
      as.numeric(final_map()$value)
    }
  })
  
  final_map <- reactive({
    if(input$PALETTE == 'Quantiles'){
    final_map() |> 
        dplyr::mutate(value = quan)
      } else {
      final_map()
    }
    )})
  
  pal <- reactive({
    
    req(input$UPLOAD_DATA$datapath)
    
    {
      tryCatch({
        return(
          if(input$PALETTE == 'Categorical'){
            pal <- leaflet::colorBin(
              palette = col_pal(),
              domain=pal_domain(),
              na.color=NA,
              bins=input$BINS)
            return(pal)
          } else if(input$PALETTE == 'Continous') {
            pal <- leaflet::colorNumeric(
              palette = col_pal(),
              domain=pal_domain(),
              na.color=NA)
            return(pal)
          } else if(input$PALETTE == 'Quantiles') {
            pal <- leaflet::colorNumeric(
              palette = col_pal(),
              domain=pal_domain(),
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
  
  marker_data <- reactive({
    req(input$UPLOAD_MARKERS$datapath)
    
    marker_data <- read.csv(input$UPLOAD_MARKERS$datapath) %>%
      dplyr::mutate(safety_check = 1) %>%
      dplyr::rename('lng'=1,
                    'lat'=2,
                    'value'=3) %>%
      dplyr::distinct() 
    return(marker_data)
  }) %>%
    bindEvent(input$GENERATE)
  
  #Custom map base outputs
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
  
  #Custom map markers
  observe({
    
    req(input$UPLOAD_MARKERS$datapath)
    
    {
      tryCatch({
        return(
          #Markers
          leafletProxy("main_map") %>%
            clearControls()  %>%
            clearGroup(c('final_map','Show ICB bounds','Show LSOA bounds','Show LAD bounds','Show ward bounds','Show markers')) %>%
            addCircles(
              data=marker_data(),
              lng = ~lng,
              lat = ~lat,
              opacity = 1,
              fillOpacity = 1,
              radius = if(input$MARKER_SIZE_FLAG == T & typeof(marker_data()$value) %in% c('numeric','double','integer')){~rescale(value,c(10,input$RADIUS))}else{input$RADIUS * 10},
              weight = 0,
              fillColor = if(input$MARKER_PALETTE == 'None'){input$MARKER_PRIMARY_COLOUR}else{~marker_pal()(value)},
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
  
  # CREATE UNDERLYING LAYER
  observe({
    
    req(input$UPLOAD_DATA$datapath)
    
    {
      tryCatch({
        
        return(
          c(
            
            leafletProxy('main_map') %>%
              clearControls()  %>%
              clearGroup(c('final_map','Show markers','Show ICB bounds','Show LSOA bounds','Show LAD bounds','Show ward bounds')),
            
            
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
              clearControls() %>%
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
  
  # Fit custom map to box
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
  
  #Marker Palette
  marker_pal <- reactive({
    
    req(input$UPLOAD_MARKERS$datapath)
    
    {
      tryCatch({
        return(
          if(input$MARKER_PALETTE == 'Numeric'){
            marker_pal <- leaflet::colorNumeric(
              palette = c(input$MARKER_PRIMARY_COLOUR,input$MARKER_SECONDARY_COLOUR),
              domain=marker_data()$value,
              na.color=NA)
            return(marker_pal)
          } else if(input$MARKER_PALETTE == 'String') {
            marker_pal <- leaflet::colorFactor(
              palette = viridis_pal()(20),
              domain= marker_data()$value,
              na.color=NA)
            return(marker_pal)
          } else if(input$MARKER_PALETTE == 'Custom') {
            marker_pal <- leaflet::colorFactor(
              #TEST THIS OUT
              palette = as.character(marker_data()$value),
              domain = as.character(marker_data()$value),
              ordered = T)
            return(marker_pal)} else
            {NULL}
        )},
        error = function(e) {
          message('There is something wrong with the markers file you uploaded.', e)
          sendSweetAlert(
            session = session,
            title = 'Error',
            text="There is something wrong with the marker file you uploaded. MORT cannot coerce types (eg: 'NAME' is a string and cannot be turned into numeric) please adjust the file or the palette type accordingly",
            type = 'error')
          return(NULL)
        })
    }
  }) %>%
    bindEvent(input$GENERATE)
  
  #OUTPUT 1: LOOKUP DOWNLOAD
  output$downloadSample <- downloadHandler(
    filename = 'sample.csv',
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(sample, file,row.names = F)
    })
}

shinyApp(ui=ui,server=server)

