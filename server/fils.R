fil <- function(){

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

}