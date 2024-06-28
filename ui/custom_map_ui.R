custom_map_ui <- function(){
  htmltools::tagList(
    sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;", 
      titlePanel(h4(strong('CUSTOM MAP'),icon('map',class='about-icon fa-pull-left'))),
      br(),
      radioGroupButtons(
        inputId = "geo_type",
        label = "Select map type",
        choices = c("ggplot2",'leaflet'),
        selected = 'leaflet',
        justified = TRUE),
      hr(),
      h4(strong('Upload Data'),icon('upload',class='about-icon fa-pull-left')),
      br(),
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
        icon = icon('compass'),
        status = 'primary',
        width = '100%'),
      hr(),
      downloadButton("downloadSample",
                     "Download Sample Files",
                     style = "width:100%;"),
      hr(),
      shiny::actionButton(
        inputId = 'GENERATE',
        label = 'Generate Map',
        width = '100%',
        icon = icon('play')),
      hr(),
      h4(strong('Advanced Options'),icon('cog',class='about-icon fa-pull-left')),
      fluidRow(column(6,
                      h5(strong('MARKERS')),
                      shinyWidgets::pickerInput(
                        inputId='MARKER_PALETTE',
                        label = 'Select Marker Palette',
                        choices = OP_PAL_MARKER),
                      numericInput(
                        inputId='RADIUS',
                        label = 'Marker Radius',
                        value = 30,
                        min = 1,
                        max = 2500,
                        step = 10,
                        width = '120%'),
                      colourpicker::colourInput(
                        'MARKER_PRIMARY_COLOUR',
                        'Marker Colour',
                        '#26a5b8'),
                      colourpicker::colourInput(
                        'MARKER_MID_COLOUR',
                        'Polygon Colour',
                        'white'),
                      colourpicker::colourInput(
                        'MARKER_SECONDARY_COLOUR',
                        'Marker Secondary Colour',
                        '#DD0075')),
               column(6,
                      h5(strong('POLYGONS')),
                      shinyWidgets::pickerInput(
                        inputId='PALETTE',
                        label = 'Select Polygon Palette',
                        choices = OP_PAL),
                      bsTooltip("PALETTE", "Make sure the value you have for your geography matches the type: so numbers only work with numeric and categorics, custom means it reads in hex codes or colors (eg: red, blue) and string just reads in text and assigns it a colour",
                                "right", options = list(container = "body")),
                      shiny::numericInput(
                        inputId = 'BINS',
                        label = 'Select number of bins',
                        value = 5,
                        min = 0,
                        max = 10,
                        step = 1),
                      colourpicker::colourInput(
                        'PRIMARY_COLOUR',
                        'Polygon Colour',
                        '#26a5b8'),
                      colourpicker::colourInput(
                        'MID_COLOUR',
                        'Polygon Colour',
                        'white'),
                      colourpicker::colourInput(
                        'SECONDARY_COLOUR',
                        'Polygon Secondary Colour',
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
      radioGroupButtons(
        inputId = "COL_NUM",
        label = "Select number of colours for legend",
        choices = c('Two colours','Three colours'),
        selected = 'Three colours',
        justified = TRUE),
      prettySwitch(
        inputId='FORCE_MAX',
        label = 'Add custom range?',
        width = '100%',
        status = 'default',
        fill = T),
      numericRangeInput(
        inputId='DOMAIN_RANGE',
        label = 'Select palette range',
        value = 10
      ),
      prettySwitch(
        inputId='MARKER_SIZE_FLAG',
        label = 'Variable Marker Size?',
        width = '100%',
        status = 'default',
        fill = T),
      bsTooltip("MARKER_SIZE_FLAG", "If you tick this and the marker has a numeric value, it will make the radius of the circle proportional to the value",
                "right", options = list(container = "body"))
  ),
  mainPanel(leafletOutput('main_map',height='85vh',width='100%'))))
}