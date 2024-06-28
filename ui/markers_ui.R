markers_ui <- function(){
  htmltools::tagList(
    sidebarLayout(
      sidebarPanel(
        titlePanel(h4(strong('MARKER LOOKUPS'),icon('map-marker',class='about-icon fa-pull-left'))),
        em('You can use this to identify the lookups for all hospital sites in the country. Future locations can be added as needed'),
        hr(),
        awesomeCheckboxGroup(
          inputId = "SELECT_COLUMNS_MARKERS",
          label = "Select Columns", 
          choices = OP_COLS_MARKERS,
          selected = OP_COLS_MARKERS,
          inline = TRUE, 
          status = "danger"),
        hr(),
        downloadButton("downloadDataMarkers",
                       "Download",
                       style = "width:100%;")),
      mainPanel(tableOutput('main_table_markers'),style='border: 0px'))
  )
}