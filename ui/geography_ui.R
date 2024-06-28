geography_ui <- function(){
  tagList(
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
      mainPanel(tableOutput('main_table'),style='border: 0px'))
  )
}